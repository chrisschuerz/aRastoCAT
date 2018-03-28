#' Aggregate NCDF Climate (Raster) Data for Catchment Subbasins
#'
#' @param ncdf_pth Path to the ncdf file
#' @param basin_shp Shape file with the basin sub-unit polygons
#' @param ncdf_crs Current reference system of ncdf file
#' @param shp_index Name of the column in the basin shapefile attribute
#'   table that provides the indices of the basin subunits
#'
#' @importFrom  pasta %_%
#' @importFrom dplyr mutate mutate_at select matches starts_with left_join
#'   vars funs group_by summarise_all
#' @importFrom tibble as_tibble add_column
#' @import     lubridate
#' @importFrom magrittr %>% set_colnames subtract
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom raster raster rasterToPoints extent crs intersect
#'   removeTmpFiles
#' @importFrom sp SpatialPoints SpatialPointsDataFrame spTransform
#'   SpatialPolygonsDataFrame
#'
#' @return Returns a tibble that provides the time series
#'   of the aggregated variable for the respective basin subunits
#' @export

aggregate_ncdf <- function(ncdf_path, crs_ncdf, shape_file, shape_index, var_label,
                           lat_label = "lat", lon_label = "lon", time_label = "time") {

  #-----------------------------------------------------------------------------
  # Get the extent of the shape file in the reference system of the ncdf
  ## Transform the shape file to the same reference system as the ncdf
  shape_trans <- st_transform(basin_shp, crs = crs_ncdf)
  ## extract the extent of the transformed shape file
  ext_trans <- extent(shape_trans)

  #-----------------------------------------------------------------------------
  # Extract and modify the variable array, the lat/lon matrices and the time
  # vector from the NCDF file
  ## Open the NCDF file located in the ncdf_path
  nc_file <- nc_open(filename = ncdf_path)

  ## Read the matrices providing the latitude and longitude for the data points
  ## and rotate them by 90° clockwise
  lat <- ncvar_get(nc_file,lat_label) %>%
    t() %>%
    apply(., 2, rev)

  lon <- ncvar_get(nc_file,lon_label) %>%
    t() %>%
    apply(., 2, rev)

  ## Read the array for the variable holding the data for each lat/lon point and
  ## time step. Rotate it as done with lat/lon and save all matrices for the
  ## individual timesteps in a list
  var_data <- ncvar_get(nc_file,var_label) %>%
    array_branch(., margin = 3) %>%
    map(.,  function(array){array %>% t(.) %>% apply(., 2, rev)})

  ## Read the time series vector from the NCDF file
  time <- ncvar_get(nc_file, time_label)

  ## The NCDF must contain an initial time step. Read the initial time step from
  ## the NCDF
  t_0 <- ncatt_get(nc_file, time_label,"units")$value %>%
    gsub("days since |seconds since ", "", .) %>%
    as.Date()

  ## Close the connection to the NCDF file after aqcuiring all requiered data.
  nc_close(nc_file)

  #-----------------------------------------------------------------------------
  # Reduce the extent of the provided NCDF data set to the extent of the provided
  # shape file after transforming it to the reference system of the NCDF
  ## Function to find the indices of the longitude matrix that covers c(xmin,
  ## xmax) of the shape file extent
  limit_lon <- function(lon, ext){
    lon_lf <- which(colSums(lon < ext[1]) == nrow(lon)) %>% .[length(.)]
    lon_rg <- which(colSums(lon > ext[2]) == nrow(lon)) %>% .[1]
    lon_lf:lon_rg
  }

  ## Function to find the indices of the latitude matrix that covers c(ymin,
  ## ymax) of the shape file extent
  limit_lat <- function(lat, ext){
    lat_lw <- which(rowSums(lat < ext[3]) == ncol(lat)) %>% .[1]
    lat_up <- which(rowSums(lat > ext[4]) == ncol(lat)) %>% .[length(.)]
    lat_up:lat_lw
  }



  ## Transform the shape file to the same reference system as the ncdf
  shape_trans <- st_transform(basin_shp, crs = crs_ncdf)
  ## extract the extent of the transformed shape file
  ext_trans <- extent(shape_trans)










  tmp_array <- ncvar_get(ncin,var_lbl)
  time <- ncvar_get(ncin, time_lbl)

  # Get initial time step
  t_0 <- ncatt_get(ncin,time_lbl,"units")$value %>%
    gsub("days since |seconds since ", "", .) %>%
    as.Date()

  nc_close(ncin)

  # Load lat/lon as raster and create polygon index layer from it -------
  lat <- raster(ncdf_pth, varname = lat_lbl)
  lon <- raster(ncdf_pth, varname = lon_lbl)
  rst_dim <- dim(lon) #x = horiz = lon / y = vert = lat
  rst_len <- length(lon)

  # Convert to points and match the lat and lons
  plat <- rasterToPoints(lat)
  plon <- rasterToPoints(lon)
  lonlat <- cbind(plon[,3], plat[,3])

  # Specify the lonlat as spatial points with projection as long/lat
  lonlat_proj <- SpatialPointsDataFrame(coords = lonlat,
                                        proj4string = CRS(ncdf_crs),
                                        data = data.frame(value = 1:length(lon))) %>%
  SpatialPoints(coords = ., proj4string = CRS(ncdf_crs)) %>%
  spTransform(., CRSobj = crs(basin_shp))

  # Derive lat/lon cell size of projected raster
  cell_size <- c(lonlat_proj@coords[1:rst_dim[2],1] %>% diff() %>% mean(),
                 subtract(lonlat_proj@coords[seq(1, rst_len - rst_dim[2], rst_dim[2]), 2],
                          lonlat_proj@coords[seq(1 + rst_dim[2], rst_len, rst_dim[2]), 2]) %>%
                   mean()
  )


  # Assign extent of point layer but adding cell extent
  ext <- extent(lonlat_proj)
  ext@xmin <- ext@xmin - cell_size[1]/2
  ext@xmax <- ext@xmax + cell_size[1]/2
  ext@ymin <- ext@ymin - cell_size[2]/2
  ext@ymax <- ext@ymax + cell_size[2]/2 # be careful here maybe other way round!!!

  # Create raster with cell indices as values and convert to spatial polygon
  assign_crs <- function(sp_obj, crs_new) {
    crs(sp_obj) <- crs_new
    return(sp_obj)
  }
  assign_ext <- function(sp_obj, ext_new) {
    extent(sp_obj) <- ext_new
    return(sp_obj)
  }

  idx_poly <- matrix(data = 1:rst_len, nrow = rst_dim[2]) %>%
    t() %>%
    apply(., 2, rev) %>%
    raster() %>%
    assign_crs(., crs(basin_shp)) %>%
    assign_ext(., ext) %>%
    as(., "SpatialPolygonsDataFrame")

  # Intersect index polygon with subbasin boundaries
  int_poly <- intersect(idx_poly, basin_shp)

  # Extract data.frame with indices, subbasin number and pixel areas
  idx_area <- data.frame(area = sapply(int_poly@polygons,
                                       FUN=function(x) {slot(x, 'area')})) %>%
    cbind(int_poly@data) %>%
    mutate(area_fract = area/(cell_size[1]*cell_size[2])) %>%
    select(matches(shp_index), layer, area_fract) %>%
    set_colnames(c("basin", "idx", "fraction")) %>%
    mutate(idx = as.integer(idx)) %>%
    group_by(basin, idx) %>%
    summarise_all(funs(sum)) %>%
    ungroup()

  # Reduce 3D array to 2D matrix with row = idx, col = date
  tmp_df <- matrix(data = tmp_array, ncol = dim(tmp_array)[3]) %>%
    as.data.frame() %>%
    set_colnames("time"%_%1:dim(tmp_array)[3]) %>%
    mutate(idx = 1: rst_len)

  # Aggregate variable for subbasins
  idx_area <- left_join(idx_area, tmp_df, by = "idx") %>%
    mutate_at(vars(starts_with("time")), funs(.*fraction)) %>%
    select(-idx) %>%
    group_by(basin) %>%
    summarise_all(funs(sum)) %>%
    mutate_at(vars(starts_with("time")), funs(./fraction)) %>%
    select(-basin, -fraction) %>%
    t() %>%
    as_tibble() %>%
    set_colnames(shp_index%_%1:ncol(.)) %>%
    add_column(year = year(t_0 + time),
               mon  = month(t_0 + time),
               day  = day(t_0 + time),
               hour = hour(t_0 + time),
               min  = minute(t_0 + time),
               .before = 1)

  return(idx_area)
}
