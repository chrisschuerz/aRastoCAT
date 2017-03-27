#' Aggregate NCDF Climate (Raster) Data for Catchment Subbasins
#'
#' @param ncdf_pth Path to the ncdf file
#' @param basin_shp Shape file with the basin sub-unit polygons
#' @param ncdf_crs Current reference system of ncdf file
#' @param shp_index Name of the column in the basin shapefile attribute
#'   table that provides the indices of the basin subunits
#'
#' @importFrom pasta %_%
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

aggregate_ncdf <- function(ncdf_pth, basin_shp, ncdf_crs, shp_index, var_lbl,
                           lat_lbl = "lat", lon_lbl = "lon", time_lbl = "time") {

  # Load NCDF file ------------------------------------------------------
  ncin <- nc_open(filename = ncdf_pth)

  # Read variable and date from ncdf
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
  ext@xmin <- ext@xmin - cell_size[2]/2
  ext@xmax <- ext@xmax + cell_size[2]/2
  ext@ymin <- ext@ymin - cell_size[1]/2
  ext@ymax <- ext@ymax + cell_size[1]/2 # be careful here maybe other way round!!!

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
  idx_area <- data.frame(area = sapply(int_poly@polygons, FUN=function(x) {slot(x, 'area')})) %>%
    cbind(int_poly@data) %>%
    mutate(area_fract = area/(cell_size[1]*cell_size[2])) %>%
    select(matches(shp_index), layer, area_fract) %>%
    set_colnames(c("basin", "idx", "fraction"))

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

  ws <- ls()
  ws <- ws[ws != "idx_area"]
  rm(list = ws)
  removeTmpFiles(h=0)
  gc()
  return(idx_area)
}
