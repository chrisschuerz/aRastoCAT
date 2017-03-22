#' Aggregate climate raster data for catchment subbasins
#'
#' @param ncdf_pth
#' @param basin_shp
#' @param ncdf_crs
#' @param shp_index
#'
#' @return
#' @export

aggregate_ncdf <- function(ncdf_pth, basin_shp, ncdf_crs, shp_index) {

  # Load NCDF file ------------------------------------------------------
  ncin <- nc_open(filename = ncdf_pth)

  # Read variable and date from ncdf
  tmp_array <- ncvar_get(ncin,"pr")
  time <- ncvar_get(ncin,"time")

  # Get initial time step
  t_0 <- ncatt_get(ncin,"time","units")$value %>%
    gsub("days since ", "", .) %>%
    as.Date()

  # Load lat/lon as raster and create polygon index layer from it -------
  lat <- raster(ncdf_files[1], varname="lat")
  lon <- raster(ncdf_files[1], varname="lon")
  rst_dim <- dim(lon) #x = horiz = lon / y = vert = lat
  rst_len <- length(lon)

  # Convert to points and match the lat and lons
  plat <- rasterToPoints(lat)
  plon <- rasterToPoints(lon)
  lonlat <- cbind(plon[,3], plat[,3])

  # Specify the lonlat as spatial points with projection as long/lat
  lonlat <- SpatialPointsDataFrame(coords = lonlat, proj4string = CRS(ncdf_crs), data = data.frame(value = 1:length(lon)))
  lonlat <- SpatialPoints(coords = lonlat, proj4string = CRS(ncdf_crs))
  lonlat_proj <- spTransform(lonlat, CRSobj = crs(basin_shp))

  # Derive lat/lon cell size of projected raster
  cell_size <- c(lonlat_proj@coords[1:rst_dim[2],1] %>% diff() %>% mean(),
                 subtract(lonlat_proj@coords[seq(1, rst_len - rst_dim[2], rst_dim[2]), 2],
                          lonlat_proj@coords[seq(1 + rst_dim[2], rst_len, rst_dim[2]), 2]) %>%
                   mean()
  )

  # Create raster with cell indices as values
  idx_rst <- matrix(data = 1:rst_len, nrow = rst_dim[2]) %>%
    t() %>%
    apply(., 2, rev) %>%
    raster()

  # Assign extent of point layer but adding cell extent
  ext <- extent(lonlat_proj)
  ext@xmin <- ext@xmin - cell_size[2]/2
  ext@xmax <- ext@xmax + cell_size[2]/2
  ext@ymin <- ext@ymin - cell_size[1]/2
  ext@ymax <- ext@ymax + cell_size[1]/2 # be careful here maybe other way round!!!
  extent(idx_rst) <- ext

  # assign crs of catchment boundary
  crs(idx_rst) <- crs(basin_shp)

  # Convert index raster to spatial polygon
  idx_poly <- as(idx_rst, "SpatialPolygonsDataFrame")

  # Intersect index polygon with subbasin boundaries
  int_poly <- raster::intersect(idx_poly, basin_shp)

  # Extract data.frame with indices, subbasin number and pixel areas
  idx_area <- data.frame(area = sapply(int_poly@polygons, FUN=function(x) {slot(x, 'area')})) %>%
    cbind(int_poly@data) %>%
    mutate(area_fract = area/(cell_size[1]*cell_size[2])) %>%
    dplyr::select(matches(shp_index), layer, area_fract) %>%
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
    add_column(date = t_0 + time, .before = 1)

  return(idx_area)
}
