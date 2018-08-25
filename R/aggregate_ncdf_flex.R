#' Aggregate NCDF Climate (Raster) Data for Catchment Subbasins
#'
#' @param ncdf_file Path to the ncdf file
#' @param crs_ncdf Current reference system of ncdf file
#' @param shape_file Shape file with the basin sub-unit polygons or path to the
#'   file
#' @param shape_index Name of the column in the basin shapefile attribute table
#'   that provides the indices of the basin subunits
#' @param var_label Name of the variable array to be extracted from the ncdf
#'   file
#' @param lat_label Name of the latitude matrix in the ncdf file
#' @param lon_label Name of the longitude matrix  in the ncdf file
#' @param time_label Name of the time vector in the ncdf file
#'
#' @importFrom dplyr funs group_by left_join mutate mutate_at rename select
#'   starts_with summarise_all ungroup vars
#' @importFrom lubridate day hour minute month year
#' @importFrom magrittr %>% %<>%  set_colnames
#' @importFrom ncdf4 nc_close nc_open ncatt_get ncvar_get
#' @importFrom purrr array_branch map
#' @importFrom sf read_sf st_area st_as_sf st_bbox st_crs st_intersection
#'   st_polygon st_set_agr st_sf st_sfc st_transform
#' @importFrom tibble add_column as_tibble
#'
#' @return Returns a tibble that provides the time series of the aggregated
#'   variable for the respective basin subunits
#' @export

aggregate_ncdf <- function(ncdf_file, crs_ncdf, shape_file, shape_index,
                           var_name = NULL, latlon_name = NULL,
                           time_range = NULL) {

  # Load shape file if path to the file is provided, else Convert shape file to
  # a simple feature object
  if(is.character(shape_file)){
    shape_file <-  read_sf(shape_file, quiet = TRUE)
  }
  ## Convert shape file to a simple feature object
  shape_file <-  st_as_sf(shape_file)
  ## Transform the shape file to the same reference system as the ncdf
  shape_trans <- st_transform(shape_file, crs = crs_ncdf)
  ## extract the extent (boundary box) of the transformed shape file
  bbox_trans <- st_bbox(shape_trans)

#-------------------------------------------------------------------------------
  # Extract the lat lon matrices and the time vector and reduce their extents
  # according to the shape file boundary and the provided time_range. Get the
  # row/col indices of the trimmed matrices and the time vector for reducing the
  # extent of the data array in the following reading step
  ## Open the NCDF file located from the path ncdf_file
  nc_file <- nc_open(filename = ncdf_file)

  ## Fetch the lat lon variables. lat and lon are always returned as matrices
  ## with the x/y dimensions of the data.
  lat_lon <- fetch_latlon(nc_file, latlon_name)
  ## Reduce the extent of the lat lon matrices according to the boundary of the
  ## shape file
  lat_lon_trim <- trim_latlon(lat_lon[[1]], lat_lon[[2]], bbox_trans)
  ## Get the indices of the trimmed matrices in the original matrices for
  ## reducing the extent of the data array to read
  lat_lon_index <- get_latlonindex(lat_lon, lat_lon_trim)

  ## Fetch the time variable.
  time <- fetch_time(nc_file)
  ## Limit time period to provided time_range
  time <- trim_time(time, time_range)

#-------------------------------------------------------------------------------
  # Fetch the array for the variable of interest and convert to list of matrices
  var_data <- fetch_var(nc_file, var_name, lat_lon_index, time[[2]])

#-------------------------------------------------------------------------------
  # Close the connection to the NCDF file after aqcuiring all requiered data.
  nc_close(nc_file)

#-------------------------------------------------------------------------------

  var_grid <- create_polygon_grid(var_data, lat_lon_trim, shape_file, crs_ncdf)

#-----------------------------------------------------------------------------
  # Intersect the shape file with the genereated polygon grid and calculate area
  # weighted averages of each times step in the data for the individual subunits
  # in the shape file

  grid_intersect <- var_grid %>%
    st_intersection(st_set_agr(shape_file, "constant"), .) %>%
    as_tibble(.) %>%
    mutate(area = st_area(geometry) %>% as.numeric(.))  # Calculate the area of each itersection

  data_aggregate <- tibble(index = grid_intersect[[shape_index]],
                           idx         = grid_intersect$idx,
                           area_fract  = grid_intersect$area) %>%
    group_by(index) %>%
    mutate(area_fract = area_fract/sum(area_fract)) %>% # Calculate fractions of areas
    left_join(., var_data, by = "idx") %>% # Jion with variable data
    multiply_by_fraction(.) %>%
    summarise_all(funs(sum)) %>% # Sum up the fractions for all shape sub units
    ungroup(.) %>%
    mutate(., index = shape_index%_%index) %>%
    transpose_tbl(., name_col = "index") %>%
    add_column(., date = time$time, .before = 1)


    return(data_aggregate)
}

multiply_by_fraction <- function(tbl) {
  data <- tbl %>% ungroup() %>% select(starts_with("timestep"))
  fract <- tbl$area_fract
  data %>%
    map_dfc(., ~.x*fract) %>%
    bind_cols(tbl %>% select(index),.) %>%
    group_by(index)
}

transpose_tbl <- function(tbl, name_col) {
  col_names <- tbl[[name_col]]
  tbl <- tbl %>%
    select(-matches(name_col)) %>%
    t(.) %>%
    as_tibble(.) %>%
    set_names(., col_names)
  return(tbl)
}
