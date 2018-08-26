#' Aggregate ncdf climate (raster) data for catchment subbasins
#'
#' @param ncdf_file Path to the ncdf file
#' @param crs_ncdf Character string providing the reference system of ncdf file
#' @param shape_file Shape file with the basin sub-unit polygons or path to the
#'   file as character string
#' @param shape_index Name of the column in the basin shapefile attribute table
#'   that provides the indices of the basin subunits
#' @param var_name (optional) character string. Name of the variable array to be
#'   extracted from the ncdf file. If \code{NULL} the first variable provided in
#'   the ncdf file is selected automatically
#' @param latlon_name (optional) character vector of \code{length 2}. Names of
#'   the longitude and latitude variables in the ncdf file. Must be provided if
#'   the variables connot be found automatically
#' @param time_label (optional) vector of \code{length 2}. Start and end date of
#'   the time range that should be extracted from the ncdf file. Dates can be
#'   either provided as unambiguous text strings (e.g. \code{'yyyy-mm-dd'} or in
#'   a date format
#'
#' @importFrom ncdf4 nc_close nc_open
#' @importFrom sf read_sf st_as_sf st_transform st_bbox
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

  var_table <- aggregate_variable(var_grid, var_data, shape_file, shape_index,
                             time[[1]])

  return(var_table)
}


