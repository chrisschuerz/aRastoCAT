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

  ## Trim the data array to the same extent lik the lat/lon matrices and create
  ## a tibble where each column is one time step. Add the indices of the
  ## remaining pixels for later data extraction.
  var_data %<>%
    map(., as.vector) %>%
    do.call(cbind, .) %>%
    as_tibble() %>%
    set_colnames("timestep"%_%1:ncol(.)) %>%
    add_column(., idx = 1:nrow(.), .before = 1)

#-------------------------------------------------------------------------------
  # Close the connection to the NCDF file after aqcuiring all requiered data.
  nc_close(nc_file)

#-------------------------------------------------------------------------------

  # Create a polygon grid from the lat/lon matrices, that is used for
  # intersecting with the subunits of the provided shape file.
  ## Function to calculate the mid point between four lat/lon points to derive
  ## the corner points of the polygon grid cells.
  calc_corner <- function(ind, mtr) {
    mean(mtr[ind[1]:(ind[1] + 1), ind[2]:(ind[2] + 1)])}

  ## Function to extrapolate the corner points of the grid in longitude
  extrapol_col <- function(mtr){
    n_col <- ncol(mtr)
    cbind(mtr[ , 1] + (mtr[ , 1] - mtr[ , 2]),
          mtr,
          mtr[ , n_col] + (mtr[ , n_col] - mtr[, n_col - 1]))
  }

  ## Function to extrapolate the corner points of the grid in latitude
  extrapol_row <- function(mtr){
    n_row <- nrow(mtr)
    rbind(mtr[1, ] + (mtr[1, ] - mtr[2, ]),
          mtr,
          mtr[n_row, ] + (mtr[n_row, ] - mtr[n_row - 1, ]))
  }

  ## Function to derive a list of tables, where each table holds the coordinates
  ## that describe the path around a cell of the polygon grid.
  extract_poly_coord <- function(ind, lat, lon){
    cbind(c(lon[ind[1]    , ind[2]],
            lon[ind[1]    , ind[2] + 1],
            lon[ind[1] + 1, ind[2] + 1],
            lon[ind[1] + 1, ind[2]],
            lon[ind[1]    , ind[2]]),
          c(lat[ind[1]    , ind[2]],
            lat[ind[1]    , ind[2] + 1],
            lat[ind[1] + 1, ind[2] + 1],
            lat[ind[1] + 1, ind[2]],
            lat[ind[1]    , ind[2]]))
  }

  ## Derive the dimensions of the trimmed matrices
  rst_dim <- dim(lat)

  ## Create all combinations of x/y of the matrice indices
  rst_ind <- expand.grid(1:(rst_dim[1]), 1:(rst_dim[2]))

  ## Create a reduced set of index combinations required for the corner point
  ## calculation
  pnt_ind <- expand.grid(1:(rst_dim[1] - 1), 1:(rst_dim[2] - 1))

  ## Calculate the longitude values of all corner points and extrapolate the
  ## values at the outer rows and columns
  lon_corner <- apply(pnt_ind, 1, calc_corner, lon) %>%
    matrix(., nrow = rst_dim[1] - 1, ncol = rst_dim[2] - 1) %>%
    extrapol_row(.) %>%
    extrapol_col(.)

  ## Calculate the latitude values of all corner points and extrapolate the
  ## values at the outer rows and columns
  lat_corner <- apply(pnt_ind, 1, calc_corner, lat) %>%
    matrix(., nrow = rst_dim[1] - 1, ncol = rst_dim[2] - 1) %>%
    extrapol_row(.) %>%
    extrapol_col(.)

  ## Convert the index combinations of the matrices into a list for application
  ## in the polygon grid definition
  ind_list <- as.list(as.data.frame(t(rst_ind)))

  ## Generate the polygon grid as a simple feature object and add the cell
  ## indices as in the variable data table
  var_grid <- map(ind_list, extract_poly_coord, lat_corner, lon_corner) %>%
    map(., function(poly_i){st_polygon(x = list(poly_i), dim = "XY")}) %>%
    st_sfc(., crs = crs_ncdf) %>%
    st_sf(idx = 1:nrow(var_data), geometry = .) %>%
    st_transform(., st_crs(shape_file)) %>%
    st_set_agr(., "constant") #Assumption of constant attributes to avoid warnings

  #-----------------------------------------------------------------------------
  # Intersect the shape file with the genereated polygon grid and calculate area
  # weighted averages of each times step in the data for the individual subunits
  # in the shape file

  grid_intersect <- var_grid %>%
    st_intersection(st_set_agr(shape_file, "constant"), .) %>%
    as_tibble(.) %>%
    mutate(area = st_area(geometry) %>% as.numeric(.))  # Calculate the area of each itersection

  data_aggregate <- tibble(shape_index = grid_intersect[[shape_index]],
                           idx         = grid_intersect$idx,
                           area_fract  = grid_intersect$area) %>%
    group_by(shape_index) %>%
    mutate(area_fract = area_fract/sum(area_fract)) %>% # Calculate fractions of areas
    left_join(., var_data, by = "idx") %>% # Jion with variable data
    mutate_at(vars(starts_with("time")), funs(.*area_fract)) %>%  # multiply all timesteps with area fraction
    select(-idx, -area_fract) %>%
    summarise_all(funs(sum)) %>% # Sum up the fractions for all shape sub units
    ungroup(.) %>%
    select(-shape_index) %>%
    t(.) %>% # Change format to column = Shape sub unit, row = time step
    as_tibble(.) %>%
    set_colnames(shape_index%_%1:ncol(.)) %>%
    add_column(year = year(t_0 + time), # Add date to the table
               mon  = month(t_0 + time),
               day  = day(t_0 + time),
               hour = hour(t_0 + time),
               min  = minute(t_0 + time),
               .before = 1)

    return(data_aggregate)
}
