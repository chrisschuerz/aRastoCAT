#' Aggregate NCDF Climate (Raster) Data for Catchment Subbasins
#'
#' @param ncdf_file Path to the ncdf file
#' @param crs_ncdf Current reference system of ncdf file
#' @param shape_file Shape file with the basin sub-unit polygons
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
#' @importFrom sf st_area st_bbox st_intersection st_polygon st_set_agr st_sf
#'   st_sfc st_transform
#' @importFrom tibble add_column as_tibble
#'
#' @return Returns a tibble that provides the time series of the aggregated
#'   variable for the respective basin subunits
#' @export

aggregate_ncdf <- function(ncdf_file, crs_ncdf, shape_file, shape_index,
                           var_label, lat_label = "lat", lon_label = "lon",
                           time_label = "time") {

  #-----------------------------------------------------------------------------
  # Extract and modify the variable array, the lat/lon matrices and the time
  # vector from the NCDF file
  ## Open the NCDF file located in the ncdf_file
  nc_file <- nc_open(filename = ncdf_file)

  ## Function to rotate a matrix 90° counter clockwise
  rotate_cc <- function(mtr) { mtr %>% t(.) %>% apply(., 2, rev)}

  ## Read the matrices providing the latitude and longitude for the data points
  ## and rotate them by 90° clockwise
  lat <- ncvar_get(nc_file,lat_label) %>%
    rotate_cc(.)

  lon <- ncvar_get(nc_file,lon_label) %>%
    rotate_cc(.)

  ## Read the array for the variable holding the data for each lat/lon point and
  ## time step. Rotate it as done with lat/lon and save all matrices for the
  ## individual timesteps in a list
  var_data <- ncvar_get(nc_file,var_label) %>%
    array_branch(., margin = 3) %>%
    map(.,  rotate_cc)

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
  limit_lon <- function(lon, bbox){
    lon_lf <- which(colSums(lon < bbox[1]) == nrow(lon)) %>% .[length(.)]
    lon_rg <- which(colSums(lon > bbox[3]) == nrow(lon)) %>% .[1]
    lon_lf:lon_rg
  }

  ## Function to find the indices of the latitude matrix that covers c(ymin,
  ## ymax) of the shape file extent
  limit_lat <- function(lat, bbox){
    lat_lw <- which(rowSums(lat < bbox[2]) == ncol(lat)) %>% .[1]
    lat_up <- which(rowSums(lat > bbox[4]) == ncol(lat)) %>% .[length(.)]
    lat_up:lat_lw
  }

  ## Transform the shape file to the same reference system as the ncdf
  shape_trans <- st_transform(basin_shp, crs = crs_ncdf)
  ## extract the extent (boundary box) of the transformed shape file
  bbox_trans <- st_bbox(shape_trans)

  ## If the lat/lon system represented in the lat/lon matrices is curved the
  ## limitation of the extent to the one of the shape file is solved
  ## iteratively
  ## Save the intitial dimensions of the lat/lon matrices for later checkup
  dim_init <- dim(lat)

  ## Set initial values for iterative step
  iter_check <- TRUE
  ind_lat_prev <- 0
  ind_lon_prev <- 0

  ## Iterate over the indices of the lat/lon matrices until the final dimensions
  ## of the matrices do not change anymore.
  while(iter_check){
    ### Find the indices of along lat and long that fully cover the shape file
    ### extent.
    ind_lat <- limit_lat(lat, bbox_trans)
    ind_lon <- limit_lon(lon, bbox_trans)

    ### Check for the first run if shape file is inside the exntent of the
    ### matrices
    if(all(dim(lat) == dim(dim_init))){
      if(!all(ind_lat %in% (1:dim_init[1])) &
         !all(ind_lon %in% (1:dim_init[2]))){
        stop("Basin shape too close to any of the grid boundaries!")
      }
    }

    ### Trim the lat/lon matrices
    lat <- lat[ind_lat, ind_lon]
    lon <- lon[ind_lat, ind_lon]
    var_data <- map(var_data, function(mtr){mtr[ind_lat, ind_lon]})

    ### Check if the dimensions are stable
    iter_check <-  ((sum(ind_lat) != sum(ind_lat_prev)) |
                      (sum(ind_lon) != sum(ind_lon_prev)))
    ind_lat_prev <- ind_lat
    ind_lon_prev <- ind_lon
  }

  ## Trim the data array to the same extent lik the lat/lon matrices and create
  ## a tibble where each column is one time step. Add the indices of the
  ## remaining pixels for later data extraction.
  var_data %<>%
    map(., as.vector) %>%
    do.call(cbind, .) %>%
    as_tibble() %>%
    set_colnames("timestep"%_%1:ncol(.)) %>%
    add_column(., idx = 1:nrow(.), .before = 1)

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
    st_sfc(., crs = crs_grid) %>%
    st_sf(idx = 1:nrow(var_data), geometry = .)

  #-----------------------------------------------------------------------------
  # Intersect the shape file with the genereated polygon grid and calculate area
  # weighted averages of each times step in the data for the individual subunits
  # in the shape file

  data_aggr <- var_grid %>%
    st_set_agr(., "constant") %>% #Assumption of constant attributes to avoid warnings
    st_intersection(st_set_agr(shape_trans, "constant"), .) %>% # Intersect the grid with the shape file
    as_tibble(.) %>%
    mutate(area_frct = st_area(geoms) %>% as.numeric(.)) %>% # Calculate the area of each itersection
    select(!!shp_index, idx, area_frct) %>% # Select The subunit index, grid index and area
    rename(shp_index = !!shp_index) %>% # Rename shape index
    group_by(shp_index) %>%
    mutate(area_frct = area_frct/sum(area_frct)) %>% # Calculate fractions of areas
    left_join(., var_data, by = "idx") %>% # Jion with variable data
    mutate_at(vars(starts_with("time")), funs(.*area_frct)) %>% # multiply all timesteps with area fraction
    select(-idx, -area_frct) %>%
    summarise_all(funs(sum)) %>% # Sum up the fractions for all shape sub units
    ungroup(.) %>%
    select(-shp_index) %>%
    t(.) %>% # Change format to column = Shape sub unit, row = time step
    as_tibble(.) %>%
    set_colnames(shp_index%_%1:ncol(.)) %>%
    add_column(year = year(t_0 + time), # Add date to the table
               mon  = month(t_0 + time),
               day  = day(t_0 + time),
               hour = hour(t_0 + time),
               min  = minute(t_0 + time),
               .before = 1)

    return(data_aggr)
}
