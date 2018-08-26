#' Trim the lat/lon matrices to the extent of the provided shape file
#'
#' @param lat Matrix holding the latitude values of each pixel
#' @param lon Matrix holding the longitude values of each pixel
#' @param bbox Boundary box of the shape file
#'
#' @return Returns a list of length 2 holding the trimmed lat and lon matrices.
#' @keywords internal
#'
trim_latlon <- function(lat, lon, bbox) {
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
    ind_lat <- limit_lat(lat, bbox)
    ind_lon <- limit_lon(lon, bbox)

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
    # var_data <- map(var_data, function(mtr){mtr[ind_lat, ind_lon]})

    ### Check if the dimensions are stable
    iter_check <-  ((sum(ind_lat) != sum(ind_lat_prev)) |
                      (sum(ind_lon) != sum(ind_lon_prev)))
    ind_lat_prev <- ind_lat
    ind_lon_prev <- ind_lon
  }
  return(list(lat = lat, lon = lon))
}

#' Helper function to find the indices of the longitude matrix that covers
#' c(xmin,xmax) of the shape file extent
#'
#' @param lon Matrix holding the longitude values of each pixel
#' @param bbox Boundary box of the shape file
#'
#' @return Returns the updated longitude indices to which the matrices will be
#'   reduced.
#' @keywords internal
#'
limit_lon <- function(lon, bbox){
  lon_lf <- which(colSums(lon < bbox[1]) == nrow(lon)) %>% .[length(.)]
  lon_rg <- which(colSums(lon > bbox[3]) == nrow(lon)) %>% .[1]
  lon_lf:lon_rg
}

#' Helper function to find the indices of the latitude matrix that covers
#' c(xmin,xmax) of the shape file extent
#'
#' @param lon Matrix holding the latitude values of each pixel
#' @param bbox Boundary box of the shape file
#'
#' @return Returns the updated latitude indices to which the matrices will be
#'   reduced.
#' @keywords internal
#'
limit_lat <- function(lat, bbox){
  lat_lw <- which(rowSums(lat < bbox[2]) == ncol(lat)) %>% .[1]
  lat_up <- which(rowSums(lat > bbox[4]) == ncol(lat)) %>% .[length(.)]
  lat_up:lat_lw
}

#' Get the indices in the original lat/lon matrices that cover the trimmed
#' matrices.
#'
#' @param lat_lon List holding the initial lat_lon matrices
#' @param lat_lon_trim List holding the trimmed lat_lon matrices
#'
#' @return Returns the updated longitude indices to which the matrices will be
#'   reduced.
#' @keywords internal
#'
get_latlonindex <- function(lat_lon, lat_lon_trim) {
  dim_trim <- dim(lat_lon_trim[[1]])[2:1]

  lat_lon_rot <- lat_lon %>%
    map(.,rotate_cc) %>%
    map(.,rotate_cc) %>%
    map(.,rotate_cc)
  start_ind <- which((lat_lon_rot[[1]] == lat_lon_trim[[1]][1,1]) &
                     (lat_lon_rot[[2]] == lat_lon_trim[[2]][1,1]),
                     arr.ind = TRUE)
  return(list(start = start_ind, count = dim_trim))
}

#' Trim the time vector provided by the ncdf file according to the provided
#' time_range.
#'
#' @param time Date vector derived from the ncdf file
#' @param time_range Vector of length 2 that either provides the date range as
#'   character strings or in a date format. If NULL the complete time vector is
#'   used.
#'
#' @importFrom lubridate as_date
#'
#' @return Returns a list that holds the trimmed date vector and the
#'   corresponding indices in the original vector that are further used for
#'   trimming the data set
#' @keywords internal
#'
trim_time <- function(time, time_range) {
  if(is.null(time_range)){
    time_ind <- list(start = 1, count = -1)
  } else {
    if(!is.null(time)) {
      time_range <- as_date(time_range)

      time_ind <- list(start = which(time >= time_range[1])[1])
      time_ind$count <- which(time >= time_range[2])[1] - time_ind$start + 1

      time <- time[time_ind$start:(time_ind$start + time_ind$count -1)]
    } else {
      warning(" Variable 'time_range' will be ignored.")
    }
  }
  return(list(time = time, time_index = time_ind))
}
