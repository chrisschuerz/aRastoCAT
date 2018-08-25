#' Fetch the laitude and longitude variables and convert to matrices
#'
#' @param nc_file Opened ncdf file
#' @param latlon_name Character vector of length 2. Names of latitude longitude
#'   variables (if provided)
#'
#' @importFrom ncdf4 ncvar_get
#'
#' @return Returns a list holding latitude and longitude matrices
#' @keywords internal
fetch_latlon <- function(nc_file, latlon_name) {
  if(is.null(latlon_name)){
    latlon_name <- find_latlon(nc_file)
    if(is.null(latlon_name)){
      dim_names <- names(nc_file$dim)
      x_name <- dim_names[tolower(substr(dim_names, 1, 3)) %in% c("x", "lon")]
      y_name <- dim_names[tolower(substr(dim_names, 1, 3)) %in% c("y", "lat")]
      if(length(x_name) != 1){
        stop("Finding 'x' dimension was not successful")
      }
      if(length(y_name) != 1){
        stop("Finding 'y' dimension was not successful")
      }
      latlon_name <- c(y_name, x_name)
    }
  }
  lat <- ncvar_get(nc_file, latlon_name[1])
  lon <- ncvar_get(nc_file, latlon_name[2])

  if(length(dim(lat)) == 1){
    dim_lat <- length(lat)
    dim_lon <- length(lon)
    lat <- matrix(rep(sort(lat, decreasing = TRUE), dim_lon),
                  nrow = dim_lat,
                  ncol = dim_lon)
    lon <- matrix(rep(sort(lon), dim_lat),
                  nrow = dim_lon,
                  ncol = dim_lat) %>%
      rotate_cc(.)
  } else {
    lat <- rotate_cc(lat)
    lon <- rotate_cc(lon)
  }

  return(list(lat = lat, lon = lon))
}

#' Automatically find latitude and longitude variables
#'
#' @param nc_file Opened ncdf file
#'
#' @importFrom purrr map
#'
#' @return Returns a vector of length 2 givong the names of the lat/lon variables
#' @keywords internal
find_latlon <- function(nc_file) {
  var_name <- names(nc_file$var)
  long_name <- map(nc_file$var, ~.x$longname)
  var_name_lat <- var_name[grepl(pattern = "latitude", tolower(long_name))]
  var_name_lon <- var_name[grepl(pattern = "longitude", tolower(long_name))]
  if(xor(length(var_name_lat) == 0, length(var_name_lon) == 0)) {
    stop("Only one of the two variables 'lat'/'lon' identified. Please check manually!")
  }
  if(length(var_name_lat) == 0) {
    return(NULL)
    warning("No 'lat'/'lon' variables identified. Using 'x' and 'y' dimensions instead."%&%
              "If 'lat'/'lon' variables should exist, define them manually!")
  } else {
    return(c(var_name_lat, var_name_lon))
  }
}

#' Fetch the date vector from the ncdf file if provided
#'
#' @param nc_file Opened ncdf file
#'
#' @importFrom dplyr %>%
#' @importFrom lubridate as_date duration
#' @importFrom ncdf ncatt_get ncvar_get
#'
#' @return Returns a vector of length 2 givong the names of the lat/lon variables
#' @keywords internal
#'
fetch_time <- function(nc_file) {
  if("time" %in% names(nc_file$dim)){
    ## Read the time intervals vector from the ncdf file
    time <- ncvar_get(nc_file, "time")

    ## Read the attributes of the time vector
    time_attr <- ncatt_get(nc_file, "time", "units")$value

    if(time_attr != 0) {
      time_init <- time_attr %>%
        gsub("days since |seconds since ", "", .) %>%
        as_date(.)

      time_inter <- time_attr %>%
        strsplit(., " ") %>%
        unlist(.) %>%
        .[1]

      time_span <- time_init + duration(num = time, units = time_inter)
    } else {
      time_span <- time
      warning("Dimension 'time' has no attributes. Time returned as numeric vector!")
    }
  } else {
    time_span <- NULL
    warning("No 'time' dimension found. Results are returned without a date")
  }
  return(time_span)
}

## Read the array for the variable holding the data for each lat/lon point and
## time step. Rotate it as done with lat/lon and save all matrices for the
## individual timesteps in a list
fetch_var <- function(nc_file, var_name, lat_lon_ind, time_ind) {
  ## If no variable name is provided, the first variable is selected
  if(is.null(var_name)) var_name <- names(nc_file$var)[1]
  n_dim <- nc_file$var[[var_name]]$ndims
  if(n_dim == 3){
    start_ind <- c(lat_lon_ind$start, time_ind$start)
    count_ind <- c(lat_lon_ind$count, time_ind$count)
  } else {
    start_ind <- lat_lon_ind$start
    count_ind <- lat_lon_ind$count
  }

  var_data <- ncvar_get(nc = nc_file, varid = var_name,
                        start = start_ind, count = count_ind)

  if(count_ind[3] > 1) {
    array_margin <- case_when(all(count_ind[1:2] == 1) ~ 1,
                              any(count_ind[1:2] == 1) ~ 2,
                              all(count_ind[1:2] != 1) ~ 3)

    var_data <- array_branch(var_data, margin = array_margin)
  } else {
    var_data <- list(var_data)
  }

    if(all(count_ind[1:2] != 1)) {
      var_data <- map(var_data, rotate_cc)
    } else if (count_ind[1] == 1) {
      var_data <- var_data %>%
        map(., rev) %>%
        map(., ~matrix(.x, nrow = count_ind[2], ncol = count_ind[1]))
    } else if(count_ind[2] == 1) {
      var_data <- var_data %>%
        map(., ~matrix(.x, nrow = count_ind[2], ncol = count_ind[1]))
    }
  return(var_data)
}


## Function to rotate a matrix 90° counter clockwise
#' Rotate matrix 90° counter clockwise
#'
#' @param mtr Matrix to rotate
#'
#' @return Returns rotated matrix
#' @keywords internal
rotate_cc <- function(mtr) { mtr %>% t(.) %>% apply(., 2, rev)}
