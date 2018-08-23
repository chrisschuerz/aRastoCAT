#' Fetch the laitude and longitude variables and convert to matrices
#'
#' @param nc_file Opnened ncdf file
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
#' @param nc_file Opnened ncdf file
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






## Function to rotate a matrix 90° counter clockwise
#' Rotate matrix 90° counter clockwise
#'
#' @param mtr Matrix to rotate
#'
#' @return Returns rotated matrix
#' @keywords internal
rotate_cc <- function(mtr) { mtr %>% t(.) %>% apply(., 2, rev)}
