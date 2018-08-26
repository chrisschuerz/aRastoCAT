#' Create a simple feature object that provides each pixel of the var data as a
#' polygon, that is used for intersecting with the subunits of the provided
#' shape file.
#'
#' @param var_data Tibble holding the data of the variable extracted from the
#'   ncdf file
#' @param lat_lon List that holds the lat/lon matrices
#' @param shape_file The shape file used for later aggregation.
#' @param crs_ncdf The reference system of the ncdf file provided as character
#'   string.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map
#' @importFrom sf st_crs st_polygon st_set_agr st_sfc st_sf st_transform
#'
#' @return Returns a tibble holding the pixels of the data set as simple feature
#'   polygons.
#' @keywords internal
#'
create_polygon_grid <- function(var_data, lat_lon, shape_file, crs_ncdf) {
  ## Derive the dimensions of the trimmed matrices
  rst_dim <- dim(lat_lon[[1]])

  ## Create all combinations of x/y of the matrice indices
  rst_ind <- expand.grid(1:(rst_dim[1]), 1:(rst_dim[2]))

  ## Create a reduced set of index combinations required for the corner point
  ## calculation
  pnt_ind <- expand.grid(1:(rst_dim[1] - 1), 1:(rst_dim[2] - 1))

  ## Calculate the latitude values of all corner points and extrapolate the
  ## values at the outer rows and columns
  lat_corner <- apply(pnt_ind, 1, calc_corner, lat_lon[[1]]) %>%
    matrix(., nrow = rst_dim[1] - 1, ncol = rst_dim[2] - 1) %>%
    extrapol_row(.) %>%
    extrapol_col(.)

  ## Calculate the longitude values of all corner points and extrapolate the
  ## values at the outer rows and columns
  lon_corner <- apply(pnt_ind, 1, calc_corner, lat_lon[[2]]) %>%
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

  return(var_grid)
}

#' Helper function to calculate the mid point between four lat/lon points to
#' derive the corner points of the polygon grid cells.
#'
#' @param ind Index positions in the matrix
#' @param mtr matrix for which the mid points are calculated.
#'
#' @return Returns a matrix holding the interpolated mid points of the original
#'   matrix
#' @keywords internal
#'
calc_corner <- function(ind, mtr) {
  mean(mtr[ind[1]:(ind[1] + 1), ind[2]:(ind[2] + 1)])}

#' Helper function to extrapolate the corner points of the grid in longitude.
#'
#' @param mtr matrix for which the longitude boundaries are extrapolated.
#'
#' @return Returns a matrix holding with extrapolated lon boundaries
#' @keywords internal
#'
extrapol_col <- function(mtr){
  n_col <- ncol(mtr)
  cbind(mtr[ , 1] + (mtr[ , 1] - mtr[ , 2]),
        mtr,
        mtr[ , n_col] + (mtr[ , n_col] - mtr[, n_col - 1]))
}

#' Helper function to extrapolate the corner points of the grid in latitude.
#'
#' @param mtr matrix for which the latitude boundaries are extrapolated.
#'
#' @return Returns a matrix holding with extrapolated lat boundaries
#' @keywords internal
#'
extrapol_row <- function(mtr){
  n_row <- nrow(mtr)
  rbind(mtr[1, ] + (mtr[1, ] - mtr[2, ]),
        mtr,
        mtr[n_row, ] + (mtr[n_row, ] - mtr[n_row - 1, ]))
}

#' Helper function to calculate the coordinates of the pixel polygons.
#'
#' @param ind Index position of respective pixel.
#' @param lat Latitude matrix
#' @param lon Longitude matrix
#'
#' @return Returns a list of tables, where each table holds the coordinates
#'   that describe the path around a cell of the polygon grid.
#' @keywords internal
#'
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
