#' Intersect var_grid with the shape file and aggregate the variable's data for
#' the shape file units
#'
#' @param var_grid simple feature polygon. The polygon grid for the individual
#'   pixels of the variable
#' @param var_data Tibble where each row represents a pixel in the var_grid and
#'   a column holds the data for one time step in each pixel.
#' @param shape_file Shape file with the basin sub-unit polygons.
#' @param shape_index Character. Name of the column in the basin shapefile
#'   attribute table that provides the indices of the basin subunits.
#' @param time Date Vector for the time steps in var_data
#'
#' @importFrom dplyr funs group_by left_join mutate summarise_all ungroup %>%
#' @importFrom sf st_area st_intersection st_set_agr
#' @importFrom tibble as_tibble tibble
#'
#' @return Returns a tibble that provides the time series of the aggregated
#'   variable for the respective basin subunits
#' @keywords internal

aggregate_variable <- function(var_grid, var_data,
                               shape_file, shape_index, time) {
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
    left_join(., var_data, by = "idx") %>% # Join with variable data
    multiply_by_fraction(.) %>%
    summarise_all(funs(sum), na.rm = TRUE) %>% # Sum up the fractions for all shape sub units
    ungroup(.) %>%
    mutate(., index = shape_index%_%index) %>%
    transpose_tbl(., name_col = "index") %>%
    add_time_if_exists(., time)

  return(data_aggregate)
}

#' Helper function to multiply each 'timestep' column of the data set by the
#' areal fraction of each pixel (speeds up the process compared to mutate_at)
#'
#' @param tbl The data table that holds the area fraction for all pixels, the
#'   data for each time step and each pixel and the grouping variable index.
#'
#' @importFrom dplyr bind_cols group_by select starts_with ungroup %>%
#' @importFrom purrr map_dfc
#'
#' @return Returns the tibble including the index and all timesteps multiplied
#'   by fraction.
#' @keywords internal
#'
multiply_by_fraction <- function(tbl) {
  data <- tbl %>% ungroup() %>% select(starts_with("timestep"))
  fract <- tbl$area_fract
  data %>%
    map_dfc(., ~.x*fract) %>%
    bind_cols(tbl %>% select(index),.) %>%
    group_by(index)
}

#' Helper function to transpose the data tibble and to name the columns by the
#' character values in the indicated column
#'
#' @param tbl The data table that should be transposed
#' @param name_col Character string that defines the name column
#'
#' @importFrom dplyr matches select %>%
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble
#'
#' @return Returns the transposed tibble with the column names indicated in the
#'   column name_col
#' @keywords internal
#'
transpose_tbl <- function(tbl, name_col) {
  col_names <- tbl[[name_col]]
  tbl <- tbl %>%
    select(-matches(name_col)) %>%
    t(.) %>%
    as_tibble(.) %>%
    set_names(., col_names)
  return(tbl)
}

#' Helper function to add the time column to the tibble if the time vector is
#' not NULL
#'
#' @param tbl The tibble holding the aggregated data
#' @param time Time vector
#'
#' @importFrom tibble add_column
#'
#' @return Returns the tibble with the time columns at the first position if
#'   time is not NULL
#' @keywords internal
#'
add_time_if_exists <- function(tbl, time) {
  if(!is.null(time)){
    add_column(tbl, date = time, .before = 1)
  }
}
