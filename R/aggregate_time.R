#' Aggregate time series derived from climate raster aggregation
#'
#' @param ts_tbl tibble with timeseries data for basin subunits
#' @param time_int character time interval to aggregate, either 'year',
#'   'mon', 'day', 'hour', or 'min'
#' @param aggr_fun Aggregation function to apply
#' @param drop_col logical to set whether unused time columns are dropped
#'   (TRUE) or kept in the table (FALSE). If FALSE unused time columns
#'   values are set to zero.
#'
#' @return Returns an aggregated time series tibble
#' @export

aggregate_time <- function(ts_tbl, time_int = "day", drop_col = FALSE,
                           aggr_fun = mean, ...) {
  time_step <- c("year", "mon", "day", "hour", "min")
  time_keep <- time_step[1:which(time_step == time_int)]
  time_drop <- time_step[!(time_step %in% time_keep)]
  time_grp  <- time_keep %>%
    lapply(., as.symbol)

  aggr_wrap <- function(x){ aggr_fun(x, ...) }

  ts_tbl %<>%
    group_by_(.dots = time_grp) %>%
    summarise_all(funs(aggr_wrap)) %>%
    ungroup()

  if(drop_col){
    ts_tbl %>%
      select(-one_of(time_drop))
  } else {
    ts_tbl %>%
      mutate_at(vars(one_of(time_drop)), funs(.*0))
  }
}
