#' Aggregate time series derived from climate raster aggregation
#'
#' @param ts_tbl tibble with timeseries data for basin subunits
#' @param time_int character time interval to aggregate, either 'year',
#'   'mon', 'day', 'hour', or 'min'
#' @param aggr_fun Aggregation function to apply
#'
#' @return Returns an aggregated time series tibble
#' @export

aggregate_timeseries <- function(ts_tbl, time_int = "day", aggr_fun = mean) {
  time_step <- c("year", "mon", "day", "hour", "min")
  time_step <- time_step[1:which(time_step == time_int)] %>%
    lapply(., as.symbol)
  #
  ts_tbl %>%
    group_by_(.dots = time_step) %>%
    summarise_at(vars(-year, -mon, -day, -hour, - min), funs(aggr_fun))
}
