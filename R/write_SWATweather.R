#' Write SWAT weather input from aggregated climate variable time series
#'
#' @param pcp_tbl data.frame providing time series of daily precipitation
#'   for the basins subunits
#' @param tmn_tbl data.frame providing time series of daily minimum
#'   temperature for the basins subunits
#' @param tmx_tbl data.frame providing time series of daily minimum
#'   temperature for the basins subunits
#' @param hmd_tbl data.frame providing time series of daily relative
#'   humidity for the basins subunits
#' @param slr_tbl data.frame providing time series of daily solar radiation
#'   for the basins subunits
#' @param wnd_tbldata.frame providing time series of daily average wind
#'   speed for the basins subunits
#' @param basin_shp Shape file of the basin subunits
#' @param write_pth Path where the SWAT input files are written
#' @param out_type Type of output: either "ArcSWAT" to create input files
#'   for ArcSWAT model setup or "txtIO" for direct input into the txtInOut
#'   folder of an existing project (be careful with this method! Might
#'   require changes in file.cio and *.sub files!)
#' @importFrom pasta %_% %//% %&% %&&%
#' @importFrom dplyr mutate mutate_at select one_of left_join vars funs
#' @importFrom tibble tibble
#' @importFrom magrittr %>% %<>% set_colnames
#'
#' @return Writes Weather input files for a SWAT2012 model
#' @export

write_SWATweather <- function(pcp_tbl = NULL, tmp_tbl = NULL,
                              tmn_tbl = NULL, tmx_tbl = NULL,
                              hmd_tbl = NULL, slr_tbl = NULL,
                              wnd_tbl = NULL, basin_shp,
                              out_type = "ArcSWAT", write_pth) {
# Static properties in a SWAT2012 project ---------------------------------
  file_name <- tibble(tbl  = c("pcp_tbl", "tmp_tbl", "hmd_tbl",
                               "slr_tbl", "wnd_tbl"),
                      file = c("pcp1.pcp", "tmp1.tmp", "hmd.hmd",
                               "slr.slr", "wnd.wnd"))

  tbl_fmt <- tibble(tbl = c("pcp_tbl", "tmp_tbl", "hmd_tbl",
                            "slr_tbl", "wnd_tbl"),
                    fmt = c("%05.1f", "%05.1f", "%08.3f",
                            "%08.3f", "%08.3f"))
# -------------------------------------------------------------------------
  loc_data <- tibble(ID = basin_shp@data$OBJECTID,
                     NAME = "sub"%_%ID,
                     LAT = basin_shp@data$Lat,
                     LONG = basin_shp@data$Long_,
                     ELEVATION = basin_shp@data$Elev)

  if(xor(is.null(tmn_tbl), is.null(tmx_tbl))){
    stop("tmn_tbl and tmx_tbl must be provided together!")
  }

  if(!is.null(tmp_tbl) & (!is.null(tmn_tbl) | !is.null(tmn_tbl))){
    stop("EITHER tmin/tmax OR subdaily temperature time series must be provided.")
  }

  if(!is.null(tmp_tbl)){
    if(is_subdaily(tmp_tbl)){
      tmn_tbl <- aggregate_time(tmp_tbl, time_int = "day", aggr_fun = min)
      tmx_tbl <- aggregate_time(tmp_tbl, time_int = "day", aggr_fun = max)
      rm(tmp_tbl)
    } else {
      stop("For calculation of tmin/tmax subdaily timeseries of tmp is required.")
    }
  }

  if(!is.null(tmn_tbl) & !is.null(tmx_tbl)){
  tmn_tbl %<>%
    select(-matches("hour"), -matches("min")) %>%
    set_colnames(c("year", "mon", "day", "tmn"%_%loc_data$ID))
  tmx_tbl %<>%
    select(-matches("hour"), -matches("min")) %>%
    set_colnames(c("year", "mon", "day", "tmx"%_%loc_data$ID))
  tmp_sort <- c("year", "mon", "day", rep(c("tmx", "tmn"), nrow(loc_data))%_%
                        rep(loc_data$ID, each = 2))
  tmp_tbl <- left_join(tmn_tbl, tmx_tbl,
                       by = c("year", "mon", "day")) %>%
    select(one_of(tmp_sort))

  rm(tmn_tbl, tmx_tbl)
  }

  tbl_lst <- ls(pattern = "_tbl$")


  for (i_tbl in tbl_lst){
    var <- get(i_tbl)
    if(!is.null(var)){
      switch(out_type,
             ArcSWAT = {
               write_ArcSWAT(i_tbl, var, loc_data, write_pth, file_name)
             },
             txtIO   = {
               write_txtIOheader(i_tbl, var, loc_data, write_pth, file_name)
               write_txtIOtable(i_tbl, var, write_pth, file_name, tbl_fmt)
             })
    }
  }
}

write_txtIOheader <- function(i_tbl, var_tbl, loc_data, write_pth, file_name){
  if(i_tbl %in% c("pcp_tbl", "tmp_tbl")){
    if(i_tbl == "pcp_tbl"){
      fmt = c("%5.1f", "%5.0f")
    } else {
      fmt = c("%10.1f", "%10.0f")
    }
    if(is_subdaily(var_tbl)){
      col_name <- c("Station ", "Lati        ",
                    "Long        ", "Elev        ")
    } else {
        col_name <- c("Station ", "Lati   ", "Long   ", "Elev   ")
      }

    loc_data %>%
      select(-ID) %>%
      mutate_at(.cols =   1, funs(paste0(.,","))) %>%
      mutate_at(.cols = 2:3, funs(sprintf(fmt[1], .))) %>%
      mutate_at(.cols =   4, funs(sprintf(fmt[2], .))) %>%
      set_colnames(col_name) %>%
      t() %>%
      write.table(file =  write_pth%//%file_name$file[file_name$tbl == i_tbl],
                  quote = FALSE, col.names = FALSE, sep = "")
  } else {
    writeLines(text = paste("Input File", file_name$file[file_name$tbl == i_tbl]),
               con =  write_pth%//%file_name$file[file_name$tbl == i_tbl])
  }
}

write_txtIOtable <- function(i_tbl, var_tbl, write_pth, file_name, fmt){
  if(is_subdaily(var_tbl)){
    var_tbl %<>% add_column(date = as.Date(var_tbl$year%-%
                                           var_tbl$mon%-%
                                           var_tbl$day) %>%
                                   format("%Y%j") %>%
                                   paste0(.,sprintf("%02d",var_tbl$hour)%&&%
                                            sprintf("%02d",var_tbl$min)),
                            .before = 1) %>%
      select(-matches("year")) %>%
      select(-matches("mon")) %>%
      select(-matches("day")) %>%
      select(-matches("hour")) %>%
      select(-matches("min"))
  } else {
    var_tbl %<>%
      add_column(date = as.Date(var_tbl$year%-%
                                           var_tbl$mon%-%
                                           var_tbl$day) %>%
                                   format("%Y%j"),
                            .before = 1) %>%
      select(-matches("year")) %>%
      select(-matches("mon")) %>%
      select(-matches("day")) %>%
      select(-matches("hour")) %>%
      select(-matches("min"))
  }
  var_tbl %>%
    mutate_at(.cols = 2:ncol(.), funs(ifelse(is.na(.), -99.0, .))) %>%
    mutate_at(.cols = 2:ncol(.), funs(sprintf(fmt$fmt[fmt$tbl == i_tbl], .))) %>%
    write.table(file = write_pth%//%file_name$file[file_name$tbl == i_tbl],
                sep =  "", append = TRUE, quote = FALSE, col.names = FALSE,
                row.names = FALSE)
}

write_ArcSWAT <- function(i_tbl, var_tbl, loc_data, write_pth, file_name) {
  if(is_subdaily(var_tbl)){
    h <- ifelse(rep(is.null(var_tbl$hour),2), c(0L,0L), var_tbl$hour[1:2])
    m <- ifelse(rep(is.null(var_tbl$min),2),  c(0L,0L), var_tbl$min[1:2])
    t_diff <- (60*h + m) %>% diff
    t_stamp <- paste(var_tbl$year[1]%&%sprintf("%02d",var_tbl$mon[1])%&%
                     sprintf("%02d",var_tbl$day[1]), t_diff, sep = ", ")
  } else {
    t_stamp <- var_tbl$year[1]%&%sprintf("%02d",var_tbl$mon[1])%&%
                sprintf("%02d",var_tbl$day[1])
  }
  var_name <- gsub("_tbl$", "", i_tbl)
  if(!dir.exists(write_pth%//%var_name)){
    dir.create(write_pth%//%var_name, recursive = TRUE)

    loc_data %<>% mutate_at(.cols = 3:5, funs(round(., digits = 3)))
    write.csv(x = loc_data, file = write_pth%//%var_name%//%
              var_name%_%"loc.txt", quote = FALSE, row.names = FALSE)

    n <- nrow(var_tbl)
    log_file <- c("time period =  "%&%as.Date(var_tbl$year[1]%-%var_tbl$mon[1]%-%
                                              var_tbl$day[1])%&&%"to"%&&%
                                      as.Date(var_tbl$year[n]%-%var_tbl$mon[n]%-%
                                              var_tbl$day[n]),
                  "time step =    "%&%ifelse(is_subdaily(var_tbl),
                                             t_diff%&&%"min", "1 day"),
                  "number years = "%&%(var_tbl$year[n] - var_tbl$year[1] + 1),
                  "",
                  "NA values :    ")

    writeLines(log_file, con = write_pth%//%var_name%//%var_name%_%"info.log")

    var_tbl %<>%
      select(-matches("year")) %>%
      select(-matches("mon")) %>%
      select(-matches("day")) %>%
      select(-matches("hour")) %>%
      select(-matches("min")) %>%
      mutate_all(funs(ifelse(is.na(.), -99.0, .))) %>%
      mutate_all(funs(round(.,digits = 2)))


    for(i_sub in loc_data$ID){
      var_i <- var_tbl %>%
        select(ends_with("_"%&%i_sub))

      write.table(t_stamp, file = write_pth%//%var_name%//%
                  loc_data$NAME[i_sub]%.%"txt",
                  quote = FALSE, row.names = FALSE, col.names = FALSE)
      write.table(var_i, sep = ",", file = write_pth%//%var_name%//%
                  loc_data$NAME[i_sub]%.%"txt", quote = FALSE,
                  row.names = FALSE, col.names = FALSE, append = TRUE)
      write.table("  "%&%sum(var_tbl == -99.0)%&&%"NA values in"%&&%
                  loc_data$NAME[i_sub],
                  file = write_pth%//%var_name%//%var_name%_%"info.log",
                  append = TRUE, col.names = FALSE, row.names = FALSE,
                  quote = FALSE)
    }
  } else {
    stop("Files for variable"%&&%var_name%&&%"already exists in the given path!")
  }
}

is_subdaily <- function(var_tbl) {
  subdaily <- FALSE
  if("hour" %in% colnames(var_tbl)){
    subdaily <- var_tbl$hour %>% diff %>% is_greater_than(0) %>% any()
  }
  if("min" %in% colnames(var_tbl)){
    subdaily <- var_tbl$hour %>% diff %>% is_greater_than(0) %>% any()
  }
  return(subdaily)
}
