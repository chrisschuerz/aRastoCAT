


write_SWATweather <- function(pcp_tbl = NULL, tmn_tbl = NULL, tmx_tbl = NULL,
                              hmd_tbl = NULL, slr_tbl = NULL, wnd_tbl = NULL,
                              basin_shp, write_pth, out_type = "ArcSWAT") {

  if(xor(is.null(tmn_tbl), is.null(tmx_tbl))){
    stop("tmn_tbl and tmx_tbl must be provided together!")
  }


  loc_data <- tibble(ID = basin_shp@data$OBJECTID,
                     NAME = "sub"%_%ID,
                     LAT = basin_shp@data$Lat,
                     LONG = basin_shp@data$Long_,
                     ELEVATION = basin_shp@data$Elev)

  if(!missing(tmn_tbl) & !missing(tmx_tbl)){
  tmn_tbl %<>% set_colnames(c("date","tmn"%_%loc_data$ID))
  tmx_tbl %<>% set_colnames(c("date","tmx"%_%loc_data$ID))
  tmp_sort <- c("date", rep(c("tmx", "tmn"), nrow(loc_data))%_%
                        rep(loc_data$ID, each = 2))
  tmp_tbl <- left_join(tmn_tbl, tmx_tbl, by = "date") %>%
    select(one_of(tmp_sort))

  rm(tmn_tbl, tmx_tbl)
  }

  tbl_lst <- ls(pattern = "_tbl$")
  for(i in 1:length(tbl_lst)){
    var <- get(tbl_lst[i])
    if(is.null(var)) tbl_lst[i] <- NA
  }
  tbl_lst <- tbl_lst[!is.na(tbl_lst)]

}

file_name <- tibble(tbl  = c("pcp_tbl", "tmp_tbl", "hmd_tbl",
                             "slr_tbl", "wnd_tbl"),
                    file = c("pcp1.pcp", "tmp1.tmp", "hmd.hmd",
                             "slr.slr", "wnd.wnd"))

tbl_fmt <- tibble(tbl = c("pcp_tbl", "tmp_tbl", "hmd_tbl",
                           "slr_tbl", "wnd_tbl"),
                  fmt = c("%05.1f", "%05.1f", "%08.3f",
                          "%08.3f", "%08.3f"))

write_txtIOheader <- function(i_tbl, var_tbl, loc_data, write_pth, file_name){
  if(i_tbl %in% c("pcp_tbl", "tmp_tbl")){
    if(i_tbl == "pcp_tbl"){
      fmt = c("%5.1f", "%5.0f")
    } else {
      fmt = c("%10.1f", "%10.0f")
    }

    loc_data %>%
      select(-ID) %>%
      mutate_at(.cols =   1, funs(paste0(.,","))) %>%
      mutate_at(.cols = 2:3, funs(sprintf(fmt[1], .))) %>%
      mutate_at(.cols =   4, funs(sprintf(fmt[2], .))) %>%
      set_colnames(c("Station", "Lati   ", "Long   ", "Elev   ")) %>%
      t() %>%
      write.table(file = write_pth%//%file_name$file[file_name$tbl == i_tbl],
                  quote = FALSE, col.names = FALSE, sep = "")
  } else {
    writeLines(text = paste("Input File", file_name$file[file_name$tbl == i_tbl]),
               con = write_pth%//%file_name$file[file_name$tbl == i_tbl])
  }
}

write_txtIOtable <- function(i_tbl, var_tbl, write_pth, file_name, fmt){
  var_tbl %>%
    mutate(date = format(date, "%Y%j")) %>%
    mutate_at(.cols = 2:ncol(.), funs(ifelse(is.na(.), -99, .))) %>%
    mutate_at(.cols = 2:ncol(.), funs(sprintf(fmt$fmt[fmt$tbl == i_tbl], .))) %>%
    write.table(file = write_pth%//%file_name$file[file_name$tbl == i_tbl], sep = "",
                append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}




# testing -------------------------------------------------------------
write_txtIOheader("pcp_tbl", pcp_tbl, loc_data, "C:/", file_name)
write_txtIOtable("pcp_tbl", pcp_tbl, "C:/", file_name, tbl_fmt)
