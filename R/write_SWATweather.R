


write_SWATweather <- function(pcp_tbl, tmn_tbl, tmx_tbl,
                              hmd_tbl, slr_tbl, wnd_tbl,
                              basin_shp, write_pth, out_type = "ArcSWAT") {

  if(xor(missing(tmn_tbl), missing(tmx_tbl))){
    stop("tmn_tbl and tmx_tbl must be provided together!")
  }

  if(!missing(tmn_tbl) & !missing(tmx_tbl)){

  }


  loc_data <- tibble(ID = basin_shp@data$OBJECTID,
                     NAME = "sub"%_%ID,
                     LAT = basin_shp@data$Lat,
                     LONG = basin_shp@data$Long_,
                     ELEVATION = basin_shp@data$Elev)



  if(!missing(pcp_tbl)){

  }

}

write_txtIOheader <- function(i_tbl, var_tbl, loc_data, write_pth){
  file_name <- tibble(tbl  = c("pcp_tbl", "tmp_tbl", "hmd_tbl",
                               "slr_tbl", "wnd_tbl"),
                      file = c("pcp1.pcp", "tmp1.tmp", "hmd.hmd",
                               "slr.slr", "wnd.wnd"))

  if(i_tbl %in% c("pcp_tbl", "tmp_tbl")){
    loc_data %>%
      select(-ID) %>%
      mutate_at(.cols =   1, funs(paste0(.,","))) %>%
      mutate_at(.cols = 2:3, funs(sprintf("%10.1f", .))) %>%
      mutate_at(.cols =   4, funs(sprintf("%10.0f", .))) %>%
      set_colnames(c("Station", "Lati   ", "Long   ", "Elev   ")) %>%
      t() %>%
      write.table(file = write_pth%//%file_name$file[file_name$tbl == i_tbl],
                  quote = FALSE, col.names = FALSE)
  } else {
    writeLines(text = paste("Input File", file_name$file[file_name$tbl == i_tbl]),
               con = write_pth%//%file_name$file[file_name$tbl == i_tbl])
  }
}
