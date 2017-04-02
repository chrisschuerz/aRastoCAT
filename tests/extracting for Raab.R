# Libraries -----------------------------------------------------------
library(rgdal)
library(pasta)
library(raster)
library(aRastoCAT)
library(magrittr)
library(tibble)
library(dplyr)

# Functions -----------------------------------------------------------
trim_by_regex <- function(string, pattern, n_pattern, reverse = FALSE) {
  nchar_str <- nchar(string)
  pat_pos <- string %>%
    gregexpr(pattern,.) %>%
    unlist()
  init_str <- ifelse(reverse,
                     pat_pos[length(pat_pos) + 1 - n_pattern] + 1,
                     pat_pos[n_pattern] + 1)
  substr(string, init_str, nchar_str)
}

get_ncdfmeta_from_filename <- function(file_name) {
  file_name %>%
    strsplit(., "_") %>%
    unlist() %>%
    .[c(1,4,7,5, 12)] %>%
    t() %>%
    as_tibble() %>%
    set_colnames(c("variable", "gcm", "rcm", "rcp", "period")) %>%
    mutate(gcm = trim_by_regex(gcm, "-", 2, TRUE),
           rcm = trim_by_regex(rcm, "-", 1))
}


# INCA data for model calibration -------------------------------------
sub_size <- c(4,30,54)
variable <- tibble(var = c("pcp", "tmp"),
                   pth = c("RR",  "T2M"))

clim_inca <- list()

for(i_sub in sub_size){
  basin_pth <- "D:/UnLoadC3/00_RB_SWAT/raab_sb"%&%
               i_sub%//%
               "Watershed/Shapes/subs1.shp"
  basin_shp <- readOGR(basin_pth, layer = "subs1")

  clim_inca[["sb"%&%i_sub]] <- list()

  for(i_var in 1:nrow(variable)){
    bin_pth <- "F:/mirror_H/ETP_AT/ETP_AT_Exe/input"%//%variable$pth[i_var]
    clim_inca[["sb"%&%i_sub]][[variable$var[i_var]]] <-
      aggregate_INCAbin(bin_pth = bin_pth,
                        basin_shp = basin_shp, bin_crs = crs(basin_shp),
                        bin_ext = c(99500, 700500, 249500, 600500),
                        shp_index = "Subbasin")
  }
}

save(clim_inca, file =  "D:/UnLoadC3/00_RB_SWAT/clim_inca.RData")


# Write ArcSWAT inputs from INCA files --------------------------------
# Aggregate pcp files
clim_inca_aggr <- clim_inca
for(i_sub in sub_size){
  clim_inca_aggr[["sb"%&%i_sub]]$pcp %<>%
    aggregate_time(., time_int = "day",drop_col = TRUE, aggr_fun = sum)
}

write_pth <- "D:/UnLoadC3/00_RB_SWAT/climate/observation"

for (i_sub in sub_size){
  basin_pth <- "D:/UnLoadC3/00_RB_SWAT/raab_sb"%&%
    i_sub%//%
    "Watershed/Shapes/subs1.shp"
  basin_shp <- readOGR(basin_pth, layer = "subs1")

  sub_name <- "sb"%&%i_sub

  dir.create(write_pth%//%sub_name)
  write_SWATweather(pcp_tbl = clim_inca_aggr[["sb"%&%i_sub]]$pcp,
                    tmp_tbl = clim_inca_aggr[["sb"%&%i_sub]]$tmp,
                    basin_shp = basin_shp,
                    write_pth = write_pth%//%sub_name,
                    out_type = "ArcSWAT")
}

# ZAMG Climate change scenarios --------------------------------------------
sub_size <- c(4,30,54)

nc_full_pth <- "I:/UnLoadC3"
nc_full <- list.files(path = nc_full_pth, full.names = FALSE)
nc_pth  <- "D:/UnLoadC3/01_Datengrundlage/06_WEATHER/ncdf_2071_2100"
crs_nc <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# For Unload select only climate projections for far future (2071 - 2100)
# and copy them on local hard drive
# nc_ff <- nc_full %>%
#   .[grepl("2071-2100", .)] %>%
#   .[grepl("pr|tas", .)]

nc_ff <- list.files(nc_pth)%>%
  .[grepl("pr|tasmin|tasmax", .)]
#dir.create(nc_pth)
#sapply(nc_full_pth%//%nc_ff, file.copy, nc_pth) #Already copied

# Extract ncdf data for the different spatial aggregations of the Raab
# catchment.

clim_2071_2100 <- list()
pb <- progress_estimated(length(sub_size)*length(nc_ff))
for(i_sub in sub_size){
  basin_pth <- "D:/UnLoadC3/00_RB_SWAT/raab_sb"%&%
               i_sub%//%
               "Watershed/Shapes/subs1.shp"
  basin_shp <- readOGR(basin_pth, layer = "subs1")

  clim_2071_2100[["sb"%&%i_sub]] <- list()
  pb$print()
  for (i_nc in nc_ff){
    nc_i_meta <- get_ncdfmeta_from_filename(i_nc)
    run_i <- nc_i_meta$gcm %_% nc_i_meta$rcm %_% nc_i_meta$rcp

    if(is.null(clim_2071_2100[["sb"%&%i_sub]][[run_i]])){
      clim_2071_2100[["sb"%&%i_sub]][[run_i]] <- list()
    }

    clim_2071_2100[["sb"%&%i_sub]][[run_i]][[nc_i_meta$variable]] <-
    aggregate_ncdf(ncdf_pth = nc_pth%//%i_nc,
                   basin_shp = basin_shp,
                   ncdf_crs = crs_nc,
                   shp_index = "Subbasin",
                   var_lbl = nc_i_meta$variable)
    pb$tick()$print()
  }

}
save(clim_2071_2100, file = "D:/UnLoadC3/00_RB_SWAT/clim_2071_2100.RData")
pb$stop()

# Write txtIO weather files from ZAMG ncdf projections ---------------------
write_pth <- "D:/UnLoadC3/00_RB_SWAT/climate/projection"
proj_name <- names(clim_2071_2100$sb4)

for (i_sub in sub_size){
  basin_pth <- "D:/UnLoadC3/00_RB_SWAT/raab_sb"%&%
    i_sub%//%
    "Watershed/Shapes/subs1.shp"
  basin_shp <- readOGR(basin_pth, layer = "subs1")

  for (i_proj in proj_name){
    dir.create(write_pth%//%"sb"%&%i_sub%//%i_proj, recursive = TRUE)
    clim <- clim_2071_2100[["sb"%&%i_sub]][[i_proj]]

    write_SWATweather(pcp_tbl = clim$pr,
                      tmx_tbl = clim$tasmax,
                      tmn_tbl = clim$tasmin,
                      basin_shp = basin_shp,
                      write_pth = write_pth%//%"sb"%&%i_sub%//%i_proj,
                      out_type = "txtIO")
  }
}



