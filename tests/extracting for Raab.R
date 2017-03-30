# Libraries -----------------------------------------------------------
library(rgdal)
library(pasta)
library(raster)
library(aRastoCAT)
library(magrittr)

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


# INCA data for model calibration -------------------------------------
sub_size <- c(4,30,54)
variable <- data.frame(var = c("pcp", "tmp"),
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


# ZAMG Climate change scenarios --------------------------------------------
nc_full_pth <- "I:/UnLoadC3"
nc_full <- list.files(path = nc_full_pth, full.names = TRUE)
nc_pth  <- "D:/UnLoadC3/01_Datengrundlage/06_WEATHER/ncdf_2071_2100"


# For Unload select only climate projections for far future (2071 - 2100)
# and copy them on local hard drive
nc_ff <- nc_full %>% 
  .[grepl("2071-2100", .)] %>% 
  .[grepl("pr|tasmin|tasmax", .)]

dir.create(nc_pth)
sapply(nc_ff, file.copy, nc_pth)

# Extract ncdf data for the different spatial aggregations of the Raab
# catchment.

