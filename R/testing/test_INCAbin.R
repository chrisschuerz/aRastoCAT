# F:\mirror_H\Lehre\Masterarbeiten\mHmMur\InputDataGeneration
# F:\mirror_H\ETP_AT\ETP_AT_Exe\input


# # Testing -------------------------------------------------------------
# library(pasta)
# library(magrittr)
# library(dplyr)
# library(raster)
# library(tibble)
# library(rgdal)
# library(doSNOW)
# library(parallel)
#
# bin_pth <- "F:/mirror_H/ETP_AT/ETP_AT_Exe/input/T2M"
# bin_ext <- c(99000, 700000, 250000, 601000)
# bin_crs <- crs(basin_shp)
# shp_index <- "Subbasin"
#
#
# mt <- array(bil_i, dim = c(header$NCOLS, header$NROWS, header$NBLOCKS))
#
# mt <- bil_i[1:(header$NCOLS*header$NROWS)]
#
# mt[,,1] %>% View
#   raster() %>%
#   plot
#
#   idx_rst <-  matrix(data = 1:(header$NROWS * header$NCOLS),
#                      nrow = header$NCOLS)
#   idx_rst[1:5, 1:5] <- 200000
#
#   idx_rst%<>%
#     t() %>%
#     raster() %>%
#       assign_ext(., bin_ext) %>%
#       assign_crs(., crs(bin_crs))
#     projectRaster(crs = crs(basin_shp))


# NBLOCKS_OUT <- 24 # timesteps per day (leave unchanged)
# offset <- 500 #leave unchanged
#
# # INCA - Austria
# NROWS_INCA <- 351 #leave unchanged
# NCOLS_INCA <-  601#leave unchanged
#
# # INCA-Austria domain (small one); Numbers show the left, right, upper and lower coordinates of the domain; offset is included to position domain correctly, when exporting netCDF or ASCII
# X0_INCA <- 99500 + offset
# X1_INCA <- 699500 + offset
# Y0_INCA <- 600500 - offset
# Y1_INCA <- 250500 - offset


# Load basin boundary shape file --------------------------------------
# bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Watershed/Shapes/"
# bnd_file_name = "subs1.shp"
# basin_shp <- readOGR(paste(bnd_dir,bnd_file_name, sep = ""),
#                    layer = "subs1")
#
#
# plot(idx_poly)
# plot(basin_shp, add = T)
