
aggregate_INCAbin <- function(bin_pth, basin_shp, bin_crs, bin_ext, shp_index) {


# Fetch header and binary files names from binary folder --------------
  hdr_lst <- list.files(path = bin_pth, pattern = ".hdr$")
  bil_lst <- list.files(path = bin_pth, pattern = ".bil$")


# Get bin meta data from first found header file ----------------------
  header <- read.table(file = bin_pth%//%hdr_lst[1], header = FALSE,
                       stringsAsFactors = FALSE)
  hdr_col <- header[,1]
  header %<>%
    .[,2] %>%
    as.numeric() %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(hdr_col)

# Create raster with cell indices as values and convert to spatial polygon
  assign_crs <- function(sp_obj, crs_new) {
    crs(sp_obj) <- crs_new
    return(sp_obj)
  }
  assign_ext <- function(sp_obj, ext_new) {
    extent(sp_obj) <- ext_new
    return(sp_obj)
  }

  idx_poly <- matrix(data = 1:(header$NROWS * header$NCOLS),
                     nrow = header$NCOLS) %>%
    t() %>%
    apply(., 2, rev) %>%
    raster() %>%
    assign_ext(., bin_ext) %>%
    assign_crs(., crs(bin_crs)) %>%
    as(., "SpatialPolygonsDataFrame")


  i <- 1

  bil_i <- readBin(con = bin_pth%//%bil_lst[i],
                   what = "numeric",
                   n = header$NROWS * header$NCOLS * header$NBLOCKS,
                   size = 4)


}

# Testing -------------------------------------------------------------
library(pasta)
library(magrittr)
library(dplyr)
library(raster)

bin_ext <- c(99000, 700000, 250000, 601000)
bin_pth <- "F:/mirror_H/ETP_AT/ETP_AT_Exe/input/T2M"

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
