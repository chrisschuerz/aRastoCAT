
aggregate_INCAbin <- function(bin_pth, basin_shp, bin_crs, bin_ext, shp_index) {

}



NBLOCKS_OUT <- 24 # timesteps per day (leave unchanged)
offset <- 500 #leave unchanged

# INCA - Austria
NROWS_INCA <- 351 #leave unchanged
NCOLS_INCA <-  601#leave unchanged

# INCA-Austria domain (small one); Numbers show the left, right, upper and lower coordinates of the domain; offset is included to position domain correctly, when exporting netCDF or ASCII
X0_INCA <- 99500 + offset
X1_INCA <- 699500 + offset
Y0_INCA <- 600500 - offset
Y1_INCA <- 250500 - offset

# Extent to be extracted from INCA-Austria; Numbers show the left, right, upper and lower coordinates of the domain
X0_Cut <- 394500 + offset
X1_Cut <- 611500 + offset
Y0_Cut <- 439500 - offset
Y1_Cut <- 300500 - offset



for (i in 1:NDAYS) {
  file <- paste(getwd(), files[i], sep = "/")
  print(file)
  zz <- file(file, "rb")
  bin <-
    readBin(
      zz,
      what = "numeric",
      n = NROWS_INCA * NCOLS_INCA * NBLOCKS_INCA,
      size = 4
    )
  close(zz)
