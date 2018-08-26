library(rgdal)
library(pasta)
library(ncdf4)

ncdf_file <- "C:/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"
# Load basin boundary shape file --------------------------------------
bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Watershed/Shapes/"
bnd_file_name = "subs1.shp"
basin_shp <- readOGR(,
                   layer = "subs1")

crs_ncdf <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

nc_test <- aggregate_ncdf(ncdf_pth = ncdf_pth,
                          basin_shp = basin_shp,
                          ncdf_crs = crs_nc,
                          shp_index = "Subbasin",
                          var_lbl = "pr")


ncdf_folder <- "G:/UnLoadC3"
ncdf_files  <- list.files(ncdf_folder, full.names = FALSE)

clim_4 <- list()
for (i in 1:length(ncdf_files)){
  file_meta <- strsplit(ncdf_files[i], "_|\\.") %>%
    unlist() %>%
    gsub("-", "_",.)

  var_label <- file_meta[1]
  run_label <- paste(file_meta[c(1, 4:7,12)], collapse = "_")

  clim_4[[run_label]] <- aggregate_ncdf(ncdf_pth = ncdf_folder%//%ncdf_files[i],
                                        basin_shp = bnd_shp,
                                        ncdf_crs = crs_nc,
                                        shp_index = "Subbasin",
                                        var_lbl = var_label)
}

nc_test <- list.files(path = "C:/", pattern = ".nc$", full.names = TRUE)

clim_test <- aggregate_ncdf(ncdf_pth = nc_test,
                            basin_shp = basin_shp,
                            ncdf_crs = crs_nc,
                            shp_index = "Subbasin",
                            var_lbl = "pr")


# Test aggregate_INCAbin ----------------------------------------------
bin_pth <- "D:/INCA_T"

t1 <- system.time({
test1 <- aggregate_INCAbin(bin_pth = bin_pth,
                          basin_shp = basin_shp, bin_crs = crs(basin_shp),
                          bin_ext = c(99000, 700000, 250000, 601000),
                          shp_index = "Subbasin")

})
t1

t4 <- system.time({
test4 <- aggregate_INCAbin(bin_pth = bin_pth,
                          basin_shp = basin_shp, bin_crs = crs(basin_shp),
                          bin_ext = c(99000, 700000, 250000, 601000),
                          shp_index = "Subbasin", n_core = 4)

})
t4

t8 <- system.time({
test8 <- aggregate_INCAbin(bin_pth = bin_pth,
                          basin_shp = basin_shp, bin_crs = crs(basin_shp),
                          bin_ext = c(99000, 700000, 250000, 601000),
                          shp_index = "Subbasin", n_core = 8)

})

t8
