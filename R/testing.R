ncdf_file <- "C:/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"
# Load basin boundary shape file --------------------------------------
bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Watershed/Shapes/"
bnd_file_name = "subs1.shp"
bnd_shp <- readOGR(paste(bnd_dir,bnd_file_name, sep = ""),
                   layer = "subs1")

crs_nc <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

test <- aggregate_ncdf(ncdf_pth = ncdf_file, basin_shp = bnd_shp, ncdf_crs = crs_nc, shp_index = "Subbasin")
