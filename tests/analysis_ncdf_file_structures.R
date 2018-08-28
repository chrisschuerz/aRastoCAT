
crs_ncdf <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
shp_raab <- "D:/r_project/aRastoCAT/inst/extdata/basin_demo.shp"

## Spartacus_AT
spat_rr_path <- "D:/MetData/Spartacus_AT/RR/RR1961.nc"
spat_tbl <- aggregate_ncdf(ncdf_file = spat_rr_path, crs_ncdf = crs_ncdf,
                           shape_file = shp_raab, shape_index = "Subbasin",
                           time_range = c("1961-01-01", "1961-12-31"))

## Spartacus_Raab
# sprb_nc <- ncdf4::nc_open(sprb_rr_path)
#
# ext <- extent(c(546000, 549000, 397000, 395000))
#
# shp_ext <- as(ext, 'SpatialPolygons')
# crs(shp_ext) <- crs_lmb
# shapefile(shp_ext, "D:/MetData/Spartacus_Raab/shp_file.shp", overwrite = TRUE)
# shape_dummy <- sf::read_sf("D:/MetData/Spartacus_Raab/shp_file.shp")
sprb_rr_path <- "D:/MetData/Spartacus_Raab/RR/RR_19610101.nc"
crs_lmb <- "+proj=lcc +lat_1=46 +lat_2=49 +lat_0=47.5 +lon_0=13.33333333333333 +x_0=400000 +y_0=400000 +ellps=bessel +units=m +no_defs"
sprb_tbl <- aggregate_ncdf(ncdf_file = sprb_rr_path, crs_ncdf = crs_lmb,
                           shape_file = shp_raab, shape_index = "Subbasin")

## UnLoad
unld_rr_path <- "D:/MetData/UnLoadC3/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"
unld_tbl <- aggregate_ncdf(ncdf_file = unld_rr_path, crs_ncdf = crs_ncdf,
                           shape_file = shp_raab, shape_index = "Subbasin",
                           time_range = c("1980-01-01", "1980-12-31"))

## EOBS
eobs_rr_path <- "D:/MetData/EOBS/rr_0.25deg_reg_v17.0.nc"
shp_szyb <- "D:/MetData/SalzachYbbs/salzachybbs.shp"
eobs_rr_path <- "D:/MetData/EOBS/rr_0.25deg_reg_v17.0.nc"
eobs_tbl <- aggregate_ncdf(ncdf_file = eobs_rr_path, crs_ncdf = crs_ncdf,
                           shape_file = shp_szyb, shape_index = "HYDROCODE",
                           time_range = c("1980-01-01", "1980-12-31"))






