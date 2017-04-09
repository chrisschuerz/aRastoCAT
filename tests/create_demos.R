library(rgdal)
library(pasta)
library(ncdf4)

bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/"
bnd_file_name = "demo_basin.shp"
basin_shp <- readOGR(paste(bnd_dir,bnd_file_name, sep = ""),
                     layer = "demo_basin")


ncdf_file <- "C:/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"

ncin <- nc_open(ncdf_file)

pr  <- ncvar_get(nc = ncin, varid = "pr")
pr_1 <- pr[20:35, 65:85, 1:365]
lon <- ncvar_get(nc = ncin, varid = "lon")
lon_1 <- lon[20:35, 65:85]
lat <- ncvar_get(nc = ncin, varid = "lat")
lat_1 <- lat[20:35, 65:85]
time <- ncvar_get(nc = ncin, varid = "time")
t_1 <- time[1:365]
t_un <- ncatt_get(ncin, "time")

# define dimensions
londim <- ncdim_def("x","degrees_east",as.double(lon_1[,1]))
latdim <- ncdim_def("y","degrees_north",as.double(lat_1[1,]))
timedim <- ncdim_def("time",t_un$units,as.double(t_1))

pr_def   <- ncvar_def(name = "pr", units = "mm", dim = list(londim,latdim,timedim),
                    missval = -999, longname = "precipitation", prec = "float")
lon_def  <- ncvar_def(name = "lon", units = "degrees_east", dim = list(londim,latdim),
                    missval = -999, longname = "longitude coordinate", prec = "double")
lat_def  <- ncvar_def(name = "lat", units = "degrees_north", dim = list(londim,latdim),
                    missval = -999, longname = "latitude coordinate", prec = "double")
time_def <- ncvar_def(name = "time_bnds", units = "days", dim = list(timedim),
                    missval = -999, longname = "time", prec = "double")

demo_nc <- nc_create("C:/demo_nc.nc", list(pr_def, lon_def, lat_def))
ncvar_put(demo_nc,pr_def,pr_1)
ncvar_put(demo_nc, lon_def, lon_1)
ncvar_put(demo_nc, lat_def, lat_1)
ncvar_put(demo_nc, time_def, t_1)

d_nc <- nc_open("C:/demo1_nc.nc")
nc_close(d_nc)

pr_d <- ncvar_get(d_nc, "pr")
