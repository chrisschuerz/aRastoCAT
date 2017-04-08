library(rgdal)
library(pasta)
library(ncdf4)

ncdf_file <- "C:/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"

ncin <- nc_open(ncdf_file)

pr  <- ncvar_get(nc = ncin, varid = "pr")
pr_1 <- pr[30:90,20:80,1:90]
lon <- ncvar_get(nc = ncin, varid = "lon")
lat <- ncvar_get(nc = ncin, varid = "lat")
time <- ncvar_get(nc = ncin, varid = "time")
t_1 <- time[1:365]
t_un <- ncatt_get(ncin, "time")



ncdf_file <- "C:/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon[,1]))
latdim <- ncdim_def("lat","degrees_north",as.double(lat[1,]))
timedim <- ncdim_def("time",t_un$units,as.double(t_1))

pr_def <- ncvar_def(name = "pr", units = "mm", dim = list(londim,latdim,timedim),
                    missval = -999, longname = "Precipitation")

