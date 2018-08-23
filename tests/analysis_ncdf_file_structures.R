#-------------------------------------------------------------------------------
# Evaluation of different ncdf formats and fixes for aRastoCAT
#-------------------------------------------------------------------------------
library(ncdf4)
library(tidyverse)
library(lubridate)

rotate_cc <- function(mtr) { mtr %>% t(.) %>% apply(., 2, rev)}

# Open different available ncdf files and do initial check of structure:
## Spartacus_AT
spat_rr_path <- "D:/MetData/Spartacus_AT/RR/RR1961.nc"

spat_rr <- nc_open(spat_rr_path)

spat_data <- ncvar_get(spat_rr,"RR") %>%
  array_branch(., margin = 3) %>%
  map(.,  rotate_cc)

spat_lat <- ncvar_get(spat_rr,"lat") %>%
  rotate_cc(.)

spat_lon <- ncvar_get(spat_rr,"lon") %>%
  rotate_cc(.)

time <- ncvar_get(spat_rr, "time")
t_0 <- ncatt_get(spat_rr, "time","units")$value %>%
  gsub("[[:alpha:]]", "", .) %>%
  as_date(.)
## Spartacus_AT contains lat/lon in wgs84 matrix OK!, date date vector OK!

## Spartacus_Raab
sprb_rr_path <- "D:/MetData/Spartacus_Raab/RR/RR_19610101.nc"

sprb_rr <- nc_open(sprb_rr_path)

sprb_data <- ncvar_get(sprb_rr,"RR") %>%
  array_branch(., margin = 3) %>%
  map(.,  rotate_cc)

sprb_lat <- ncvar_get(sprb_rr,"y") %>%
  rotate_cc(.)

sprb_lon <- ncvar_get(sprb_rr,"x") %>%
  rotate_cc(.)

sprb_time <- ncvar_get(sprb_rr, "time")
sprb_t_0 <- ncatt_get(sprb_rr, "time","units")$value %>%
  gsub("[[:alpha:]]", "", .) %>%
  as_date(.)
## Spartacus_Raab has only one date, no lat lon and onedimensional array
## Requires modification!!!
## Idea: Get a lot of info with text mining
a <- capture.output(spat_rr) # and get out required text chunks!

## EOBS
eobs_rr_path <- "D:/MetData/EOBS/rr_0.25deg_reg_v17.0.nc"

eobs_rr <- nc_open(eobs_rr_path)

eobs_data <- ncvar_get(eobs_rr,"RR") %>%
  array_branch(., margin = 3) %>%
  map(.,  rotate_cc)

eobs_lat <- ncvar_get(eobs_rr,"y") %>%
  rotate_cc(.)

eobs_lon <- ncvar_get(eobs_rr,"x") %>%
  rotate_cc(.)

eobs_time <- ncvar_get(eobs_rr, "time")
eobs_t_0 <- ncatt_get(eobs_rr, "time","units")$value %>%
  gsub("[[:alpha:]]", "", .) %>%
  as_date(.)
## EOBS has no lat/lon matrices but latitude longitude dimensions

## UnLoad
unld_rr_path <- "D:/MetData/UnLoadC3/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"

unld_rr <- nc_open(unld_rr_path)

unld_data <- ncvar_get(unld_rr,"RR") %>%
  array_branch(., margin = 3) %>%
  map(.,  rotate_cc)

unld_lat <- ncvar_get(unld_rr,"lat") %>%
  rotate_cc(.)

unld_lon <- ncvar_get(unld_rr,"x") %>%
  rotate_cc(.)

unld_time <- ncvar_get(unld_rr, "time")
unld_t_0 <- ncatt_get(unld_rr, "time","units")$value %>%
  gsub("[[:alpha:]]", "", .) %>%
  as_date(.)


find_latlon <- function(nc_file) {
  var_name <- names(nc_file$var)
  long_name <- map(nc_file$var, ~.x$longname)
  var_name_lat <- var_name[grepl(pattern = "latitude", tolower(long_name))]
  var_name_lon <- var_name[grepl(pattern = "longitude", tolower(long_name))]
  if(xor(length(var_name_lat) == 0, length(var_name_lon) == 0)) {
    stop("Only one of the two variables 'lat'/'lon' identified. Please check manually!")
  }
  if(length(var_name_lat) == 0) {
    return(NULL)
    warning("No 'lat'/'lon' variables identified. Using 'x' and 'y' dimensions instead."%&%
            "If 'lat'/'lon' variables should exist, define them manually!")
  } else {
    return(c(var_name_lat, var_name_lon))
  }
}

find_latlon(spat_rr)
find_latlon(sprb_rr)
find_latlon(eobs_rr)
find_latlon(unld_rr)

fetch_latlon <- function(nc_file, latlon_name) {
  if(is.null(latlon_name)){
    latlon_name <- find_latlon(nc_file)
    if(is.null(latlon_name)){
      xy_names <- names(nc_file$dim)[names(nc_file$dim) %in% c("x", "y")]
      if(length(xy_names) != 2){
        stop("Finding 'x' and 'y' dimensions was not successful")
      }
      dim_y <- ncvar_get(nc_file, "y")
      dim_x <- ncvar_get(nc_file, "x")
      lat <- matrix(rep(dim_y, length(dim_x)),
                    nrow = length(dim_y),
                    ncol = length(dim_x))
      lon <- matrix(rep(dim_x, length(dim_y)),
                    nrow = length(dim_x),
                    ncol = length(dim_y)) %>%
        rotate_cc(.)
    } else {
      lat <- ncvar_get(nc_file,latlon_name[1]) %>%
        rotate_cc(.)

      lon <- ncvar_get(nc_file, latlon_name[2]) %>%
        rotate_cc(.)
    }

  } else {
    lat <- ncvar_get(nc_file,latlon_name[1]) %>%
      rotate_cc(.)

    lon <- ncvar_get(nc_file, latlon_name[2]) %>%
      rotate_cc(.)
  }
  return(list(lat = lat, lon = lon))
}

unld_ll <- fetch_latlon(unld_rr, NULL)
spat_ll <- fetch_latlon(spat_rr, NULL)
sprb_ll <- fetch_latlon(sprb_rr, NULL)
eobs_ll <- fetch_latlon(eobs_rr, NULL)
