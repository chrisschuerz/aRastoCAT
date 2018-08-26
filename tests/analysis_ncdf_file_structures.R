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
ncdf_file <- unld_rr_path

unld_rr <- nc_open(unld_rr_path)

unld_data <- ncvar_get(unld_rr,"pr", start = c(1,1,100), count = c(-1,-1,1))
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




unld_ll <- fetch_latlon(unld_rr, NULL)
spat_ll <- fetch_latlon(spat_rr, NULL)
sprb_ll <- fetch_latlon(sprb_rr, NULL)
eobs_ll <- fetch_latlon(eobs_rr, NULL)
