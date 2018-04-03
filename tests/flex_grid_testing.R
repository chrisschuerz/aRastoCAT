library("rgdal")
library("raster")
library("ncdf4")
library("magrittr")
library("lubridate")
library("pracma")
library("dplyr")
library("tibble")
library("pasta")
library(rgdal)
library(purrr)
library(sf)
library(shapefiles)
library(dplyr)

# Angabe des Pfades zur netcdf Datei
# setwd("H:/CLIM2POWER/DWD_Data/ToyDataSet_1980/day/tas/v20180221")
# Hier werden alle Daten im Verzeichnis ausgelesen,
#  die die Endung, die über pattern definiert ist, entsprechen - im Fall anpassen
# files <- list.files(pattern = '\\.nc$')
# ncdf_pth <- paste(getwd(), files[1], sep = "/")
# print(ncdf_pth)

# Definition des Verzeichnises, in dem die Einzugsgebiete / Zonen als Shape-file vorliegen
# setwd("H:/CLIM2POWER/DWD_Data/ZonalData/")

basin_shp <- "D:/Projects_R/ZonalData/Raab_Basins_NB.shp"

# Angabe des Spaltennamens der Zonen, für die die Werte aggregiert werden sollen
ncdf_pth <- "D:/Projects_R/tas.nc"
crs_grid <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

shp_index <- "NB"
var_lbl <- "tas"
lat_lbl = "lat"
lon_lbl = "lon"
time_lbl = "time"

ncdf_test <- aggregate_ncdf(ncdf_file = ncdf_pth, crs_ncdf = crs_grid,
                            shape_file = basin_shp, shape_index = shp_index,
                            var_label = var_lbl, lat_label = lat_lbl,
                            lon_label = lon_lbl, time_label = time_lbl)

  # Load NCDF file ------------------------------------------------------

ncin <- nc_open(filename = ncdf_pth)

# Read variable and date from ncdf
lat <- ncvar_get(ncin,lat_lbl) %>%
  t() %>%
  apply(., 2, rev)

lon <- ncvar_get(ncin,lon_lbl) %>%
  t() %>%
  apply(., 2, rev)

var_data <- ncvar_get(ncin,var_lbl) %>%
  array_branch(., margin = 3) %>%
  map(.,  function(array){array %>% t(.) %>% apply(., 2, rev)})

time <- ncvar_get(ncin, time_lbl)

# Get initial time step
t_0 <- ncatt_get(ncin,time_lbl,"units")$value %>%
  gsub("days since |seconds since ", "", .) %>%
  as.Date()

nc_close(ncin)

limit_lat <- function(lat, ext){
  lat_lw <- which(rowSums(lat < ext[3]) == ncol(lat)) %>% .[1]
  lat_up <- which(rowSums(lat > ext[4]) == ncol(lat)) %>% .[length(.)]
  lat_up:lat_lw
}

limit_lon <- function(lon, ext){
  lon_lf <- which(colSums(lon < ext[1]) == nrow(lon)) %>% .[length(.)]
  lon_rg <- which(colSums(lon > ext[2]) == nrow(lon)) %>% .[1]
  lon_lf:lon_rg
}

dim_init <- dim(lat)

iter_check <- TRUE
ind_lat_prev <- 0
ind_lon_prev <- 0

while(iter_check){
  ind_lat <- limit_lat(lat, ext_trans)
  ind_lon <- limit_lon(lon, ext_trans)

  if(all(dim(lat) == dim(init))){
    if(!all(ind_lat %in% (1:dim_init[1])) &
       !all(ind_lon %in% (1:dim_init[2]))){
      stop("Basin shape too close to any of the grid boundaries!")
    }
  }
  lat <- lat[ind_lat, ind_lon]
  lon <- lon[ind_lat, ind_lon]
  var_data <- map(var_data, function(mtr){mtr[ind_lat, ind_lon]})

  iter_check <-  ((sum(ind_lat) != sum(ind_lat_prev)) |
                  (sum(ind_lon) != sum(ind_lon_prev)))
  ind_lat_prev <- ind_lat
  ind_lon_prev <- ind_lon
}

var_data %<>%
  map(., as.vector) %>%
  do.call(cbind, .) %>%
  as_tibble() %>%
  set_colnames("timestep"%_%1:ncol(.)) %>%
  add_column(., idx = 1:nrow(.), .before = 1)


# latlon <- cbind(lat, lon) %>% st_multipoint(x = .)
# test <- st_voronoi(x = latlon)
rst_dim <- dim(lat)
rst_ind <- expand.grid(1:(rst_dim[1]), 1:(rst_dim[2]))
pnt_ind <- expand.grid(1:(rst_dim[1] - 1), 1:(rst_dim[2] - 1))

calc_corner <- function(ind, mtr) {
  mean(mtr[ind[1]:(ind[1] + 1), ind[2]:(ind[2] + 1)])}

extrapol_row <- function(mtr){
  n_row <- nrow(mtr)
  rbind(mtr[1, ] + (mtr[1, ] - mtr[2, ]),
        mtr,
        mtr[n_row, ] + (mtr[n_row, ] - mtr[n_row - 1, ]))
}

extrapol_col <- function(mtr){
  n_col <- ncol(mtr)
  cbind(mtr[ , 1] + (mtr[ , 1] - mtr[ , 2]),
        mtr,
        mtr[ , n_col] + (mtr[ , n_col] - mtr[, n_col - 1]))
}

lat_corner <- apply(pnt_ind, 1, calc_corner, lat) %>%
  matrix(., nrow = rst_dim[1] - 1, ncol = rst_dim[2] - 1) %>%
  extrapol_row(.) %>%
  extrapol_col(.)

lon_corner <- apply(pnt_ind, 1, calc_corner, lon) %>%
  matrix(., nrow = rst_dim[1] - 1, ncol = rst_dim[2] - 1) %>%
  extrapol_row(.) %>%
  extrapol_col(.)


ind_list <- as.list(as.data.frame(t(rst_ind)))

extract_poly_coord <- function(ind, lat, lon){
  cbind(c(lon[ind[1]    , ind[2]],
          lon[ind[1]    , ind[2] + 1],
          lon[ind[1] + 1, ind[2] + 1],
          lon[ind[1] + 1, ind[2]],
          lon[ind[1]    , ind[2]]),
        c(lat[ind[1]    , ind[2]],
          lat[ind[1]    , ind[2] + 1],
          lat[ind[1] + 1, ind[2] + 1],
          lat[ind[1] + 1, ind[2]],
          lat[ind[1]    , ind[2]]))
}

var_grid <- map(ind_list, extract_poly_coord, lat_corner, lon_corner) %>%
  map(., function(poly_i){st_polygon(x = list(poly_i), dim = "XY")}) %>%
  st_sfc(., crs = crs_grid) %>%
  st_sf(idx = 1:nrow(var_data), geometry = .) #var_data,

int <-
  st_intersection(basin_trans, var_grid) %>%
  as_tibble() %>%
  mutate(area = st_area(geoms)) %>%
  select(!!shp_index, idx, area) %>%
  rename(shp_index = !!shp_index) %>%
  group_by(shp_index) %>%
  mutate(area = area/sum(area)) %>%
  left_join(., var_data, by = "idx") %>%
  mutate_at(vars(starts_with("time")), funs(.*area)) %>%
  select(-idx, -area) %>%
  summarise_all(funs(sum)) %>%
  ungroup() %>%
  select(-shp_index) %>%
  t() %>%
  as_tibble() %>%
  set_colnames(shp_index%_%1:ncol(.)) %>%
  add_column(year = year(t_0 + time),
             mon  = month(t_0 + time),
             day  = day(t_0 + time),
             hour = hour(t_0 + time),
             min  = minute(t_0 + time),
             .before = 1)




################################################################################
################################################################################


rst_dim <- dim(lon) #x = horiz = lon / y = vert = lat
rst_len <- length(lon)

# Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])

# Specify the lonlat as spatial points with projection as long/lat
lonlat_proj <- SpatialPointsDataFrame(coords = lonlat,
                                      proj4string = CRS(ncdf_crs),
                                      data = data.frame(value = 1:length(lon))) %>%
SpatialPoints(coords = ., proj4string = CRS(ncdf_crs)) %>%
spTransform(., CRSobj = crs(basin_shp))

  # Derive lat/lon cell size of projected raster
  cell_size <- c(lonlat_proj@coords[1:rst_dim[2],1] %>% diff() %>% mean(),
                 subtract(lonlat_proj@coords[seq(1, rst_len - rst_dim[2], rst_dim[2]), 2],
                          lonlat_proj@coords[seq(1 + rst_dim[2], rst_len, rst_dim[2]), 2]) %>%
                   mean()
  )


  # Assign extent of point layer but adding cell extent
  ext <- extent(lonlat_proj)
  ext@xmin <- ext@xmin - cell_size[1]/2
  ext@xmax <- ext@xmax + cell_size[1]/2
  ext@ymin <- ext@ymin - cell_size[2]/2
  ext@ymax <- ext@ymax + cell_size[2]/2 # be careful here maybe other way round!!!

  # Create raster with cell indices as values and convert to spatial polygon
  assign_crs <- function(sp_obj, crs_new) {
    crs(sp_obj) <- crs_new
    return(sp_obj)
  }
  assign_ext <- function(sp_obj, ext_new) {
    extent(sp_obj) <- ext_new
    return(sp_obj)
  }

  idx_poly <- matrix(data = 1:rst_len, nrow = rst_dim[2]) %>%
    t() %>%
    apply(., 2, rev) %>%
    raster() %>%
    assign_crs(., crs(basin_shp)) %>%
    assign_ext(., ext) %>%
    as(., "SpatialPolygonsDataFrame")

  # Intersect index polygon with subbasin boundaries
  int_poly <- raster::intersect(idx_poly, basin_shp)

  # Extract data.frame with indices, subbasin number and pixel areas
  idx_area <- data.frame(area = sapply(int_poly@polygons,
                                       FUN=function(x) {slot(x, 'area')})) %>%
    cbind(int_poly@data) %>%
    mutate(area_fract = area/(cell_size[1]*cell_size[2])) %>%
    select(matches(shp_index), layer, area_fract) %>%
    set_colnames(c("basin", "idx", "fraction")) %>%
    mutate(idx = as.integer(idx)) %>%
    group_by(basin, idx) %>%
    summarise_all(funs(sum)) %>%
    ungroup()

  # Reduce 3D array to 2D matrix with row = idx, col = date
  tmp_df <- matrix(data = tmp_array, ncol = dim(tmp_array)[3]) %>%
    as.data.frame() %>%
    set_colnames("time"%_%1:dim(tmp_array)[3]) %>%
    mutate(idx = 1: rst_len)

  # Aggregate variable for subbasins
  idx_area <- left_join(idx_area, tmp_df, by = "idx") %>%
    mutate_at(vars(starts_with("time")), funs(.*fraction)) %>%
    select(-idx) %>%
    group_by(basin) %>%
    summarise_all(funs(sum)) %>%
    mutate_at(vars(starts_with("time")), funs(./fraction)) %>%
    select(-basin, -fraction) %>%
    t() %>%
    as_tibble() %>%
    set_colnames(shp_index%_%1:ncol(.)) %>%
    add_column(year = year(t_0 + time),
               mon  = month(t_0 + time),
               day  = day(t_0 + time),
               hour = hour(t_0 + time),
               min  = minute(t_0 + time),
               .before = 1)

#  return(idx_area)
#}
