library("rgdal")
library("raster")
library("ncdf4")
library("magrittr")
library("lubridate")
library("pracma")
library("dplyr")
library("tibble")
library("pasta")

# Angabe des Pfades zur netcdf Datei
setwd("H:/CLIM2POWER/DWD_Data/ToyDataSet_1980/day/tas/v20180221")
# Hier werden alle Daten im Verzeichnis ausgelesen,
#  die die Endung, die über pattern definiert ist, entsprechen - im Fall anpassen
files <- list.files(pattern = '\\.nc$')
ncdf_pth <- paste(getwd(), files[1], sep = "/")
print(ncdf_pth)

# Definition des Verzeichnises, in dem die Einzugsgebiete / Zonen als Shape-file vorliegen
setwd("H:/CLIM2POWER/DWD_Data/ZonalData/")
basin_shp <- readOGR(dsn = getwd(), "Raab_Basins_NB")
# Angabe des Spaltennamens der Zonen, für die die Werte aggregiert werden sollen
shp_index <- "NB"

ncdf_crs <- as.character(crs(basin_shp))

var_lbl <- "tas"
lat_lbl = "lat"
lon_lbl = "lon"
time_lbl = "time"

#aggregate_ncdf <- function(ncdf_pth, basin_shp, ncdf_crs, shp_index, var_lbl,
#                           lat_lbl = "lat", lon_lbl = "lon", time_lbl = "time") {

  # Load NCDF file ------------------------------------------------------
  ncin <- nc_open(filename = ncdf_pth)

  # Read variable and date from ncdf
  tmp_array <- ncvar_get(ncin,var_lbl)
  time <- ncvar_get(ncin, time_lbl)

  # Get initial time step
  t_0 <- ncatt_get(ncin,time_lbl,"units")$value %>%
    gsub("days since |seconds since ", "", .) %>%
    as.Date()

  nc_close(ncin)

  # Load lat/lon as raster and create polygon index layer from it -------
  lat <- raster(ncdf_pth, varname = lat_lbl)
  lon <- raster(ncdf_pth, varname = lon_lbl)


  #MODIFIED VERSION FOR SPATIAL POLYGON!!!
  lat <- as.matrix(lat)
  lon <- as.matrix(lon)

  # latlon <- cbind(lat, lon) %>% st_multipoint(x = .)
  # test <- st_voronoi(x = latlon)
  rst_dim <- dim(lat)
  rst_dim_corner <- rst_dim - 1

  lat_corner <- ((lat[1:(rst_dim[1] - 1), ] + lat[2:rst_dim[1], ])/2) %>%
    .[,2:rst_dim[2]]
  lon_corner <- ((lon[ ,1:(rst_dim[2] - 1)] + lon[ ,2:rst_dim[2]])/2) %>%
    .[2:rst_dim[1], ]
  latlon <- cbind(as.vector(lat_corner), as.vector(lon_corner))

  pnt_ind <- expand.grid(1:(rst_dim_corner[1] - 1), 1:(rst_dim_corner[2] - 1))

  pnt_list <- as.list(as.data.frame(t(pnt_ind)))

  extract_poly_coord <- function(ind, lat, lon){
    cbind(lat[c(ind[1], ind[1] + 1, ind[1] + 1, ind[1], ind[1])],
          lon[c(ind[1], ind[1], ind[1] + 1, ind[1] + 1, ind[1])])
  }

  poly_box <- map(pnt_list, extract_poly_coord, lat_corner, lon_corner)

  test <- st_polygon(x = poly_box) %>% st_sfc() %>% st_sf()



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
    #apply(., 2, rev) %>%
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
