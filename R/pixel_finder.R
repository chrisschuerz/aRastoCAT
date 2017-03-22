library(ncdf4)
library(raster)
library(maps)
library(magrittr)


# Load NCDF file ------------------------------------------------------
# ncdf_files <- list.files("G:/UnLoadC3", full.names = TRUE)
# FOr testing one file on C:/
ncdf_files <- "C:/pr_bc_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_AT_EZG_1971-2000.nc"
ncin <- nc_open(filename = ncdf_files[1])


# Load basin boundary shape file --------------------------------------
bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Watershed/Shapes/"
bnd_file_name = "subs1.shp"
bnd_shp <- readOGR(paste(bnd_dir,bnd_file_name, sep = ""),
                   layer = "subs1")

# Load lat/lon as raster ----------------------------------------------
lat <- raster(ncdf_files[1], varname="lat")
lon <- raster(ncdf_files[1], varname="lon")
rst_dim <- dim(lon) #x = horiz = lon / y = vert = lat
rst_len <- length(lon)

# Convert to points and match the lat and lons
plat <- rasterToPoints(lat)
plon <- rasterToPoints(lon)
lonlat <- cbind(plon[,3], plat[,3])

# Specify the lonlat as spatial points with projection as long/lat
lonlat <- SpatialPointsDataFrame(coords = lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"), data = data.frame(value = 1:length(lon)))
lonlat <- SpatialPoints(coords = lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
lonlat_proj <- spTransform(lonlat, CRSobj = crs(bnd_shp))

# Derive lat/lon cell size of projected raster
cell_size <- c(plonlat@coords[1:rst_dim[2],1] %>% diff() %>% mean(),
               subtract(plonlat@coords[seq(1, rst_len - rst_dim[2], rst_dim[2]), 2],
                        plonlat@coords[seq(1 + rst_dim[2], rst_len, rst_dim[2]), 2]) %>%
                 mean()
)

# Create raster with cell indices as values
idx_rst <- matrix(data = 1:rst_len, nrow = rst_dim[1]) %>%
  raster()

# Assign extent of point layer but adding cell extent
ext <- extent(plonlat)
ext@xmin <- ext@xmin - cell_size[2]/2
ext@xmax <- ext@xmax + cell_size[2]/2
ext@ymin <- ext@ymin - cell_size[1]/2
ext@ymax <- ext@ymax + cell_size[1]/2 # be careful here maybe other way round!!!
extent(idx_rst) <- ext

# assign crs of catchment boundary
crs(idx_rst) <- crs(bnd_shp)

# Convert index raster to spatial polygon
idx_poly <- as(idx_rst, "SpatialPolygonsDataFrame")

# Intersect index polygon with subbasin boundaries
int_poly <- intersect(idx_poly, bnd_shp)
idx_area <- data.frame(area = sapply(int_poly@polygons, FUN=function(x) {slot(x, 'area')}))

# Extract data.frame with indices, subbasin number and pixel areas
bsn_idx <-

  idx_area <- cbind(int_poly@data, idx_area) %>%
  select()




plot(idx_poly)
plot(bnd_shp, add = T)
areaSpatialGrid(plonlat)
plonlat@coords[1:126,1] %>% diff() %>% mean()

plg_df <- SpatialPolygonsDataFrame(pxl, data.frame(cell = 1:length(lon)), match.ID=F)

plonlat@coords[seq(1,nrow(plonlat@coords), rst_dim[2]),2] - plonlat@coords[seq(1 + rst_dim[2],nrow(plonlat@coords), rst_dim[2]),2]
