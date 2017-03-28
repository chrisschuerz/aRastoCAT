# Load basin boundary shape file for 4 subbasins ---------------------------
bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Watershed/Shapes/"
bnd_file_name = "subs1.shp"
basin_4 <- readOGR(paste(bnd_dir,bnd_file_name, sep = ""),
                     layer = "subs1")

bin_pth <- "F:/mirror_H/ETP_AT/ETP_AT_Exe/input"


obs_4 <- list()
t_T <- system.time({
   obs_4$tmp <- aggregate_INCAbin(bin_pth = bin_pth%//%"T2M",
                                  basin_shp = basin_4, bin_crs = crs(basin_4),
                                  bin_ext = c(99500, 700500, 249500, 600500),
                                  shp_index = "Subbasin")

})
t_T[3]/60

t_P <- system.time({
  obs_4$pcp <- aggregate_INCAbin(bin_pth = bin_pth%//%"RR",
                                 basin_shp = basin_4, bin_crs = crs(basin_4),
                                 bin_ext = c(99500, 700500, 249500, 600500),
                                 shp_index = "Subbasin")

})
t_P[3]/60


# Load basin boundary shape file for 30 subbasins --------------------------
bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/raab_30/Watershed/Shapes/"
bnd_file_name = "subs1.shp"
basin_30 <- readOGR(paste(bnd_dir,bnd_file_name, sep = ""),
                   layer = "subs1")


obs_30 <- list()
t_T <- system.time({
  obs_30$tmp <- aggregate_INCAbin(bin_pth = bin_pth%//%"T2M",
                                 basin_shp = basin_30, bin_crs = crs(basin_30),
                                 bin_ext = c(99500, 700500, 249500, 600500),
                                 shp_index = "Subbasin")

})
t_T[3]/60

t_P <- system.time({
  obs_30$pcp <- aggregate_INCAbin(bin_pth = bin_pth%//%"RR",
                                 basin_shp = basin_30, bin_crs = crs(basin_30),
                                 bin_ext = c(99500, 700500, 249500, 600500),
                                 shp_index = "Subbasin")

})
t_P[3]/60
