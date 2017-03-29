library(rgdal)
library(pasta)
library(raster)

sub_size <- c(30,54,4)
variable <- data.frame(var = c("pcp", "tmp"),
                       pth = c("RR",  "T2M"))

clim_inca <- list()

for(i_sub in sub_size){
  basin_pth <- "D:/UnLoadC3/00_RB_SWAT/raab_sb"%&%
               i_sub%//%
               "Watershed/Shapes/subs1.shp"
  basin_shp <- readOGR(basin_pth, layer = "subs1")

  clim_inca[["sb"%&%i_sub]] <- list()

  for(i_var in nrow(variable)){
    bin_pth <- "F:/mirror_H/ETP_AT/ETP_AT_Exe/input"%//%variable$pth[i_var]
    clim_inca[["sb"%&%i_sub]][[variable$var[i_var]]] <-
      aggregate_INCAbin(bin_pth = bin_pth,
                        basin_shp = basin_shp, bin_crs = crs(basin_shp),
                        bin_ext = c(99500, 700500, 249500, 600500),
                        shp_index = "Subbasin")
  }
}

save(clim_inca, file =  "D:/UnLoadC3/00_RB_SWAT/clim_inca")


