#' Aggregate binary INCA raster data for catchment subbasins
#'
#' @param bin_pth Path to folder where binary ('.bil') and header ('.hdr')
#'   files are located
#' @param basin_shp Shape file with the basin subunit polygons
#' @param bin_crs String providing the reference system of the binary files
#' @param bin_ext Vector of length four that provides xmin, xmax, ymin, ymax
#'   of the raster extent of the binaries
#' @param shp_index  Name of the column in the basin shapefile attribute
#'   table that provides the indices of the basin subunits
#'
#' @importFrom pasta %_% %//%
#' @importFrom dplyr mutate mutate_at select matches starts_with left_join
#'   vars funs group_by summarize_all
#' @importFrom tibble as_tibble add_column
#' @import     lubridate
#' @import foreach
#' @import doSNOW
#' @importFrom parallel makeCluster stopCluster
#' @importFrom magrittr %>% set_colnames subtract
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom raster raster rasterToPoints extent crs intersect
#'   removeTmpFiles
#' @importFrom sp SpatialPoints SpatialPointsDataFrame spTransform
#'   SpatialPolygonsDataFrame
#'
#' @return Returns tibble that provides the time series
#'   of the aggregated variable for the respective basin subunits
#' @export
#'
#' @examples
aggregate_INCAbin <- function(bin_pth, basin_shp, bin_crs, bin_ext, shp_index, n_core = 1) {

# Fetch header and binary files names from binary folder --------------
  hdr_lst <- list.files(path = bin_pth, pattern = ".hdr$")
  bil_lst <- list.files(path = bin_pth, pattern = ".bil$")

# Get bin meta data from first found header file ----------------------
  header <- read.table(file = bin_pth%//%hdr_lst[1], header = FALSE,
                       stringsAsFactors = FALSE)
  hdr_col <- header[,1]
  header %<>%
    .[,2] %>%
    as.numeric() %>%
    t() %>%
    as.data.frame() %>%
    set_colnames(hdr_col)

# Create raster with cell indices as values and convert to spatial polygon
  assign_crs <- function(sp_obj, crs_new) {
    crs(sp_obj) <- crs_new
    return(sp_obj)
  }
  assign_ext <- function(sp_obj, ext_new) {
    extent(sp_obj) <- ext_new
    return(sp_obj)
  }



  idx_poly <- matrix(data = 1:(header$NROWS * header$NCOLS),
                     nrow = header$NCOLS) %>%
    t() %>%
    raster() %>%
    assign_ext(., bin_ext) %>%
    assign_crs(., crs(bin_crs)) %>%
    as(., "SpatialPolygonsDataFrame") %>%
    spTransform(., CRSobj = crs(basin_shp))

# Intersect index polygon with subbasin boundaries
  int_poly <- intersect(idx_poly, basin_shp)

  # Extract data.frame with indices, subbasin number and pixel areas
  idx_area <- tibble(area = sapply(int_poly@polygons,
                                       FUN = function(x) {slot(x, 'area')})) %>%
    cbind(int_poly@data) %>%
    mutate(area_fract = area/(header$XDIM * header$YDIM)) %>%
    dplyr::select(matches(shp_index), layer, area_fract) %>%
    set_colnames(c("basin", "idx", "fraction")) %>%
    mutate(idx = as.integer(idx))


  # cl <- makeCluster(n_core)
  # registerDoSNOW(cl)
  #
  # print(paste("Aggregating", length(bil_lst), "binary files:"))
  # prgr_bar <- txtProgressBar(min = 0, max = length(bil_lst), initial = 0, style = 3)
  # progress <- function(n) setTxtProgressBar(prgr_bar, n)
  # opts <- list(progress = progress)

  aggregate_i <- function(bin_i, idx_area, header, bin_pth, shp_index){

  # sub_aggr <- foreach( i_bil = 1:length(bil_lst),
  #                      .packages = c("dplyr", "tibble", "lubridate", "magrittr", "pasta"),
  #                      .options.snow = opts) %dopar% {
    t_0 <- strsplit(bin_i, "\\_|\\.") %>%
      unlist() %>%
      .[2] %>%
      paste("00:00") %>%
      ymd_hm()
    t_step <- seq(24*3600/header$NBLOCKS, 24*3600, length.out = header$NBLOCKS)

    bil_i <- readBin(con = bin_pth%//%bin_i,
                     what = "numeric",
                     n = header$NROWS * header$NCOLS * header$NBLOCKS,
                     size = 4) %>%
      matrix(ncol = header$NBLOCKS) %>%
      as_tibble() %>%
      set_colnames("time"%_%1:header$NBLOCKS) %>%
      mutate(idx = 1:nrow(.))

    # Aggregate variable for subbasins
    aggr_i <- left_join(idx_area, bil_i, by = "idx") %>%
      mutate_at(vars(starts_with("time")), funs(.*fraction)) %>%
      dplyr::select(-idx) %>%
      group_by(basin) %>%
      summarise_all(funs(sum)) %>%
      mutate_at(vars(starts_with("time")), funs(./fraction)) %>%
      dplyr::select(-basin, -fraction) %>%
      t() %>%
      as_tibble() %>%
      set_colnames(shp_index%_%1:ncol(.)) %>%
      add_column(year = year(t_0 + t_step),
                 mon  = month(t_0 + t_step),
                 day  = day(t_0 + t_step),
                 hour = hour(t_0 + t_step),
                 min  = minute(t_0 + t_step),
                 .before = 1)

    return(aggr_i)
  }
  # stopCluster(cl)
  t1 <- system.time({
  sub_aggr <- lapply(bil_lst, aggregate_i, idx_area, header)

  })

  tpar <- system.time({
    cl <- makeCluster(4)
    clusterEvalQ(cl, {library(tibble); library(dplyr); library(lubridate); library(magrittr); library(pasta)})
    sub_aggr_par <- parLapply(cl, bil_lst, aggregate_i, idx_area, header, bin_pth, shp_index)
    stopCluster(cl)
  })

  sub_aggr <- bind_rows(sub_aggr)

}


# F:\mirror_H\Lehre\Masterarbeiten\mHmMur\InputDataGeneration
# F:\mirror_H\ETP_AT\ETP_AT_Exe\input


# # Testing -------------------------------------------------------------
# library(pasta)
# library(magrittr)
# library(dplyr)
# library(raster)
# library(tibble)
 # library(rgdal)
# library(doSNOW)
# library(parallel)
#
# bin_pth <- "F:/mirror_H/ETP_AT/ETP_AT_Exe/input/T2M"
# bin_ext <- c(99000, 700000, 250000, 601000)
# bin_crs <- crs(basin_shp)
# shp_index <- "Subbasin"
#
#
# mt <- array(bil_i, dim = c(header$NCOLS, header$NROWS, header$NBLOCKS))
#
# mt <- bil_i[1:(header$NCOLS*header$NROWS)]
#
# mt[,,1] %>% View
#   raster() %>%
#   plot
#
#   idx_rst <-  matrix(data = 1:(header$NROWS * header$NCOLS),
#                      nrow = header$NCOLS)
#   idx_rst[1:5, 1:5] <- 200000
#
#   idx_rst%<>%
#     t() %>%
#     raster() %>%
#       assign_ext(., bin_ext) %>%
#       assign_crs(., crs(bin_crs))
  #     projectRaster(crs = crs(basin_shp))


# NBLOCKS_OUT <- 24 # timesteps per day (leave unchanged)
# offset <- 500 #leave unchanged
#
# # INCA - Austria
# NROWS_INCA <- 351 #leave unchanged
# NCOLS_INCA <-  601#leave unchanged
#
# # INCA-Austria domain (small one); Numbers show the left, right, upper and lower coordinates of the domain; offset is included to position domain correctly, when exporting netCDF or ASCII
# X0_INCA <- 99500 + offset
# X1_INCA <- 699500 + offset
# Y0_INCA <- 600500 - offset
# Y1_INCA <- 250500 - offset


# Load basin boundary shape file --------------------------------------
# bnd_dir <- "D:/UnLoadC3/00_RB_SWAT/raab_sb4/Watershed/Shapes/"
# bnd_file_name = "subs1.shp"
# basin_shp <- readOGR(paste(bnd_dir,bnd_file_name, sep = ""),
#                    layer = "subs1")
#
#
# plot(idx_poly)
# plot(basin_shp, add = T)
