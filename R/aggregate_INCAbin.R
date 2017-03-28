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
#'   vars funs group_by ungroup summarise_all
#' @importFrom tibble as_tibble add_column
#' @importFrom lubridate year month day hour minute
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

aggregate_INCAbin <- function(bin_pth, basin_shp, bin_crs, bin_ext, shp_index) {

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
    mutate(idx = as.integer(idx)) %>%
    group_by(basin, idx) %>%
    summarise_all(funs(sum)) %>%
    ungroup()

  aggregate_i <- function(bin_i, idx_area, header, bin_pth, shp_index){
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
      select(-idx) %>%
      group_by(basin) %>%
      summarise_all(funs(sum)) %>%
      mutate_at(vars(starts_with("time")), funs(./fraction)) %>%
      select(-basin, -fraction) %>%
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

  sub_aggr <- lapply(bil_lst, aggregate_i, idx_area, header, bin_pth, shp_index)
  sub_aggr <- bind_rows(sub_aggr)

}
