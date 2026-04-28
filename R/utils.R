# A file of little functions that we use across the board.

#' Check a spatial grid is supplied and in raster or sf format
#'
#' @param spatial_grid
#'
#' @noRd
check_grid <- function(spatial_grid) {
    checkmate::assert_multi_class(spatial_grid, c("SpatRaster", "sf"))
}

#' Check if area spatial objects have same crs
#'
#' @param sp1 raster or sf
#' @param sp2 raster or sf
#'
#' @return `logical` TRUE crs' match, FALSE if they don't
#' @noRd
check_matching_crs <- function(sp1, sp2){
    ifelse(sf::st_crs(sp1) == sf::st_crs(sp2), TRUE, FALSE)
}

#' Check if sf object spans the antimeridian
#'
#' @param sf_object
#'
#' @return `logical` TRUE if it does span the antimeridian, FALSE if it doesn't
#' @noRd
check_antimeridian <- function(sf_object, dat){
  if(sf::st_crs(sf_object) != sf::st_crs(4326)){
    b_box <- sf::st_transform(sf_object, 4326) %>%
      sf::st_bbox()
  } else{
    b_box <- sf::st_bbox(sf_object)
  }

  if(round(b_box$xmin) == -180 & round(b_box$xmax) == 180 & sf::st_crs(dat) == sf::st_crs(4326)){
    TRUE
  } else if (round(b_box$xmin) == -180 & round(b_box$xmax) == 180 & sf::st_crs(dat) != sf::st_crs(4326)){
    message("Your area polygon or grid crosses the antimeridian, but your data are not in long-lat (EPSG 4326) format. This may result in problems when cropping and gridding data, if the data are not in a suitable local projection.")
    FALSE
  } else FALSE
}

#' If input is character, read in from file pointed to, assuming it is a common vector or raster file format
#'
#' @param dat
#'
#' @return `sf` or `terra::rast` format data
#' @noRd
data_from_filepath <- function(dat){

    ext <- tools::file_ext(dat)

    if (ext %in% c("tif", "tiff", "grd", "gri")) {
      print("Data is in raster format")
      terra::rast(dat)
    } else if (ext %in% c("shp", "gpkg")) {
      print("Data is in vector format")
      sf::read_sf(dat)
    } else
      stop("File does not appear to be in one of the common spatial data formats, try reading it directly using e.g. `terra::rast()` or `sf::st_read()`")
}
