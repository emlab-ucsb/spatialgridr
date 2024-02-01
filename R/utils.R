# A file of little functions that we use across the board.

#' Check planning grid or area polygon input is supplied and is in correct format
#'
#' @param planning_grid sf or raster planning grid
#' @param area_polygon sf object
#'
#' @noRd
check_grid_or_polygon <- function(planning_grid, area_polygon) {
  if (is.null(area_polygon) & is.null(planning_grid)) {
    stop("an area polygon or planning grid must be supplied")
  } else if (!is.null(area_polygon) & !is.null(planning_grid)) {
    stop("please supply either an area polygon or a planning grid, not both")
  } else if (!is.null(planning_grid) &
             !(class(planning_grid)[1] %in% c("RasterLayer", "SpatRaster", "sf"))) {
    stop("planning_grid must be a raster or sf object")
  } else if (!is.null(area_polygon) &
             !(class(area_polygon)[1] == "sf")) {
    stop("area_polygon must be an sf object")
  }
}


#' Check if area polygon or planning grid crs is same as data crs
#'
#' @param area_polygon sf object
#' @param planning_grid raster or sf
#' @param dat raster or sf
#'
#' @return `logical` TRUE crs' match, FALSE if they don't
#' @noRd
check_matching_crs <- function(area_polygon, planning_grid, dat){
  if(is.null(planning_grid)){
    ifelse(sf::st_crs(area_polygon) == sf::st_crs(dat), TRUE, FALSE)
  }else{
    ifelse(sf::st_crs(planning_grid) == sf::st_crs(dat), TRUE, FALSE)
  }
}

#' Check if sf object spans the antimeridian
#'
#' @param sf_object
#'
#' @return `logical` TRUE if it does span the antimeridian, FALSE if it doesn't
#' @noRd
check_antimeridian <- function(sf_object){
  if(sf::st_crs(sf_object) != sf::st_crs(4326)){
    b_box <- sf::st_transform(sf_object, 4326) %>%
      sf::st_bbox()
  } else{
    b_box <- sf::st_bbox(sf_object)
  }

  if(round(b_box$xmin) == -180 & round(b_box$xmax) == 180){
    TRUE
  } else{
    FALSE
  }
}

#' Check if data is a raster
#'
#' @param dat
#'
#' @return `logical` TRUE if raster, else FALSE
#' @noRd
check_raster <- function(dat){
  if(class(dat)[1] %in% c("RasterLayer", "SpatRaster")){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Check if data is sf
#'
#' @param dat
#'
#' @return TRUE if sf, else FALSE
#' @noRd
check_sf <- function(dat){
  if(class(dat)[1] == "sf"){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' If input is character, read in from file pointed to, assuming it is a common vector or raster file format
#'
#' @param dat
#'
#' @return `sf` or `terra::rast` format data
#' @noRd
data_from_filepath <- function(dat){
  ## First deal with whether the input is a file or a dataset
  if (class(dat)[1] == "character") { # If a file, we need to load the data

    ext <- tools::file_ext(dat)
    nm <- basename(dat)
    if (ext %in% c("tif", "tiff", "grd", "gri")) {
      print("Data is in raster format")
      dat <- terra::rast(dat)
    } else if (ext %in% c("shp", "gpkg")) {
      print("Data is in vector format")
      dat <- sf::read_sf(dat)
    }
  }
  return(dat)
}
