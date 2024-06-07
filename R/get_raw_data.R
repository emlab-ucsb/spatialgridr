#' Crop and mask/ intersect data
#'
#' @description
#' Called by `get_data_in_grid` when needed
#'
#' @param spatial_grid grid or `sf` polygon to crop/ mask/ intersect with
#' @param dat `terra::rast()` or `sf` data
#' @param meth `string` name of method to use for raster projection if data is raster
#' @param matching_crs `logical` TRUE if `spatial_grid` and `dat` have the same crs
#' @param antimeridian `logical` TRUE if cropping area crosses the antimeridian
#'
#' @return `terra::rast()` or `sf`
#' @noRd
get_raw_data <- function(spatial_grid, dat, meth, matching_crs, antimeridian){
  boundary <- boundary %>%
    sf::st_geometry() %>%
    sf::st_as_sf()

  if(check_raster(dat)){
    if(matching_crs){
      dat %>%
        terra::crop(sf::st_as_sf(boundary), mask = TRUE)
    }else{
      boundary %>%
        sf::st_transform(sf::st_crs(dat)) %>%
        sf::st_as_sf() %>%
        {if(antimeridian) terra::crop(terra::rotate(dat, left = FALSE), sf::st_shift_longitude(.)) else terra::crop(dat, .)} %>%
        terra::project(terra::crs(boundary), method = meth) %>%
        terra::mask(., boundary)
    }
  }else{
    if(matching_crs){
      dat %>%
        sf::st_intersection(sf::st_geometry(boundary)) %>%
        {if(antimeridian) sf::st_wrap_dateline(.) else .}

    }else{
      if(antimeridian){
        boundary %>%
          sf::st_transform(sf::st_crs(dat)) %>%
          sf::st_shift_longitude() %>%
          sf::st_intersection(dat %>% sf::st_shift_longitude()) %>%
          sf::st_wrap_dateline() %>%
          sf::st_transform(sf::st_crs(boundary))
      }else{
        boundary %>%
          sf::st_transform(sf::st_crs(dat)) %>%
          sf::st_intersection(dat, sf::st_geometry(.)) %>%
          sf::st_transform(sf::st_crs(boundary))
      }
    }
  }
}
