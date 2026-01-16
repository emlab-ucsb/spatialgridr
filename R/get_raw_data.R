#' Crop and mask/ intersect data
#'
#' @description
#' Called by `get_data_in_grid` when needed
#'
#' @param spatial_grid `sf` polygon to crop/ mask/ intersect data with
#' @param dat `terra::rast()` or `sf` data
#' @param matching_crs `logical` TRUE if `spatial_grid` and `dat` have the same crs
#' @param antimeridian `logical` TRUE if cropping area crosses the antimeridian
#'
#' @return `terra::rast()` or `sf`; same as `dat`
#' @noRd
get_raw_data <- function(spatial_grid, dat, matching_crs, antimeridian){

  if(check_raster(dat)){
    if(matching_crs){
      dat %>%
        terra::crop(., sf::st_as_sf(spatial_grid)) %>%
        terra::mask(., sf::st_as_sf(spatial_grid)) # separate step rather than mask = TRUE because that doesn't work well with antimeridian crossing sf objects
    }else{
      spatial_grid %>%
        sf::st_transform(sf::st_crs(dat)) %>%
        sf::st_as_sf() %>%
        {if(antimeridian) terra::crop(terra::rotate(dat), sf::st_shift_longitude(.)) else terra::crop(dat, .)} %>%
        terra::project(terra::crs(spatial_grid), method = "average") %>%
        terra::mask(., spatial_grid)
    }
  }else{
    if(matching_crs){
      dat %>%
        sf::st_intersection(sf::st_geometry(spatial_grid)) %>%
        {if(antimeridian) sf::st_wrap_dateline(.) else .}

    }else{
      if(antimeridian){
        spatial_grid %>%
          sf::st_transform(sf::st_crs(dat)) %>%
          sf::st_shift_longitude() %>%
          sf::st_intersection(dat %>% sf::st_shift_longitude()) %>%
          sf::st_wrap_dateline() %>%
          sf::st_transform(sf::st_crs(spatial_grid))
      }else{
        spatial_grid %>%
          sf::st_transform(sf::st_crs(dat)) %>%
          sf::st_intersection(dat, sf::st_geometry(.)) %>%
          sf::st_transform(sf::st_crs(spatial_grid))
      }
    }
  }
}
