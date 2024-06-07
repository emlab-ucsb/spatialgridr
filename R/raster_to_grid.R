#' Internal helper function for gridding raster input data
#'
#' @description
#' Called from `get_data_in_grid` when needed
#'
#' @param spatial_grid `terra::rast()` or `sf` planning grid
#' @param dat `terra::rast()` input data
#' @param matching_crs `logical` TRUE if crs of data and planning grid match, else FASE
#' @param meth `string` name of method using for projecting/ resampling of raster, or gridding to sf
#' @param name `string` name of returned raster or if sf, column name in sf object
#' @param antimeridian `logical` TRUE if data to be gridded cross the antimeridian
#'
#' @return `terra::rast()` or `sf` gridded data, depending on `spatial_grid` format
#'
#' @noRd
ras_to_grid <- function(spatial_grid, dat, matching_crs, meth, name, antimeridian){

  if(is.null(name)) name <- names(dat)

  if(check_raster(spatial_grid)) {
    if(antimeridian){
      spatial_grid %>%
        terra::as.polygons() %>%
        sf::st_as_sf() %>%
        sf::st_transform(sf::st_crs(dat)) %>%
        sf::st_shift_longitude() %>%
        terra::crop(terra::rotate(dat, left = FALSE), .) %>%
        terra::project(spatial_grid, method = meth) %>%
        terra::mask(spatial_grid) %>%
        stats::setNames(name)
    }else{
      spatial_grid %>%
        {if(matching_crs) . else terra::as.polygons(.) %>% terra::project(terra::crs(dat))} %>%
            terra::crop(dat, .) %>%
            {if(matching_crs) terra::resample(., spatial_grid, method = meth) else terra::project(., spatial_grid, method = meth)} %>%
            terra::mask(spatial_grid) %>%
            stats::setNames(name)
    }
  } else {
    grid_has_extra_cols <- if(ncol(spatial_grid)>1) TRUE else FALSE

    if(grid_has_extra_cols) extra_cols <- sf::st_drop_geometry(spatial_grid)


    temp_grid <- if(matching_crs){
      spatial_grid %>%
        sf::st_geometry()
    }  else {
      spatial_grid %>%
        sf::st_geometry() %>%
        sf::st_transform(sf::st_crs(dat)) %>%
        {if(antimeridian) sf::st_shift_longitude(.) else .}
    }

    dat %>%
      {if(antimeridian) terra::rotate(., left = FALSE) else .}%>%
      exactextractr::exact_extract(temp_grid, meth , force_df = TRUE) %>%
      stats::setNames(name) %>%
      data.frame(temp_grid, .) %>%
      sf::st_sf() %>%
      {if(matching_crs) . else sf::st_transform(., sf::st_crs(spatial_grid))} %>%
      {if(grid_has_extra_cols) cbind(., extra_cols) %>% dplyr::select(colnames(extra_cols), name) else .} %>%
      sf::st_set_geometry("geometry")
  }
}
