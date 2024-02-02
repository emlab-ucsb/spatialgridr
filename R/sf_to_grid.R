#' Internal helper function for gridding sf input data
#'
#' @description
#' Called from `get_data_in_grid` when needed
#'
#' @param dat `terra::rast()` input data
#' @param spatial_grid `terra::rast()` or `sf` planning grid
#' @param matching_crs `logical` TRUE if crs of data and planning grid match, else FASE
#' @param name `string` name of returned raster or if sf, column name in sf object
#' @param sf_col_layer_names `string` names of columns in sf data that will be gridded
#' @param antimeridian `logical` TRUE if data to be gridded cross the antimeridian
#'
#' @return `terra::rast()` or `sf` gridded data, depending on `spatial_grid` format
#' @noRd
sf_to_grid <- function(dat, spatial_grid, matching_crs, name, sf_col_layer_names, antimeridian){

  #1st option: the sf data object is polygons showing a single feature presence or sf_col_layer_names is names of each habitat/ species/ other feature

  if(is.null(name)) name <- "data"

  if(is.null(sf_col_layer_names)){
    dat <- dat %>%
      sf::st_geometry() %>%
      sf::st_sf() %>%
      dplyr::mutate(sf_col_layer_names = 1, .before = 1)
  }  else {
    dat <- dat %>%
      dplyr::select(dplyr::all_of(sf_col_layer_names))
  }

  if(check_raster(spatial_grid)){

    if(matching_crs) dat_cropped <- dat else{
      p_grid <- spatial_grid %>%
        terra::as.polygons() %>%
        sf::st_as_sf() %>%
        sf::st_transform(sf::st_crs(dat)) %>%
        {if(antimeridian) sf::st_shift_longitude(.) else .}

      dat_cropped <- dat %>%
        {if(antimeridian & !unique(sf::st_geometry_type(.)) %in% c("POINT", "MULTIPOINT")) {
          sf::st_break_antimeridian(., lon_0 = 180) %>% sf::st_shift_longitude()}
          else if(antimeridian & unique(sf::st_geometry_type(.)) %in% c("POINT", "MULTIPOINT")){
            sf::st_shift_longitude(.)
          } else .} %>%
        sf::st_crop(p_grid) %>%
        sf::st_transform(sf::st_crs(spatial_grid)) %>%
        {if(antimeridian) sf::st_union(.) %>% sf::st_sf() else .}
    }
      dat_cropped %>%
        terra::rasterize(spatial_grid, field = 1, by = sf_col_layer_names) %>%
        terra::mask(spatial_grid) %>%
        stats::setNames(name)

  } else{ #this is for sf planning grid output
    if(antimeridian & (sf::st_crs(dat) == sf::st_crs(4326))){
      p_grid <- spatial_grid %>%
        sf::st_geometry() %>%
        sf::st_transform(sf::st_crs(dat)) %>%
        sf::st_shift_longitude()

      dat_cropped <- dat %>%
        {if(!unique(sf::st_geometry_type(.)) %in% c("POINT", "MULTIPOINT")) sf::st_break_antimeridian(., lon_0 = 180) else .} %>%
        sf::st_shift_longitude() %>%
        sf::st_crop(p_grid) %>%
        sf::st_transform(sf::st_crs(spatial_grid)) %>%
        sf::st_union() %>%
        sf::st_sf()
    }else{
      dat_cropped <- if(matching_crs) dat %>% sf::st_crop(spatial_grid) else{spatial_grid %>%
          sf::st_transform(sf::st_crs(dat)) %>%
          sf::st_crop(dat, .) %>%
          sf::st_transform(sf::st_crs(spatial_grid))}

    }
    presence_absence <- dat_cropped %>%
      sf::st_intersects(spatial_grid, .) %>%
      {lengths(.)>0} %>%
      as.integer()

    spatial_grid %>%
      dplyr::mutate("{name}" := presence_absence, .before = 1)
  }
}
