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
sf_to_grid <- function(dat, spatial_grid, matching_crs, name, sf_col_layer_names, antimeridian, cov_fraction, return_cov_frac){

  #1st option: the sf data object is polygons showing a single feature presence or sf_col_layer_names is names of each habitat/ species/ other feature

  if(is.null(name)) name <- "data"

  if(is.null(sf_col_layer_names)){
    dat <- dat %>%
      sf::st_geometry() %>%
      sf::st_sf() %>%
      dplyr::mutate(sf_col_layer_names = 1, .before = 1)
  }  else {
    dat <- dat %>%
      dplyr::group_by(.data[[sf_col_layer_names]]) %>%
      dplyr::summarise() %>%
      dplyr::ungroup()
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
        #sf::st_union() %>%
        sf::st_sf()
    }else{
      dat_cropped <- if(matching_crs) dat %>% sf::st_crop(spatial_grid) else{spatial_grid %>%
          sf::st_transform(sf::st_crs(dat)) %>%
          sf::st_crop(dat, .) %>%
          sf::st_transform(sf::st_crs(spatial_grid))}

    }
    if(is.null(sf_col_layer_names)){
      presence_absence <- dat_cropped %>%
        sf::st_intersects(spatial_grid, .) %>%
        {lengths(.)>0} %>%
        as.integer()

      spatial_grid %>%
        dplyr::mutate("{name}" := presence_absence, .before = 1)
    }else{ #this is for gridding sf data with multiple features
      spatial_grid_with_id <- spatial_grid %>%
        dplyr::mutate(cellID = 1:nrow(.))

      spatial_grid_with_area <- spatial_grid_with_id %>%
        dplyr::mutate(area_cell = as.numeric(sf::st_area(.))) %>%
        sf::st_drop_geometry()

      layer_names <- dat_cropped[[1]]

      dat_list <- split(dat_cropped, layer_names)

      intersected_data_list <- lapply(layer_names, function(x) dat_list[[x]] %>%
                                        sf::st_intersection(spatial_grid_with_id, .) %>%
                                        dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
                                        sf::st_drop_geometry(.) %>%
                                        dplyr::full_join(spatial_grid_with_area, ., by = c("cellID")) %>%
                                        dplyr::mutate(perc_area = area / area_cell, .keep = "unused", .before = 1) %>%
                                        dplyr::mutate(perc_area = dplyr::case_when(is.na(perc_area) ~ 0,
                                                                                   .default = as.numeric(perc_area))) %>%
                                        dplyr::left_join(spatial_grid_with_id, .,  by = "cellID") %>%
                                        dplyr::select(!cellID) %>%
                                        {if(return_cov_frac) dplyr::select(., 1, {{x}} := 1) else {
                                          dplyr::mutate(.,
                                                        {{x}} := dplyr::case_when(perc_area >= cov_fraction  ~ 1,
                                                                                  .default = 0),
                                                        .before = 1
                                          ) %>%
                                            dplyr::select(1)
                                        }})

      lapply(intersected_data_list, sf::st_drop_geometry) %>%
        do.call(cbind, .) %>%
        sf::st_set_geometry(sf::st_geometry(intersected_data_list[[1]]))
    }
  }
}
