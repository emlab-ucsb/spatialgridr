#' Internal helper function for gridding sf input data
#'
#' @description
#' Called from `get_data_in_grid` when needed
#'
#' @param dat `terra::rast()` input data
#' @param spatial_grid `terra::rast()` or `sf` planning grid
#' @param matching_crs `logical` TRUE if crs of data and planning grid match, else FASE
#' @param name `string` name of returned raster or if sf, column name in sf object
#' @param feature_names `string` names of columns in sf data that will be gridded
#' @param antimeridian `logical` TRUE if data to be gridded cross the antimeridian
#'
#' @return `terra::rast()` or `sf` gridded data, depending on `spatial_grid` format
#' @noRd
sf_to_grid <- function(dat, spatial_grid, matching_crs, name, feature_names, antimeridian, cutoff, apply_cutoff){

  is_raster <- check_raster(spatial_grid)

  if(matching_crs) {
    dat_cropped <- dat %>%
      sf::st_crop(spatial_grid)
  } else{
    grid_temp <- spatial_grid %>%
      {if(is_raster) terra::as.polygons(.) %>% sf::st_as_sf() else .} %>%
    sf::st_geometry() %>%
      sf::st_transform(sf::st_crs(dat)) %>%
      {if(antimeridian) sf::st_shift_longitude(.) else .}

    dat_cropped <- dat %>%
      {if(antimeridian & !unique(sf::st_geometry_type(.)) %in% c("POINT", "MULTIPOINT")) {
        sf::st_break_antimeridian(., lon_0 = 180) %>% sf::st_shift_longitude()}
        else if(antimeridian & unique(sf::st_geometry_type(.)) %in% c("POINT", "MULTIPOINT")){
          sf::st_shift_longitude(.)} else .} %>%
      sf::st_crop(grid_temp) %>%
      sf::st_transform(sf::st_crs(spatial_grid))
  }

  if(is.null(feature_names)){
    if(is.null(name)) name <- "data"

    dat_grouped <- dat_cropped %>%
      dplyr::mutate({{name}} := 1, .before = 1) %>%
      dplyr::group_by({{name}}) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      {if(sf::st_geometry_type(., by_geometry = FALSE) == "GEOMETRY") sf::st_cast(., to = "MULTIPOLYGON") else .}

  }  else {
    dat_grouped <- dat_cropped %>%
      dplyr::group_by(.data[[feature_names]]) %>%
      dplyr::summarise() %>%
      dplyr::ungroup() %>%
      {if(sf::st_geometry_type(., by_geometry = FALSE) == "GEOMETRY") sf::st_cast(., to = "MULTIPOLYGON") else .}
  }

  if(is_raster){
    nms <- dat_grouped[[1]]

    exactextractr::coverage_fraction(spatial_grid, dat_grouped) %>%
      terra::rast() %>%
      setNames(nms) %>%
      terra::mask(spatial_grid) %>%
      {if(apply_cutoff) terra::classify(., matrix(c(-1, cutoff, NA, cutoff, 1.2, 1), ncol = 3, byrow = TRUE), include.lowest = FALSE, right = FALSE) else .} %>%
      .[[lapply(., function(x) !all(terra::values(x) == 0)) %>% unlist()]] #removes all zero layers and by default also all NA layers
  } else{

    spatial_grid_with_id <- spatial_grid %>%
      dplyr::mutate(cellID = 1:nrow(.))

    spatial_grid_with_area <- spatial_grid_with_id %>%
      dplyr::mutate(area_cell = as.numeric(sf::st_area(.))) %>%
      sf::st_drop_geometry()

    layer_names <- if(is.null(feature_names)) name else dat_grouped[[1]]

    dat_list <- if(is.null(feature_names)) list(dat_grouped) %>% setNames(layer_names) else split(dat_grouped, layer_names)

    intersected_data_list <- suppressWarnings(
                                  lapply(layer_names, function(x) dat_list[[x]] %>%
                                      sf::st_intersection(spatial_grid_with_id, .) %>%
                                      dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
                                      sf::st_drop_geometry(.) %>%
                                      dplyr::full_join(spatial_grid_with_area, ., by = c("cellID")) %>%
                                      dplyr::mutate(perc_area = .data$area / .data$area_cell, .keep = "unused", .before = 1) %>%
                                      dplyr::mutate(perc_area = dplyr::case_when(is.na(.data$perc_area) ~ 0,
                                                                                 .default = as.numeric(.data$perc_area))) %>%
                                      dplyr::left_join(spatial_grid_with_id, .,  by = "cellID") %>%
                                      dplyr::select(!.data$cellID) %>%
                                      {if(!apply_cutoff) dplyr::select(., 1, {{x}} := 1) else {
                                        dplyr::mutate(.,
                                                      {{x}} := dplyr::case_when(.data$perc_area >= cutoff  ~ 1,
                                                                                .default = 0)
                                        ) %>%
                                          dplyr::select({{x}})
                                        }})
    )

    lapply(intersected_data_list, function(x) sf::st_drop_geometry(x) %>% dplyr::select(dplyr::where(~any(. != 0)))) %>%
      do.call(cbind, .) %>%
      sf::st_set_geometry(sf::st_geometry(intersected_data_list[[1]])) %>%
      sf::st_set_geometry("geometry")
  }
  }
