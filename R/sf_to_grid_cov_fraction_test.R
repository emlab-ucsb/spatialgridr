#expanding the usage cases for sf to raster and sf to sf grids to cover classified sf objects and continuous sf objects

data_sf <- sf::st_read(system.file("ex/lux.shp", package="terra"))

ras_grid <- terra::rast(data_sf, ncol = 75, nrow = 100)

sf_class_to_raster <- function(dat, spatial_grid, sf_col_name, cov_fraction){
  #group same polygons together so we don't end up with many polygons of the same variable name
  data_sf_dissolved <- dat %>%
    dplyr::group_by(.data[[sf_col_name]]) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()

  data_sf_dissolved_names <- data_sf_dissolved[[sf_col_name]]


 coverage_fractions_ras <- exactextractr::coverage_fraction(spatial_grid, data_sf_dissolved) |>
    terra::rast() |>
   setNames(data_sf_dissolved_names)

 thresholded_ras <- coverage_fractions_ras |>
   terra::classify(matrix(c(0, cov_fraction, NA, coverage_fraction_threshold, 1.2, 1), ncol = 3, byrow = TRUE), include.lowest = TRUE)

 #define minimum total cell coverage that should be used to force cell classification
 min_cov <- 0.95
 total_cell_coverage <- coverage_fractions_ras %>%
   sum(na.rm = TRUE) %>%
   terra::classify(matrix(c(0, min_cov, NA, min_cov, 1.2, 1), ncol = 3, byrow = TRUE), include.lowest = TRUE)


return(coverage_fractions_ras)
}

sf_to_raster_by_coverage_fraction <- sf_class_to_raster(dat = data_sf, spatial_grid = ras_grid, sf_col_name = "NAME_2", cov_fraction = 0.5)

terra::plot(sf_to_raster_by_coverage_fraction)

terra::plot(sum(sf_to_raster_by_coverage_fraction, na.rm = T))
terra::lines(vect(data_sf))


#currently, if multiple polygons intersect a cell and none of them hits the coverage fraction classification threshold, the cell ends up NA
#want to change this to chose the cell value with the highest coverage where there is almost total coverage by the polygons




#sf to raster for continuous sf objects - not needed

grouping_col <- "POP"

sf_to_raster_cont_values <- terra::asterize(vect(data_sf), r,  field = grouping_col, fun = 'mean', cover=T)

plot(sf_to_raster_cont_values, type = "classes")
lines(vect(data_sf))

#using a coverage approach -would be possible, but would need to create a raster layer for each value in the sf object, so would potentially be memory heavy
exactextractr::coverage_fraction(r, data_sf |> dplyr::select("POP")) |> rast() |> plot()

#sf to sf grid


sf_class_to_sf <- function(dat, spatial_grid, sf_col_name, cov_fraction){

  spatial_grid_with_id <- spatial_grid %>%
    dplyr::mutate(cellID = 1:nrow(.))


  spatial_grid_with_area <- spatial_grid_with_id %>%
    dplyr::mutate(area_cell = as.numeric(sf::st_area(.))) %>%
    sf::st_drop_geometry()

  dat_grouped <- dat %>%
    dplyr::group_by(.data[[sf_col_name]]) %>%
    dplyr::summarise() %>%
    dplyr::ungroup()

  layer_names <- dat_grouped[[1]]

  dat_list <- split(dat_grouped, layer_names)

  intersected_data_list <- lapply(layer_names, function(x) dat_list[[x]] %>%
                                    sf::st_intersection(spatial_grid_with_id, .) %>%
                                    dplyr::mutate(area = as.numeric(sf::st_area(.))) %>%
                                    sf::st_drop_geometry(.) %>%
                                    dplyr::full_join(spatial_grid_with_area, ., by = c("cellID")) %>%
                                    dplyr::mutate(perc_area = area / area_cell, .keep = "unused") %>%
                                    dplyr::left_join(spatial_grid_with_id, .,  by = "cellID") %>%
                                    dplyr::mutate(
                                      {{x}} := dplyr::case_when(perc_area >= cov_fraction  ~ 1,
                                                         .default = 0),
                                      .before = 1
                                    ) %>%
                                    dplyr::select(1))

  lapply(intersected_data_list, sf::st_drop_geometry) %>%
    do.call(cbind, .) %>%
    sf::st_set_geometry(sf::st_geometry(intersected_data_list[[1]]))

}

data_sf <- sf::st_read(system.file("ex/lux.shp", package="terra"))

r <- rast(data_sf, ncol = 75, nrow = 100)

#sf categorical to grid
sf_grid <- sf::st_as_sf(as.polygons(r, dissolve = FALSE))

sf_to_sf_by_coverage_fraction <- sf_class_to_sf(dat = data_sf, spatial_grid = sf_grid, sf_col_name = "NAME_2", cov_fraction = 0.5)

plot(sf_to_sf_by_coverage_fraction, border = F)

sf_to_sf_by_coverage_fraction %>%
  sf::st_drop_geometry() %>%
  rowSums() %>%
  as.data.frame() %>%
  sf::st_set_geometry(sf::st_geometry(sf_to_sf_by_coverage_fraction)) %>%
  plot()


#current method - all intersected cells are classified
presence_absence <- data_sf %>%
  sf::st_intersects(sf_grid, .) %>%
  {lengths(.)>0} %>%
  as.integer()

sf_grid %>%
  dplyr::mutate(presence := presence_absence, .before = 1) %>%
  plot()
