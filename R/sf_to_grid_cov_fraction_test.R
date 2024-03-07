library(terra)
library(dplyr)

#expanding the usage cases for sf to raster and sf to sf grids to cover classified sf objects and continuous sf objects

lux_sf <- sf::st_read(system.file("ex/lux.shp", package="terra"))

r <- rast(lux_sf, ncol = 75, nrow = 100)

#classified sf object to raster grid

grouping_col <- "NAME_1"

#group same polygons together so we don't end up with many polygons of the same variable name
lux_sf_dissolved <- lux_sf %>%
  dplyr::group_by(.data[[grouping_col]]) %>%
  dplyr::summarise()

lux_sf_dissolved_names <- lux_sf_dissolved[[grouping_col]]

#min % coverage of raster cell by sf polygon for it to be classified as that type
coverage_fraction_threshold <- 0.5

sf_to_raster_by_coverage_fraction <- exactextractr::coverage_fraction(r, lux_sf_dissolved) |> rast() |> classify(matrix(c(0, coverage_fraction_threshold, NA, coverage_fraction_threshold, 1, 1), ncol = 3, byrow = TRUE), include.lowest = TRUE) |> setNames(lux_sf_dissolved_names)

plot(sf_to_raster_by_coverage_fraction)

plot(sum(sf_to_raster_by_coverage_fraction, na.rm = T))

#sf to raster for continuous sf objects - not needed

grouping_col <- "POP"

sf_to_raster_cont_values <- rasterize(vect(lux_sf), r,  field = grouping_col, fun = 'mean', cover=T)

plot(sf_to_raster_cont_values, type = "classes")
lines(vect(lux_sf))

#using a coverage approach -would be possible, but would need to create a raster layer for each value in the sf object, so would potentially be memory heavy
exactextractr::coverage_fraction(r, lux_sf |> dplyr::select("POP")) |> rast() |> plot()

#sf to sf grid

#sf categorical to grid
sf_grid <- sf::st_as_sf(as.polygons(r, dissolve = FALSE))

#current method
presence_absence <- lux_sf %>%
  sf::st_intersects(sf_grid, .) %>%
  {lengths(.)>0} %>%
  as.integer()

sf_grid %>%
  dplyr::mutate(presence := presence_absence, .before = 1) %>%
  plot()

#new approach

grouping_col <- "NAME_2"

sf_grid_id <- sf_grid %>%
  dplyr::mutate(cellID = 1:nrow(.))

intersected_data <- lux_sf %>%
  dplyr::group_by(.data[[grouping_col]]) %>%
  dplyr::summarise() %>%
  split(seq(nrow(.))) %>%
  lapply(function(x) sf::st_intersection(sf_grid_id, x))

lux_sf_dummy <- lux_sf %>%
  dplyr::select(dplyr::all_of(grouping_col)) %>%
  model.matrix(formula(paste("~", grouping_col, "-1")), .) %>%
  as.data.frame() %>%
  setNames(lux_sf[[grouping_col]]) %>%
  sf::st_set_geometry(sf::st_geometry(lux_sf))

grid_data_intersection <- sf_grid_id %>%
  sf::st_intersection(lux_sf_dummy)


#first try:
grid_data_intersection <- sf_grid %>%
  dplyr::mutate(cellID = 1:nrow(.)) %>%
  sf::st_intersection(lux_sf %>% select(NAME_2))

plot(grid_data_intersection)

shared_area <- grid_data_intersection %>%
  mutate(shared_area = sf::st_area(sf::st_geometry(.)) %>% as.numeric(), .before = 1)

grid_cell_areas <- sf_grid %>%
  dplyr::mutate(cellID = 1:nrow(.)) %>%
  mutate(area_cell = sf::st_area(sf::st_geometry(.)) %>% as.numeric(), .before = 1)

grid_data_coverage <- grid_cell_areas %>%
  dplyr::full_join(shared_area %>% sf::st_drop_geometry(), by = dplyr::join_by(cellID)) %>%
  mutate(area_overlap = shared_area/area_cell, .keep = "unused")

plot(grid_data_coverage["area_overlap"])
