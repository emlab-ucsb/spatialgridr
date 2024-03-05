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

#sf to raster for continuous sf objects

grouping_col <- "POP"

sf_to_raster_cont_values <- rasterize(lux_sf, r,  field = grouping_col, fun = "mean")

plot(sf_to_raster_cont_values)

#using a coverage approach
exactextractr::coverage_fraction(r, lux_sf |> dplyr::select("POP"))
