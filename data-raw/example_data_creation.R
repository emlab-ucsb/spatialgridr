#creating small datasets for use in package examples

#retrieve Kiribati EEZ from marineregions - to have an EEZ that crosses the antimeridian

kir_eez <- mregions2::mrp_get("eez", cql_filter = "iso_ter1 = 'KIR'") |>
  sf::st_geometry() |>
  sf::st_sfc()

#retrieve Samoan EEZ - small EEZ that doesn't cross the antimeridian but almost within the same extent as Kiribati

samoa_eez <- mregions2::mrp_get("eez", cql_filter = "territory1 = 'Samoa'") |>
  sf::st_geometry() |>
  sf::st_sfc()

#get polygon covering extent of both EEZs
poly_extent <- sf::st_union(sf::st_geometry(kir_eez), sf::st_geometry(samoa_eez))

#get knolls base polygon extent
system.file("extdata/geomorphology", "Ridges.rds", package = "oceandatr", mustWork = TRUE) |>
  readRDS() |>
  sf::st_geometry() |>
  sf::st_crop(poly_extent) |>
  sf::st_intersection(poly_extent) |>
  saveRDS("inst/extdata/ridges.rds")

#get some coral data
