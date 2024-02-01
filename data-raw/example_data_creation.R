#creating small datasets for use in package examples

library(magrittr)
#retrieve Kiribati EEZ from marineregions - to have an EEZ that crosses the antimeridian

kir_eez <- mregions2::mrp_get("eez", cql_filter = "iso_ter1 = 'KIR'") |>
  dplyr::select(sovereign1)

saveRDS(kir_eez, "inst/extdata/kir_eez.rds")

#retrieve Samoan EEZ - small EEZ that doesn't cross the antimeridian but almost within the same extent as Kiribati

samoa_eez <- mregions2::mrp_get("eez", cql_filter = "territory1 = 'Samoa'") |>
  dplyr::select(sovereign1)

saveRDS(samoa_eez,"inst/extdata/samoa_eez.rds")

#get polygon of both EEZs
poly_samoa_kir <- rbind(samoa_eez |> sf::st_cast(to = "MULTIPOLYGON"), kir_eez)

#get LHS of antimeridan polygon extent

lhs_polygon <- poly_samoa_kir |>
  sf::st_crop(xmin = 0, ymin = as.numeric(sf::st_bbox(poly_samoa_kir)$ymin), xmax = 180, ymax = as.numeric(sf::st_bbox(poly_samoa_kir)$ymax))

rhs_polygon <- poly_samoa_kir |>
  sf::st_crop(xmin = -180, ymin = as.numeric(sf::st_bbox(poly_samoa_kir)$ymin), xmax = 0, ymax = as.numeric(sf::st_bbox(poly_samoa_kir)$ymax))

#get knolls base polygon extent just for extent of the Samoa and Kiribati EEZs
knolls <- system.file("extdata/geomorphology", "Ridges.rds", package = "oceandatr", mustWork = TRUE) |>
  readRDS()

rbind(sf::st_crop(knolls, lhs_polygon), sf::st_crop(knolls, rhs_polygon)) |>
  saveRDS("inst/extdata/ridges.rds")

#get some coral data

system.file("extdata/binary_grid_figure7.tif", package = "oceandatr", mustWork = TRUE) |>
  terra::rast() |>
  terra::crop(poly_samoa_kir) |>
  terra::writeRaster("inst/extdata/cold_coral.tif", gdal = c("COMPRESS=ZSTD", "PREDICTOR=2", "ZSTD_LEVEL=22", "NUM_THREADS=10"), datatype = "INT1U")
