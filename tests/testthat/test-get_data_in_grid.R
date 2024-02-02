test_that("returns Samoa example of gridded data - raster", {
  expect_s4_class(suppressWarnings(get_data_in_grid(spatial_grid = get_grid(area_polygon = readRDS(system.file("extdata", "samoa_eez.rds", package = "spatialgridr")),
                                                                            projection_crs = '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                                                            resolution = 5000),
                        dat = readRDS(system.file("extdata", "ridges.rds", package = "spatialgridr")))),
                        class = "SpatRaster")

})

test_that("returns Samoa example of gridded data  - sf", {
  expect_s3_class(suppressWarnings(get_data_in_grid(area_polygon = readRDS(system.file("extdata", "samoa_eez.rds", package = "spatialgridr")),
                                                         dat = readRDS(system.file("extdata", "ridges.rds", package = "spatialgridr")))),
                  class = "sf")

})


test_that("returns kiribati example (antimeridian example) of gridded data - raster", {
  expect_s4_class(suppressWarnings(get_data_in_grid(spatial_grid = get_grid(area_polygon = readRDS(system.file("extdata", "kir_eez.rds", package = "spatialgridr")),
                                                                                           projection_crs = '+proj=laea +lon_0=-159.609375 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                                                                           resolution = 5000),
                                                         dat = terra::rast(system.file("extdata", "cold_coral.tif", package = "spatialgridr")),
                                                         antimeridian = TRUE)),
                  class = "SpatRaster")

})

test_that("returns kiribati example (antimeridian example) of gridded data - sf", {
  expect_s3_class(suppressWarnings(get_data_in_grid(area_polygon = readRDS(system.file("extdata", "kir_eez.rds", package = "spatialgridr")),
                                                         dat = readRDS(system.file("extdata", "ridges.rds", package = "spatialgridr")),
                                                         antimeridian = TRUE)),
                  class = "sf")

})
