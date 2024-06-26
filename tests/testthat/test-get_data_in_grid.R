test_that("returns Samoa example of gridded data - raster", {
  expect_s4_class(suppressWarnings(get_data_in_grid(spatial_grid = get_grid(boundary = get_boundary(name = "Samoa", type = "eez", country_type = "country"),
                                                    crs = '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                                    resolution = 10000),
                        dat = readRDS(system.file("extdata", "ridges.rds", package = "spatialgridr")))),
                        class = "SpatRaster")

})

test_that("returns Samoa example of raw data  - sf", {
  expect_s3_class(suppressWarnings(get_data_in_grid(spatial_grid = get_boundary(name = "Samoa", type = "eez", country_type = "country"),
                                                         dat = readRDS(system.file("extdata", "ridges.rds", package = "spatialgridr")),
                                                    raw = TRUE)),
                  class = "sf")

})


test_that("returns kiribati example (antimeridian example) of gridded data - raster", {
  expect_s4_class(suppressWarnings(get_data_in_grid(spatial_grid = get_grid(boundary = get_boundary(name = "Kiribati", type = "eez", country_type = "sovereign"),
                                                                                           crs = '+proj=laea +lon_0=-159.609375 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                                                                           resolution = 50000),
                                                         dat = terra::rast(system.file("extdata", "cold_coral.tif", package = "spatialgridr")),
                                                         antimeridian = TRUE)),
                  class = "SpatRaster")

})

test_that("returns kiribati example (antimeridian example) of raw data - sf", {
  expect_s3_class(suppressWarnings(get_data_in_grid(spatial_grid = get_boundary(name = "Kiribati", type = "eez", country_type = "sovereign"),
                                                         dat = readRDS(system.file("extdata", "ridges.rds", package = "spatialgridr")),
                                                    raw = TRUE,
                                                         antimeridian = TRUE)),
                  class = "sf")

})

test_that("returns samoa example of multi-column sf gridded data - sf", {
  expect_s3_class(suppressWarnings(get_data_in_grid(spatial_grid = get_grid(boundary = get_boundary(name = "Samoa", type = "eez", country_type = "country"),
                                                    crs = '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                                    resolution = 10000,
                                                    output = "sf_square"),
                                                    dat = readRDS(system.file("extdata", "abyssal_plains.rds", package = "spatialgridr")),
                                                    feature_names = "Class")),
                  class = "sf")

})
