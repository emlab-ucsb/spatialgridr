test_that("return grid for Samoa - raster", {
  expect_s4_class(get_grid(area_polygon = readRDS(system.file("extdata", "samoa_eez.rds", package = "spatialgridr")),
                                    projection_crs = '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                    resolution = 5000),
                  class = "SpatRaster")
})

test_that("return grid for Samoa - sf square", {
  expect_s3_class(get_grid(area_polygon = readRDS(system.file("extdata", "samoa_eez.rds", package = "spatialgridr")),
                                    projection_crs = '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                    option = "sf_square",
                                    resolution = 5000),
                  class = "sf")
})

test_that("return grid for Samoa - sf hex", {
  expect_s3_class(get_grid(area_polygon = readRDS(system.file("extdata", "samoa_eez.rds", package = "spatialgridr")),
                                    projection_crs = '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                    option = "sf_hex",
                                    resolution = 5000),
                  class = "sf")
})

test_that("return grid for kiribati - raster", {
  expect_s4_class(get_grid(area_polygon =  readRDS(system.file("extdata", "kir_eez.rds", package = "spatialgridr")),
                                    projection_crs = '+proj=laea +lon_0=-159.609375 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                    resolution = 5000),
                  class = "SpatRaster")
})



test_that("return planning grid for kiribati - sf square", {
  expect_s3_class(get_grid(area_polygon =  readRDS(system.file("extdata", "kir_eez.rds", package = "spatialgridr")),
                                    projection_crs = '+proj=laea +lon_0=-159.609375 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                    option = "sf_square",
                                    resolution = 5000),
                  class = "sf")
})



test_that("return planning grid for kiribati - raster", {
  expect_s3_class(get_grid(area_polygon =  readRDS(system.file("extdata", "kir_eez.rds", package = "spatialgridr")),
                                    projection_crs = '+proj=laea +lon_0=-159.609375 +lat_0=0 +datum=WGS84 +units=m +no_defs',
                                    option = "sf_hex",
                                    resolution = 5000),
                  class = "sf")
})
