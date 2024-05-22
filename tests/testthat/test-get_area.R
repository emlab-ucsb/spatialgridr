test_that("retrieving eez matches mregions2", {
  expect_equal(get_area(name = "Australia", type = "eez", country_type = "country"), mregions2::mrp_get("eez", cql_filter = "territory1 = 'Australia'"))
})

test_that("eez is sf object", {
  expect_s3_class(get_area(name = "France", type = "eez", country_type = "sovereign"), "sf")
})

test_that("12nm is sf object", {
  expect_s3_class(get_area(name = "United Kingdom", type = "12nm", country_type = "sovereign"), "sf")
})

test_that("oceans is sf object", {
  expect_s3_class(get_area(name = "Indian Ocean", type = "ocean", country_type = "sovereign"), "sf")
})

test_that("retrieving a country matches rnaturalearth", {
  expect_equal(get_area(name = "Australia", type = "countries", country_type = "country"), rnaturalearth::ne_countries(scale = 10, type = "countries", country = "Australia"))
})

test_that("country is sf object", {
  expect_s3_class(get_area(name = "France", type = "countries", country_type = "sovereign"), "sf")
})

test_that("bermuda example", {
  expect_equal(nrow(get_area("Bermuda", type = "eez", country_type = "country")), 1)
  })

test_that("kiribati example", {
  expect_equal(nrow(get_area(name = "Kiribati", type = "eez", country_type = "sovereign")),3)
})
