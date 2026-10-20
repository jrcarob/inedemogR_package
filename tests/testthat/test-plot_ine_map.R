test_that("plot_ine_map highlights matching regions", {
  raw_sf <- sf::st_sf(
    GEOID = c("14021", "41091", "28079"),
    NAME = c("Córdoba", "Sevilla", "Madrid"),
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2))
    ),
    crs = 25830
  )
  mockery::stub(plot_ine_map, "get_ine_geo", raw_sf)

  p <- plot_ine_map(geo_level = "province", highlight = "^Córdoba$")

  expect_s3_class(p, "ggplot")
  expect_equal(p$data$highlighted, c(TRUE, FALSE, FALSE))
})

test_that("plot_ine_map warns when highlight matches nothing", {
  raw_sf <- sf::st_sf(
    GEOID = c("41091"),
    NAME = c("Sevilla"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0))),
    crs = 25830
  )
  mockery::stub(plot_ine_map, "get_ine_geo", raw_sf)

  expect_warning(
    plot_ine_map(geo_level = "province", highlight = "^Nowhere$"),
    "did not match"
  )
})

test_that("plot_ine_map with no highlight leaves everything unhighlighted", {
  raw_sf <- sf::st_sf(
    GEOID = c("41091", "28079"),
    NAME = c("Sevilla", "Madrid"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),
    crs = 25830
  )
  mockery::stub(plot_ine_map, "get_ine_geo", raw_sf)

  p <- plot_ine_map(geo_level = "province")

  expect_equal(p$data$highlighted, c(FALSE, FALSE))
})
