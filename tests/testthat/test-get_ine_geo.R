test_that("get_ine_geo returns tidy municipality geometries", {
  raw_sf <- sf::st_sf(
    cpro = c("41", "28"),
    cmun = c("091", "079"),
    name = c("Sevilla", "Madrid"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),
    crs = 4326
  )
  mockery::stub(get_ine_geo, "mapSpain::esp_get_munic", raw_sf)

  out <- get_ine_geo(geo_level = "municipality")

  expect_true(inherits(out, "sf"))
  expect_true(all(c("GEOID", "NAME") %in% names(out)))
  expect_equal(out$GEOID, c("41091", "28079"))
  expect_equal(sf::st_crs(out)$epsg, 25830L)
})

test_that("get_ine_geo returns tidy province geometries and filters by region", {
  raw_sf <- sf::st_sf(
    cpro = c("41", "28"),
    ine.prov.name = c("Sevilla", "Madrid"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),
    crs = 4326
  )
  mockery::stub(get_ine_geo, "mapSpain::esp_get_prov", raw_sf)

  out <- get_ine_geo(geo_level = "province", region = "Sevilla")

  expect_equal(nrow(out), 1)
  expect_equal(out$GEOID, "41")
  expect_equal(out$NAME, "Sevilla")
})
