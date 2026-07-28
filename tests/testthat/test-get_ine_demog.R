fake_ine_response <- function(geo_var, geo_names, geo_codes, sex, values, year) {
  df <- data.frame(
    Anyo = year,
    Valor = values,
    Sexo = sex,
    stringsAsFactors = FALSE
  )
  df[[geo_var]] <- geo_names
  df[[paste0(geo_var, ".Codigo")]] <- geo_codes
  df
}

test_that("get_ine_demog returns a tidy tibble for a single indicator", {
  resp <- fake_ine_response(
    "Municipios", c("Sevilla", "Madrid"), c("41091", "28079"),
    "Total", c(688711, 3223334), 2023
  )
  mockery::stub(get_ine_demog, "ineapir::get_data_table", resp)

  out <- get_ine_demog(indicator = "population_total", year = 2023)

  expect_true(tibble::is_tibble(out))
  expect_true(all(c("GEOID", "NAME", "year", "population_total") %in% names(out)))
  expect_equal(nrow(out), 2)
  expect_equal(out$population_total[out$NAME == "Sevilla"], 688711)
})

test_that("get_ine_demog joins two indicators sharing a geo_level", {
  resp <- fake_ine_response(
    "Provincias", c("Sevilla", "Madrid"), c("41", "28"),
    "Total", c(15000, 30000), 2023
  )
  mockery::stub(get_ine_demog, "ineapir::get_data_table", resp)

  out <- get_ine_demog(indicator = c("births_total", "deaths_total"), year = 2023)

  expect_true(all(c("births_total", "deaths_total") %in% names(out)))
  expect_equal(nrow(out), 2)
})

test_that("get_ine_demog rejects mismatched geo levels across indicators", {
  expect_error(
    get_ine_demog(indicator = c("population_total", "births_total")),
    "different geographic levels"
  )
})

test_that("get_ine_demog rejects unknown indicators", {
  expect_error(
    get_ine_demog(indicator = "not_a_real_indicator"),
    "Unknown indicator"
  )
})

test_that("get_ine_demog requires an indicator", {
  expect_error(get_ine_demog(indicator = character(0)))
})

test_that("get_ine_demog filters by region", {
  resp <- fake_ine_response(
    "Municipios", c("Sevilla", "Madrid"), c("41091", "28079"),
    "Total", c(688711, 3223334), 2023
  )
  mockery::stub(get_ine_demog, "ineapir::get_data_table", resp)

  out <- get_ine_demog(indicator = "population_total", year = 2023, region = "Sevilla")

  expect_equal(nrow(out), 1)
  expect_equal(out$NAME, "Sevilla")
})

test_that("get_ine_demog(geometry = TRUE) joins geometries from get_ine_geo", {
  resp <- fake_ine_response(
    "Municipios", "Sevilla", "41091", "Total", 688711, 2023
  )
  mockery::stub(get_ine_demog, "ineapir::get_data_table", resp)

  geo_sf <- sf::st_sf(
    GEOID = "41091",
    NAME = "Sevilla",
    geometry = sf::st_sfc(sf::st_point(c(0, 0))),
    crs = 25830
  )
  mockery::stub(get_ine_demog, "get_ine_geo", geo_sf)

  out_sf <- get_ine_demog(indicator = "population_total", year = 2023, geometry = TRUE)

  expect_true(inherits(out_sf, "sf"))
  expect_true("population_total" %in% names(out_sf))
  expect_equal(nrow(out_sf), 1)
})
