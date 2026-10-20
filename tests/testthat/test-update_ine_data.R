test_that("update_ine_data caches and skips already-current indicators", {
  cache_dir <- withr::local_tempdir()

  resp <- data.frame(
    Municipios = "Sevilla", Municipios.Codigo = "41091",
    Provincias = "Sevilla", Provincias.Codigo = "41",
    Sexo = "Total", Anyo = 2023, Valor = 100
  )
  mockery::stub(update_ine_data, "get_ine_demog", function(indicator, ...) {
    tibble::tibble(GEOID = "41091", NAME = "Sevilla", year = 2023, value = 100)
  })

  latest_years <- update_ine_data(data_dir = cache_dir)

  wired <- sum(!is.na(ine_variables$id_table))
  expect_equal(length(latest_years), wired)
  expect_true(file.exists(file.path(cache_dir, "population_total.rds")))

  # second call with same data should report up-to-date, not re-download
  msgs <- testthat::capture_messages(update_ine_data(data_dir = cache_dir))
  expect_true(any(grepl("up-to-date", msgs)))
})

test_that("update_ine_data force re-downloads even when current", {
  cache_dir <- withr::local_tempdir()
  mockery::stub(update_ine_data, "get_ine_demog", function(indicator, ...) {
    tibble::tibble(GEOID = "41091", NAME = "Sevilla", year = 2023, value = 100)
  })

  update_ine_data(data_dir = cache_dir)
  msgs <- testthat::capture_messages(update_ine_data(data_dir = cache_dir, force = TRUE))
  expect_true(any(grepl("Updated", msgs)))
})
