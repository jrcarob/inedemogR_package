test_that("list_ine_indicators returns the ine_variables reference table", {
  out <- list_ine_indicators()

  expect_true(tibble::is_tibble(out))
  expect_true(all(c("indicator", "name", "description") %in% names(out)))
  expect_true("population_total" %in% out$indicator)
})
