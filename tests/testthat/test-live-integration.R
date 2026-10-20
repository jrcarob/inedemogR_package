# Live integration test: the full mortality and fertility pipeline against the
# INE API. Opt-in (it downloads several MB and depends on INE availability):
# set INEDEMOGR_LIVE_TESTS=true. Never runs on CRAN. Deterministic numerical
# checks live in the other test files and do not need the network.

test_that("full pipeline runs end to end on live INE data", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("INEDEMOGR_LIVE_TESTS"), "true"), "set INEDEMOGR_LIVE_TESTS=true")
  skip_if_offline("servicios.ine.es")

  pop <- suppressWarnings(get_ine_population(n_periods = 3))
  deaths <- suppressWarnings(get_ine_deaths(n_periods = 3))
  births_age <- suppressWarnings(get_ine_births_by_age(n_periods = 3))
  expect_false(is.null(attr(pop, "provenance")))

  exposure <- suppressWarnings(compute_exposure(pop$data, deaths$data_provinces))
  rates <- suppressWarnings(compute_death_rates(deaths$data_provinces, exposure$data))
  lt <- suppressWarnings(build_life_tables(rates$mx_1x1))

  expect_equal(length(unique(lt$fltper$nuts3_code)), 52)
  expect_true(all(lt$qc$passed))
  e0 <- life_expectancy_summary(lt$fltper, "female")$e0
  expect_true(all(e0 > 75 & e0 < 95))

  asfr <- suppressWarnings(age_specific_fertility_rate(births_age$data, pop$data))
  tfr <- total_fertility_rate(asfr)$tfr
  expect_true(all(tfr > 0.5 & tfr < 3))
})
