test_that("compute_exposure does not duplicate rows across provinces sharing (year, age)", {
  # Regression test: split_lexis_triangles()'s no-cohort branch used to drop
  # nuts3_code and the reattachment join matched on (year, age) only, so
  # every province sharing a (year, age) cell multiplied together.
  population <- tibble::tibble(
    nuts3_code = rep(c("ES111", "ES112"), each = 2),
    province_name = rep(c("A Coruna", "Lugo"), each = 2),
    year = rep(c(2022, 2023), times = 2),
    age = 0,
    female = c(100, 110, 50, 55),
    male = c(105, 115, 52, 58),
    total = c(205, 225, 102, 113)
  )
  deaths <- tibble::tibble(
    nuts3_code = rep(c("ES111", "ES112"), each = 2),
    province_name = rep(c("A Coruna", "Lugo"), each = 2),
    year = rep(c(2022, 2023), times = 2),
    age = 0,
    female = c(1, 1, 0, 1),
    male = c(1, 2, 1, 0),
    total = c(2, 3, 1, 1)
  )

  result <- suppressWarnings(compute_exposure(population, deaths))

  expect_equal(nrow(result$data), nrow(population))
  dupes <- result$data |>
    dplyr::count(.data$nuts3_code, .data$year, .data$age) |>
    dplyr::filter(.data$n > 1)
  expect_equal(nrow(dupes), 0)

  es111_2022 <- result$data[result$data$nuts3_code == "ES111" & result$data$year == 2022, ]
  expect_equal(nrow(es111_2022), 1)
})

test_that("compute_exposure keeps each province's own death counts, not a pooled total", {
  population <- tibble::tibble(
    nuts3_code = rep(c("ES111", "ES112"), each = 2),
    province_name = rep(c("A Coruna", "Lugo"), each = 2),
    year = rep(c(2022, 2023), times = 2),
    age = 0,
    female = c(1000, 1000, 500, 500),
    male = c(1000, 1000, 500, 500),
    total = c(2000, 2000, 1000, 1000)
  )
  deaths <- tibble::tibble(
    nuts3_code = rep(c("ES111", "ES112"), each = 2),
    province_name = rep(c("A Coruna", "Lugo"), each = 2),
    year = rep(c(2022, 2023), times = 2),
    age = 0,
    female = c(10, 10, 4, 4),
    male = c(10, 10, 4, 4),
    total = c(20, 20, 8, 8)
  )

  result <- suppressWarnings(compute_exposure(population, deaths))

  es111 <- result$data[result$data$nuts3_code == "ES111" & result$data$year == 2022, ]
  es112 <- result$data[result$data$nuts3_code == "ES112" & result$data$year == 2022, ]
  # Exposure should reflect each province's own (small) death correction,
  # not a value contaminated by the other province's death counts.
  expect_true(es111$total != es112$total)
})

test_that("compute_exposure excludes years present in population but absent from deaths", {
  # Regression test: population commonly includes a most-recent year (e.g.
  # a Jan-1 stock snapshot) with no rows anywhere in `deaths` yet, since
  # death registration lags population estimates. Treating that year as
  # zero deaths (rather than missing) used to mechanically produce zero
  # mortality and a nonsensical inflated life expectancy downstream.
  population <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna",
    year = c(2023, 2024, 2025), age = 0,
    female = c(100, 105, 110), male = c(105, 110, 115), total = c(205, 215, 225)
  )
  deaths <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna",
    year = c(2023, 2024), age = 0,
    female = c(1, 1), male = c(1, 2), total = c(2, 3)
  )

  result <- suppressWarnings(compute_exposure(population, deaths))

  expect_equal(sort(unique(result$data$year)), c(2023, 2024))
  # 2024's exposure still uses 2025's population as the P(x,t+1) boundary,
  # so it should differ from a plain average of 2023/2024 population.
  exp_2024 <- result$data$total[result$data$year == 2024]
  expect_true(exp_2024 != mean(c(215, 215)))
})
