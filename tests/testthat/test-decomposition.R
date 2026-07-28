make_decomp_fixture <- function() {
  age <- 0:100
  lt1 <- build_life_table(
    data.frame(age = age, mx = 0.0004 * exp(0.075 * age)), "female"
  )
  lt2 <- build_life_table(
    data.frame(age = age, mx = 0.0003 * exp(0.070 * age)), "female"
  )
  list(lt1 = lt1, lt2 = lt2)
}

test_that("decompose_life_expectancy (arriaga) contributions sum to the e0 difference", {
  fx <- make_decomp_fixture()

  decomp <- decompose_life_expectancy(fx$lt1, fx$lt2, method = "arriaga")

  expect_equal(nrow(decomp), nrow(fx$lt1))
  expect_equal(sum(decomp$contribution), fx$lt2$ex[1] - fx$lt1$ex[1])
})

test_that("decompose_life_expectancy (pollard) contributions closely approximate the e0 difference", {
  fx <- make_decomp_fixture()

  decomp <- decompose_life_expectancy(fx$lt1, fx$lt2, method = "pollard")

  expect_equal(nrow(decomp), nrow(fx$lt1))
  # Pollard's formula is exact only in the continuous limit; on a 1x1
  # (single-year) life table, under this fixture's fast-rising Gompertz
  # hazard, the discretization error is a real few percent of the total
  # gap (empirically ~4% here) - not bit-identical to Arriaga's exact
  # sum, but still a reasonable approximation.
  true_diff <- fx$lt2$ex[1] - fx$lt1$ex[1]
  expect_equal(sum(decomp$contribution), true_diff, tolerance = 0.1 * true_diff)
})

test_that("arriaga and pollard decompositions broadly agree age-by-age", {
  fx <- make_decomp_fixture()

  arriaga <- decompose_life_expectancy(fx$lt1, fx$lt2, method = "arriaga")
  pollard <- decompose_life_expectancy(fx$lt1, fx$lt2, method = "pollard")

  # Per Ponnapalli (2005), Arriaga and Pollard decompositions are not
  # sensitive to the choice of method - their age patterns should agree
  # closely (empirically ~0.98 here) without being identical.
  expect_gt(stats::cor(arriaga$contribution, pollard$contribution), 0.95)
})

test_that("decompose_life_expectancy errors when ages don't match", {
  fx <- make_decomp_fixture()
  lt2_short <- fx$lt2[fx$lt2$age <= 90, ]

  expect_error(
    decompose_life_expectancy(fx$lt1, lt2_short),
    "same set of ages"
  )
})

test_that("decompose_life_expectancy defaults to arriaga", {
  fx <- make_decomp_fixture()

  default_result <- decompose_life_expectancy(fx$lt1, fx$lt2)
  arriaga_result <- decompose_life_expectancy(fx$lt1, fx$lt2, method = "arriaga")

  expect_equal(default_result, arriaga_result)
})

test_that("decompose_life_expectancy with identical life tables gives zero contribution everywhere", {
  fx <- make_decomp_fixture()

  decomp_arriaga <- decompose_life_expectancy(fx$lt1, fx$lt1, method = "arriaga")
  decomp_pollard <- decompose_life_expectancy(fx$lt1, fx$lt1, method = "pollard")

  expect_true(all(abs(decomp_arriaga$contribution) < 1e-8))
  expect_true(all(abs(decomp_pollard$contribution) < 1e-8))
})
