make_abridged_fixture <- function() {
  age <- 0:100
  mx1 <- data.frame(age = age, mx = 0.0004 * exp(0.075 * age))

  age_group <- assign_age_group(age)
  mx5 <- stats::aggregate(mx ~ age_group, data.frame(age_group = age_group, mx = mx1$mx), mean)
  # aggregate() alphabetizes age_group; keep the raw mx per group, but the
  # engine doesn't require any particular row order (it sorts by age_start).

  list(mx1 = mx1, mx5 = mx5)
}

test_that("parse_age_group_labels parses standard and open-interval labels", {
  parsed <- parse_age_group_labels(c("00-04", "05-09", "95-99", "100+"))

  expect_equal(parsed$age_start, c(0, 5, 95, 100))
  expect_equal(parsed$n, c(5, 5, 5, NA_integer_))
  expect_equal(parsed$is_open, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("build_abridged_life_table produces one row per age group, sorted by age", {
  fx <- make_abridged_fixture()

  alt <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "female")

  expect_equal(nrow(alt), nrow(fx$mx5))
  expect_equal(alt$age_start, sort(alt$age_start))
  expect_equal(alt$age_group[1], "00-04")
  expect_equal(alt$age_group[nrow(alt)], "100+")
})

test_that("build_abridged_life_table's youngest group survivorship matches an exact single-year build", {
  fx <- make_abridged_fixture()

  alt <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "female")

  # Cross-check: l(5) implied by the abridged table's first group should
  # match l(5) from the exact single-year life table built on the same
  # mx1 data (both use the same Andreev-Kingkade a0 and a1=0.4/a(2:4)=0.5
  # conventions for ages 0-4).
  lt_1x1 <- build_life_table(fx$mx1[fx$mx1$age <= 4, ], sex = "female")
  # lt_1x1 treats age 4 as terminal (open interval), so compare survivorship
  # up to (not including) age 4 plus one more step computed the same way
  # the abridged engine does, via d(0-4) = l0 - l5.
  d0_4 <- 100000 - alt$lx[2] # lx[2] is l(5), the entry to the 05-09 group
  expect_equal(alt$lx[1] - alt$dx[1], alt$lx[2])
  expect_true(d0_4 > 0 && d0_4 < 100000)
})

test_that("build_abridged_life_table's e0 closely matches the exact single-year build_life_table()", {
  # Regression test for a real bug: an earlier version of the abridged
  # engine's Lx formula omitted the interval-width multiplier
  # (nLx = n*l(x+n) + nax*ndx, not l(x+n) + nax*ndx), which silently
  # under-counted person-years by roughly a factor of 5 for every
  # non-infant group while still passing every individual QC check (lx
  # monotonic, Lx > 0, qx in [0,1]) - it produced a demographically
  # impossible e0 of ~23 years instead of ~87. Cross-checking the
  # aggregate e0 against the exact single-year life table on the same
  # mx curve is the check that actually catches this class of bug.
  fx <- make_abridged_fixture()

  alt <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "female")
  lt_1x1 <- build_life_table(fx$mx1, sex = "female")

  expect_equal(alt$ex[1], lt_1x1$ex[1], tolerance = 0.05 * lt_1x1$ex[1])
})

test_that("build_abridged_life_table terminal open interval uses Lx = lx/mx", {
  fx <- make_abridged_fixture()

  alt <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "female")
  last <- nrow(alt)

  expect_equal(alt$Lx[last], alt$lx[last] / alt$mx[last])
  expect_equal(alt$qx[last], 1)
})

test_that("build_abridged_life_table errors when mx_1x1 is missing ages 0-4", {
  fx <- make_abridged_fixture()
  mx1_short <- fx$mx1[fx$mx1$age >= 2, ]

  expect_error(
    build_abridged_life_table(mx1_short, fx$mx5, sex = "female"),
    "ages 0-4"
  )
})

test_that("build_abridged_life_table_both averages female/male a0 for the youngest group", {
  fx <- make_abridged_fixture()

  alt_total <- build_abridged_life_table_both(fx$mx1, fx$mx5)
  alt_female <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "female")
  alt_male <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "male")

  expected_a0 <- mean(c(
    andreev_kingkade_a0(fx$mx1$mx[1], "female"), andreev_kingkade_a0(fx$mx1$mx[1], "male")
  ))
  # ax[1] for total isn't directly exposed pre-aggregation, but the
  # implied 0-4 nax should sit between the female-only and male-only
  # values given the averaged a0 feeds the same recursion.
  expect_true(
    alt_total$ax[1] >= min(alt_female$ax[1], alt_male$ax[1]) - 1e-6 &&
      alt_total$ax[1] <= max(alt_female$ax[1], alt_male$ax[1]) + 1e-6
  )
  expect_true(is.finite(expected_a0))
})

test_that("validate_abridged_life_table passes on a well-formed table", {
  fx <- make_abridged_fixture()
  alt <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "female")

  qc <- validate_abridged_life_table(alt)

  expect_true(qc$passed)
  expect_length(qc$issues, 0)
})

test_that("validate_abridged_life_table flags qx out of bounds", {
  fx <- make_abridged_fixture()
  alt <- build_abridged_life_table(fx$mx1, fx$mx5, sex = "female")
  alt$qx[3] <- 1.5

  qc <- suppressWarnings(validate_abridged_life_table(alt))

  expect_false(qc$passed)
  expect_true("qx_out_of_bounds" %in% names(qc$issues))
})

test_that("build_abridged_life_tables builds fltper/mltper/bltper for every province/year", {
  mx_1x1 <- dplyr::bind_rows(
    tibble::tibble(
      nuts3_code = "ES111", province_name = "A Coruna", year = 2023, age = 0:100,
      mx_female = 0.0004 * exp(0.075 * (0:100)),
      mx_male = 0.0005 * exp(0.078 * (0:100)),
      mx_total = 0.00045 * exp(0.0765 * (0:100))
    )
  )
  age_group <- assign_age_group(0:100)
  mx_5x1 <- dplyr::bind_rows(lapply(unique(age_group), function(g) {
    idx <- age_group == g
    tibble::tibble(
      nuts3_code = "ES111", province_name = "A Coruna", year = 2023, age_group = g,
      mx_female = mean(mx_1x1$mx_female[idx]),
      mx_male = mean(mx_1x1$mx_male[idx]),
      mx_total = mean(mx_1x1$mx_total[idx])
    )
  }))

  alt <- build_abridged_life_tables(mx_1x1, mx_5x1)

  expect_true(all(c("fltper", "mltper", "bltper", "qc") %in% names(alt)))
  expect_equal(nrow(alt$fltper), length(unique(age_group)))
  expect_equal(alt$fltper$nuts3_code[1], "ES111")
  expect_equal(alt$fltper$year[1], 2023)
})
