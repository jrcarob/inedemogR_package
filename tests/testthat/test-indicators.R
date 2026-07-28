make_pop_fixture <- function() {
  tibble::tibble(
    nuts3_code = "ES111",
    province_name = "A Coruna",
    year = 2023,
    age = c(0, 10, 14, 15, 40, 64, 65, 80),
    female = c(10, 10, 10, 20, 20, 20, 5, 5),
    male = c(10, 10, 10, 20, 20, 20, 5, 5)
  )
}

test_that("age_dependency_ratio computes youth/old/total ratios per province-year", {
  pop <- make_pop_fixture() |> dplyr::mutate(total = .data$female + .data$male)

  result <- age_dependency_ratio(pop)

  # young: age <= 14 -> ages 0,10,14 -> total 20+20+20 = 60
  # working: 14 < age < 65 -> ages 15,40,64 -> total 40+40+40 = 120
  # old: age >= 65 -> ages 65,80 -> total 10+10 = 20
  expect_equal(result$youth_dependency_ratio, 60 / 120 * 100)
  expect_equal(result$old_age_dependency_ratio, 20 / 120 * 100)
  expect_equal(result$total_dependency_ratio, (60 + 20) / 120 * 100)
})

test_that("age_dependency_ratio treats age == young_max as young and age == old_min as old", {
  # Boundary check: age 14 must count as young (not working), age 65 must
  # count as old (not working).
  pop <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(14, 15, 64, 65), total = c(100, 100, 100, 100)
  )

  result <- age_dependency_ratio(pop)

  expect_equal(result$youth_dependency_ratio, 100 / 200 * 100)
  expect_equal(result$old_age_dependency_ratio, 100 / 200 * 100)
})

test_that("aging_index computes elderly-per-100-children", {
  pop <- make_pop_fixture() |> dplyr::mutate(total = .data$female + .data$male)

  result <- aging_index(pop)

  expect_equal(result$aging_index, 20 / 60 * 100)
})

test_that("sex_ratio aggregates over age by default", {
  pop <- make_pop_fixture()

  result <- sex_ratio(pop)

  expect_equal(nrow(result), 1)
  expect_equal(result$sex_ratio, 80 / 80 * 100)
})

test_that("sex_ratio keeps age when by_age = TRUE", {
  pop <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(0, 1), female = c(10, 10), male = c(20, 5)
  )

  result <- sex_ratio(pop, by_age = TRUE)

  expect_equal(nrow(result), 2)
  expect_equal(result$sex_ratio, c(200, 50))
})

test_that("birth_death_ratio computes births/deaths per geography-year", {
  df <- tibble::tibble(
    GEOID = c("15", "28"), NAME = c("A Coruna", "Madrid"), year = 2023,
    births_total = c(3000, 30000), deaths_total = c(6000, 25000)
  )

  result <- birth_death_ratio(df)

  expect_equal(result$birth_death_ratio, c(0.5, 1.2))
  expect_equal(result$GEOID, c("15", "28"))
})

test_that("crude_birth_rate joins births and population and computes per-1000 rate", {
  births <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023, total = 100
  )
  pop <- tibble::tibble(
    nuts3_code = "ES111", year = 2023, age = c(0, 1), total = c(5000, 5000)
  )

  result <- crude_birth_rate(births, pop)

  expect_equal(result$cbr, 100 / 10000 * 1000)
})

test_that("general_fertility_rate divides births by reproductive-age women only", {
  births <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023, total = 100
  )
  pop <- tibble::tibble(
    nuts3_code = "ES111", year = 2023, age = c(10, 25, 40, 60),
    female = c(2000, 2500, 2500, 3000)
  )

  result <- general_fertility_rate(births, pop)

  # Only ages 25 and 40 fall in the default 15-49 window; age 10 and 60
  # (and their female counts) must be excluded from the denominator.
  expect_equal(result$gfr, 100 / (2500 + 2500) * 1000)
})

test_that("general_fertility_rate respects custom age_min/age_max", {
  births <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023, total = 100
  )
  pop <- tibble::tibble(
    nuts3_code = "ES111", year = 2023, age = c(10, 25, 40, 60),
    female = c(2000, 2500, 2500, 3000)
  )

  result <- general_fertility_rate(births, pop, age_min = 20, age_max = 44)

  expect_equal(result$gfr, 100 / (2500 + 2500) * 1000)
})

test_that("infant_mortality_rate divides age-0 deaths by births in the same year", {
  deaths <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(0, 1, 65), total = c(3, 1, 40)
  )
  births <- tibble::tibble(nuts3_code = "ES111", year = 2023, total = 1000)

  result <- infant_mortality_rate(deaths, births)

  # Only age 0 (3 deaths) counts as infant mortality; ages 1 and 65 must
  # be excluded from the numerator.
  expect_equal(result$imr, 3 / 1000 * 1000)
})

test_that("crude_death_rate sums deaths over age and computes per-1000 rate", {
  deaths <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(0, 1), total = c(1, 49)
  )
  pop <- tibble::tibble(
    nuts3_code = "ES111", year = 2023, age = c(0, 1), total = c(5000, 5000)
  )

  result <- crude_death_rate(deaths, pop)

  expect_equal(result$cdr, 50 / 10000 * 1000)
})

test_that("rate_of_natural_increase computes cbr - cdr per province-year", {
  cbr <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023, cbr = 6.5
  )
  cdr <- tibble::tibble(nuts3_code = "ES111", year = 2023, cdr = 12.3)

  result <- rate_of_natural_increase(cbr, cdr)

  expect_equal(result$rni, 6.5 - 12.3)
  expect_equal(result$province_name, "A Coruna")
})

test_that("life_expectancy_summary extracts e0 and e65 from a life table", {
  lt <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(0, 65), ex = c(86.97, 20.5)
  )

  result <- life_expectancy_summary(lt, sex = "female")

  expect_equal(result$e0, 86.97)
  expect_equal(result$e65, 20.5)
  expect_equal(result$sex, "female")
})

make_lifetable_mx_fixture <- function() {
  # A flat/low mx up to a low terminal age (e.g. 70) is unrealistic here:
  # build_life_table()'s open-interval formula (Lx = lx/mx at the terminal
  # age) assumes the terminal age is genuinely old (high mx), so a low,
  # flat mx there produces a nonsensical "remaining life expectancy" in
  # the thousands of years. Use a Gompertz-like curve spanning to age 100
  # (matching real usage's actual terminal age) so the terminal mx is
  # realistically high.
  age <- 0:100
  mx_female <- 0.0003 * exp(0.07 * age)
  mx_male <- 0.00035 * exp(0.072 * age)
  tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = age, mx_female = mx_female, mx_male = mx_male,
    mx_total = (mx_female + mx_male) / 2
  )
}

test_that("life_expectancy builds a life table for the matched province and returns e0/e65", {
  mx <- make_lifetable_mx_fixture()

  result <- life_expectancy(mx, province = "A Coruna", sex = "female")

  expect_equal(nrow(result), 1)
  expect_equal(result$nuts3_code, "ES111")
  expect_equal(result$sex, "female")
  expect_true(is.finite(result$e0) && result$e0 > 0)
  expect_true(is.finite(result$e65) && result$e65 > 0)
  expect_true(result$e0 > result$e65)
})

test_that("life_expectancy errors when province matches nothing", {
  mx <- make_lifetable_mx_fixture()

  expect_error(
    life_expectancy(mx, province = "^Nowhere$"),
    "did not match"
  )
})

test_that("life_expectancy errors when province matches more than one province", {
  mx <- dplyr::bind_rows(
    make_lifetable_mx_fixture(),
    make_lifetable_mx_fixture() |> dplyr::mutate(nuts3_code = "ES112", province_name = "Lugo")
  )

  expect_error(
    life_expectancy(mx, province = "A Coruna|Lugo"),
    "matched more than one"
  )
})

make_births_age_fixture <- function() {
  tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(14, 20, 30, 50), female = c(1, 10, 15, 1), total = c(2, 20, 30, 2)
  )
}

make_pop_reproductive_fixture <- function() {
  tibble::tibble(
    nuts3_code = "ES111", year = 2023,
    age = c(20, 30), female = c(1000, 1500)
  )
}

test_that("age_specific_fertility_rate joins on nuts3_code/year/age and excludes out-of-range ages", {
  births_age <- make_births_age_fixture()
  pop <- make_pop_reproductive_fixture()

  result <- age_specific_fertility_rate(births_age, pop)

  # age 14 (below age_min) and age 50 (above age_max) must be dropped
  expect_equal(sort(result$age), c(20, 30))
  expect_equal(result$asfr[result$age == 20], 20 / 1000 * 1000)
  expect_equal(result$asfr_female[result$age == 20], 10 / 1000 * 1000)
  expect_equal(result$asfr[result$age == 30], 30 / 1500 * 1000)
  expect_equal(result$asfr_female[result$age == 30], 15 / 1500 * 1000)
})

test_that("age_specific_fertility_rate respects custom age_min/age_max boundaries", {
  births_age <- make_births_age_fixture()
  pop <- tibble::tibble(
    nuts3_code = "ES111", year = 2023, age = c(14, 20, 30, 50),
    female = c(500, 1000, 1500, 400)
  )

  result <- age_specific_fertility_rate(births_age, pop, age_min = 14L, age_max = 50L)

  expect_equal(sort(result$age), c(14, 20, 30, 50))
})

make_asfr_fixture <- function() {
  tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(20, 30), asfr = c(20, 30), asfr_female = c(10, 15)
  )
}

test_that("total_fertility_rate sums asfr over ages and divides by 1000", {
  result <- total_fertility_rate(make_asfr_fixture())

  expect_equal(result$tfr, (20 + 30) / 1000)
})

test_that("mean_age_at_childbearing computes the asfr-weighted mean age", {
  asfr <- make_asfr_fixture()
  result <- mean_age_at_childbearing(asfr)

  expect_equal(result$mac, (20 * 20 + 30 * 30) / (20 + 30))
})

test_that("gross_reproduction_rate sums asfr_female over ages and divides by 1000", {
  result <- gross_reproduction_rate(make_asfr_fixture())

  expect_equal(result$grr, (10 + 15) / 1000)
})

test_that("net_reproduction_rate weights asfr_female by female Lx/radix", {
  asfr <- make_asfr_fixture()
  fltper <- tibble::tibble(
    nuts3_code = "ES111", year = 2023, age = c(20, 30), Lx = c(99000, 98000)
  )

  result <- net_reproduction_rate(asfr, fltper)

  expected <- (10 / 1000 * 99000 / 100000) + (15 / 1000 * 98000 / 100000)
  expect_equal(result$nrr, expected)
})

test_that("net_reproduction_rate is lower than gross_reproduction_rate under any mortality", {
  asfr <- make_asfr_fixture()
  fltper <- tibble::tibble(
    nuts3_code = "ES111", year = 2023, age = c(20, 30), Lx = c(99000, 97000)
  )

  grr <- gross_reproduction_rate(asfr)$grr
  nrr <- net_reproduction_rate(asfr, fltper)$nrr

  expect_true(nrr < grr)
})
