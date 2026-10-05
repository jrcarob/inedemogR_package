# Regression tests for the defects demonstrated in the SoftwareX review of
# version 0.1.0 (SOFTX-D-26-01227, Reviewer 3's attached script), plus the
# deterministic identity tests the reviewers requested. Each block states the
# correct expected behaviour; the 0.1.0 result is noted in a comment.

flat_counts <- function(age = 0:100, years = 2024, value = 1000) {
  df <- expand.grid(age = age, year = years)
  tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna",
    year = df$year, age = df$age,
    female = value, male = value, total = 2 * value
  )
}

gompertz_mx <- function(age = 0:100, a = 0.0003, b = 0.07) a * exp(b * age)

# Deaths/exposure for one province-year whose rates follow a Gompertz curve,
# large enough that every age 80-99 has > 100 deaths (so Y = 95).
gompertz_counts <- function(e = 1e6) {
  age <- 0:100
  mx <- gompertz_mx(age)
  exposure <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2024, age = age,
    female = e, male = e, total = 2 * e
  )
  deaths <- exposure
  deaths$female <- mx * e
  deaths$male <- mx * e * 1.2
  deaths$total <- deaths$female + deaths$male
  list(deaths = deaths, exposure = exposure)
}

# ---- Missing data ----------------------------------------------------------

test_that("an explicitly missing death count stays missing and fails validation", {
  exposure <- flat_counts()
  deaths <- flat_counts(value = 10)
  deaths$female[deaths$age == 20] <- NA
  deaths$total[deaths$age == 20] <- NA

  rates <- suppressWarnings(compute_death_rates(deaths, exposure))
  row <- rates$mx_1x1[rates$mx_1x1$age == 20, ]
  expect_true(is.na(row$mx_female)) # 0.1.0: 0
  expect_true(is.na(row$mx_total))
  expect_false(rates$qc$passed) # 0.1.0: TRUE
})

test_that("a province-year absent from deaths is not turned into zero mortality", {
  population <- expand.grid(nuts3_code = c("ES111", "ES112"), year = 2023:2025, age = 0:100)
  population$province_name <- ifelse(population$nuts3_code == "ES111", "A Coruna", "Lugo")
  population$female <- 1000
  population$male <- 1000
  population$total <- 2000
  deaths <- subset(population, year < 2025 & !(nuts3_code == "ES112" & year == 2024))
  deaths$female <- 1
  deaths$male <- 1
  deaths$total <- 2

  exposure <- suppressWarnings(suppressMessages(compute_exposure(population, deaths)))
  rates <- suppressWarnings(suppressMessages(compute_death_rates(deaths, exposure$data)))
  lugo_2024 <- subset(rates$mx_1x1, province_name == "Lugo" & year == 2024)
  expect_equal(nrow(lugo_2024), 0) # 0.1.0: 101 rows of zero mortality
})

test_that("build_life_table rejects missing rates and non-contiguous ages", {
  mortality <- data.frame(age = 0:100, mx = gompertz_mx())
  bad_na <- mortality
  bad_na$mx[bad_na$age == 20] <- NA
  expect_error(build_life_table(bad_na, "female"), "finite") # 0.1.0: all ex NA

  bad_gap <- subset(mortality, age != 20)
  expect_error(build_life_table(bad_gap, "female"), "contiguous") # 0.1.0: accepted

  bad_dup <- rbind(mortality, mortality[21, ])
  expect_error(build_life_table(bad_dup, "female"), "contiguous")
})

test_that("validate_life_table fails on missing ages, non-finite values, implausible e0", {
  lt <- build_life_table(data.frame(age = 0:100, mx = gompertz_mx()), "female")
  expect_true(validate_life_table(lt)$passed)

  gap <- lt[lt$age != 20, ]
  expect_false(suppressWarnings(validate_life_table(gap))$passed)

  na_ex <- lt
  na_ex$ex[5] <- NA
  expect_false(suppressWarnings(validate_life_table(na_ex))$passed)

  huge <- lt
  huge$ex[1] <- 217.9 # Ceuta 1999 male e0 under 0.1.0
  expect_false(suppressWarnings(validate_life_table(huge))$passed)
})

# ---- Andreev-Kingkade a0 (HMD V6 Table 3) -----------------------------------

test_that("andreev_kingkade_a0 reproduces every branch of HMD V6 Table 3", {
  # 0.1.0 returned 0.0642 (female) and 0.055736 (male) at m0 = 0.004.
  expect_equal(andreev_kingkade_a0(0.004, "female"), 0.14903 - 2.05527 * 0.004)
  expect_equal(andreev_kingkade_a0(0.004, "male"), 0.14929 - 1.99545 * 0.004)

  expect_equal(andreev_kingkade_a0(0.03, "female"), 0.04667 + 3.88089 * 0.03)
  expect_equal(andreev_kingkade_a0(0.03, "male"), 0.02832 + 3.26021 * 0.03)

  expect_equal(andreev_kingkade_a0(0.10, "female"), 0.31411)
  expect_equal(andreev_kingkade_a0(0.10, "male"), 0.29915)

  # Thresholds: lower bound of each interval is closed.
  expect_equal(andreev_kingkade_a0(0.01724, "female"), 0.04667 + 3.88089 * 0.01724)
  expect_equal(andreev_kingkade_a0(0.06891, "female"), 0.31411)
  expect_equal(andreev_kingkade_a0(0.02300, "male"), 0.02832 + 3.26021 * 0.02300)
  expect_equal(andreev_kingkade_a0(0.08307, "male"), 0.29915)

  expect_true(is.na(andreev_kingkade_a0(NA_real_, "female")))
})

test_that("single-sex life tables use ax = 0.5 at every age except 0 (HMD V6)", {
  lt <- build_life_table(data.frame(age = 0:100, mx = gompertz_mx()), "female")
  expect_equal(lt$ax[lt$age == 1], 0.5) # 0.1.0: 0.4
  expect_equal(lt$ax[lt$age == 0], andreev_kingkade_a0(lt$mx[1], "female"))
})

test_that("combined-sex a0 is the death-weighted average of the sex-specific a0 (HMD V6 Eq. 77)", {
  x <- gompertz_counts()
  rates <- suppressMessages(compute_death_rates(x$deaths, x$exposure))
  lt <- suppressMessages(build_life_tables(rates$mx_1x1))
  d0f <- x$deaths$female[1]
  d0m <- x$deaths$male[1]
  a0f <- andreev_kingkade_a0(rates$mx_1x1$mx_female[1], "female")
  a0m <- andreev_kingkade_a0(rates$mx_1x1$mx_male[1], "male")
  expect_equal(lt$bltper$ax[lt$bltper$age == 0], (a0f * d0f + a0m * d0m) / (d0f + d0m))
})

# ---- Exposure (HMD V6 Eq. 57) ----------------------------------------------

test_that("cohort-split exposure uses (DL - DU) / 6 with conventional triangle labels", {
  population <- data.frame(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2020:2021,
    age = 40, female = 1000, male = 1000, total = 2000
  )
  # Lower triangle (born t - x = 1980) holds 80 deaths, upper (1979) holds 20.
  deaths <- data.frame(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2020, age = 40,
    cohort = c(1980, 1979), female = c(80, 20), male = c(80, 20), total = c(160, 40)
  )
  exposure <- suppressWarnings(suppressMessages(compute_exposure(population, deaths)))
  expect_equal(exposure$data$female, 1000 + (80 - 20) / 6) # 0.1.0: 1030
})

test_that("even-split exposure reduces to the mean of the two January-1 stocks", {
  population <- data.frame(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2020:2021,
    age = 40, female = c(1000, 1100), male = 1000, total = c(2000, 2100)
  )
  deaths <- data.frame(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2020, age = 40,
    female = 30, male = 30, total = 60
  )
  exposure <- suppressWarnings(suppressMessages(compute_exposure(population, deaths)))
  expect_equal(exposure$data$female, 1050)
})

# ---- Kannisto old-age smoothing (HMD V6 Eqs. 67-68) ------------------------

test_that("Kannisto Poisson fit recovers known parameters and never declines with age", {
  age <- 80:99
  a <- 0.07
  b <- 0.11
  mu <- a * exp(b * (age + 0.5 - 80)) / (1 + a * exp(b * (age + 0.5 - 80)))
  e <- rep(1e7, length(age))
  fit <- inedemogR:::fit_kannisto(age, deaths = mu * e, exposure = e)
  expect_equal(fit$a, a, tolerance = 1e-4)
  expect_equal(fit$b, b, tolerance = 1e-4)

  # Declining observed rates (the Ceuta/Melilla failure mode) must not produce
  # a negative slope: b is constrained to be >= 0.
  e2 <- rep(100, 5)
  d2 <- c(20, 15, 10, 8, 5)
  fit2 <- inedemogR:::fit_kannisto(80:84, deaths = d2, exposure = e2)
  expect_gte(fit2$b, 0)
})

test_that("old-age replacement age Y follows the HMD 100-deaths rule within [80, 95]", {
  d <- data.frame(age = 70:100, female = 500, male = 500)
  d$female[d$age >= 88] <- 90
  expect_equal(inedemogR:::kannisto_replacement_age(d$age, d$female, d$male), 88)

  big <- data.frame(age = 70:100, female = 500, male = 500)
  expect_equal(inedemogR:::kannisto_replacement_age(big$age, big$female, big$male), 95)

  small <- data.frame(age = 70:100, female = 5, male = 5)
  expect_equal(inedemogR:::kannisto_replacement_age(small$age, small$female, small$male), 80)
})

test_that("build_life_tables keeps observed rates below Y and extends to 110+", {
  x <- gompertz_counts()
  rates <- suppressMessages(compute_death_rates(x$deaths, x$exposure))
  lt <- suppressMessages(build_life_tables(rates$mx_1x1))
  f <- lt$fltper
  expect_equal(range(f$age), c(0, 110))
  expect_equal(f$mx[f$age < 95], rates$mx_1x1$mx_female[rates$mx_1x1$age < 95])
  expect_true(all(diff(f$mx[f$age >= 95]) >= 0))
  expect_true(all(lt$qc$passed))
})

test_that("build_life_tables withholds tables with incomplete age coverage", {
  x <- gompertz_counts()
  rates <- suppressMessages(compute_death_rates(x$deaths, x$exposure))
  holed <- rates$mx_1x1[!(rates$mx_1x1$age %in% 85:99), ]
  lt <- suppressWarnings(suppressMessages(build_life_tables(holed)))
  expect_equal(nrow(lt$fltper), 0)
  expect_equal(nrow(lt$failed), 3) # female, male, total
  expect_match(lt$failed$reason[1], "contiguous|coverage")
})

test_that("Kannisto settings are configurable for sensitivity analysis", {
  x <- gompertz_counts()
  rates <- suppressMessages(compute_death_rates(x$deaths, x$exposure))
  lt85 <- suppressMessages(build_life_tables(rates$mx_1x1, kannisto_age = 85))
  expect_equal(unique(lt85$qc$kannisto_age), 85)
  f <- lt85$fltper
  expect_equal(f$mx[f$age < 85], rates$mx_1x1$mx_female[rates$mx_1x1$age < 85])

  lt_fit <- suppressMessages(build_life_tables(rates$mx_1x1, fit_min = 85))
  expect_true(all(lt_fit$qc$passed))
  expect_error(build_life_tables(rates$mx_1x1, kannisto_age = 70), "kannisto_age")
})

test_that("zero exposure above Y (tiny populations) is smoothed, missing exposure is withheld", {
  x <- gompertz_counts(e = 300)
  rates <- suppressMessages(compute_death_rates(x$deaths, x$exposure))
  m <- rates$mx_1x1
  old <- m$age >= 97
  m$e_male[old] <- 0
  m$d_male[old] <- 0
  m$mx_male[old] <- NaN # 0 / 0
  lt <- suppressMessages(build_life_tables(m))
  expect_equal(nrow(lt$failed), 0)
  expect_true(all(lt$qc$passed))
  expect_lt(lt$qc$kannisto_age[1], 97)

  m$e_female[m$age == 90] <- NA # top-coded or unavailable population
  lt2 <- suppressWarnings(suppressMessages(build_life_tables(m)))
  expect_equal(nrow(lt2$failed), 3)
  expect_match(lt2$failed$reason[1], "exposure")
})

# ---- Life-table identities -------------------------------------------------

test_that("full life table satisfies its accounting identities", {
  lt <- build_life_table(data.frame(age = 0:100, mx = gompertz_mx()), "male")
  n <- nrow(lt)
  expect_equal(sum(lt$dx), lt$lx[1])
  expect_equal(lt$lx[-1], lt$lx[-n] - lt$dx[-n])
  expect_equal(lt$dx / lt$Lx, lt$mx) # mx = dx / Lx at every age incl. open
  expect_equal(lt$ex[1], sum(lt$Lx) / lt$lx[1])
})

test_that("abridged life table preserves mx = dx / Lx in every closed interval", {
  infant_rates <- data.frame(age = 0:4, mx = c(.004, .0003, .0002, .0002, .0002))
  ages <- seq(0, 95, 5)
  grouped_rates <- data.frame(
    age_group = c(sprintf("%02d-%02d", ages, ages + 4), "100+"),
    mx = c(.001, rep(.02, 15), rep(.2, 4), .4)
  )
  alt <- build_abridged_life_table(infant_rates, grouped_rates, "female")
  closed <- alt[-c(1, nrow(alt)), ]
  expect_equal(closed$dx / closed$Lx, closed$mx) # 0.1.0: 0.1848 vs 0.2 at 80-84
  expect_equal(sum(alt$dx), alt$lx[1])
  expect_true(validate_abridged_life_table(alt)$passed)

  # A zero rate in a neighbouring group makes Greville's slope undefined: the
  # constant-hazard fallback must still keep the identity and finite values.
  sparse <- grouped_rates
  sparse$mx[3] <- 0
  alt0 <- build_abridged_life_table(infant_rates, sparse, "female")
  closed0 <- alt0[-c(1, nrow(alt0)), ]
  ok <- closed0$mx > 0
  expect_equal(closed0$dx[ok] / closed0$Lx[ok], closed0$mx[ok])
  expect_true(all(is.finite(alt0$ex)))

  broken <- alt
  broken$Lx[17] <- broken$Lx[17] * 1.1
  expect_false(suppressWarnings(validate_abridged_life_table(broken))$passed)
})

# ---- Decomposition ---------------------------------------------------------

test_that("pollard recovers a change confined to the open age interval", {
  table_a <- build_life_table(data.frame(age = 0:100, mx = c(rep(.01, 100), .20)), "female")
  table_b <- build_life_table(data.frame(age = 0:100, mx = c(rep(.01, 100), .25)), "female")
  exact <- table_a$lx[101] / table_a$lx[1] * (1 / .25 - 1 / .20)
  pollard <- decompose_life_expectancy(table_a, table_b, "pollard")
  arriaga <- decompose_life_expectancy(table_a, table_b, "arriaga")
  expect_equal(sum(pollard$contribution), exact) # 0.1.0: 22.5% of exact
  expect_equal(sum(arriaga$contribution), exact)
})

test_that("decompositions are antisymmetric and zero for identical schedules", {
  lt1 <- build_life_table(data.frame(age = 0:100, mx = gompertz_mx(a = 4e-4, b = .075)), "female")
  lt2 <- build_life_table(data.frame(age = 0:100, mx = gompertz_mx(a = 3e-4, b = .070)), "female")
  for (m in c("arriaga", "pollard")) {
    fwd <- sum(decompose_life_expectancy(lt1, lt2, m)$contribution)
    rev <- sum(decompose_life_expectancy(lt2, lt1, m)$contribution)
    expect_equal(fwd, -rev, tolerance = 1e-6)
    expect_equal(sum(decompose_life_expectancy(lt1, lt1, m)$contribution), 0)
  }
  # Unequal radices give the same per-radix answer.
  lt1b <- lt1
  lt1b[c("lx", "dx", "Lx", "Tx")] <- lt1b[c("lx", "dx", "Lx", "Tx")] * 2
  for (m in c("arriaga", "pollard")) {
    expect_equal(
      decompose_life_expectancy(lt1b, lt2, m)$contribution,
      decompose_life_expectancy(lt1, lt2, m)$contribution
    )
  }
})

test_that("decomposition residuals are reported", {
  lt1 <- build_life_table(data.frame(age = 0:100, mx = gompertz_mx(a = 4e-4, b = .075)), "female")
  lt2 <- build_life_table(data.frame(age = 0:100, mx = gompertz_mx(a = 3e-4, b = .070)), "female")
  d <- decompose_life_expectancy(lt1, lt2, "pollard")
  gap <- lt2$ex[1] - lt1$ex[1]
  expect_equal(attr(d, "e0_difference"), gap)
  expect_equal(attr(d, "residual"), gap - sum(d$contribution))
  expect_lt(abs(attr(d, "residual") / gap), 0.01)
})

# ---- ESP 2013 --------------------------------------------------------------

test_that("ESP 2013 weights sum to 100,000 with 200 for ages 95+", {
  w <- inedemogR:::esp_2013_weights
  expect_equal(sum(w$weight), 100000) # 0.1.0: 100400
  expect_equal(sum(w$weight[w$age_group_start >= 95]), 200) # 0.1.0: 600
})

test_that("constant mortality of 0.01 standardises to 1,000 per 100,000", {
  exposure <- flat_counts()
  deaths <- flat_counts(value = 10)
  rates <- suppressMessages(compute_death_rates(deaths, exposure))
  expect_equal(rates$asdr$asdr_female, 1000) # 0.1.0: 1004
})

test_that("ASDR is NA when an age is missing rather than silently low", {
  exposure <- flat_counts()
  deaths <- flat_counts(value = 10)
  deaths$female[deaths$age == 50] <- NA
  rates <- suppressWarnings(suppressMessages(compute_death_rates(deaths, exposure)))
  expect_true(is.na(rates$asdr$asdr_female))
  expect_equal(rates$asdr$asdr_male, 1000)
})

# ---- Fertility (INE conventions) --------------------------------------------

test_that("ASFR uses mean annual population (mean of consecutive January-1 stocks)", {
  births <- data.frame(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2024,
    age = 33, female = 55, total = 110
  )
  population <- data.frame(nuts3_code = "ES111", year = 2024:2025, age = 33, female = c(1000, 1200))
  asfr <- age_specific_fertility_rate(births, population)
  expect_equal(asfr$asfr, 110 / 1100) # per woman
  expect_equal(asfr$asfr_per_1000, 100) # 0.1.0: 110
})

test_that("mean age at childbearing uses the x + 0.5 midpoint", {
  asfr <- data.frame(nuts3_code = "ES111", province_name = "A Coruna", year = 2024, age = 33, asfr = 0.1)
  expect_equal(mean_age_at_childbearing(asfr)$mac, 33.5) # 0.1.0: 33
})

test_that("births outside 15-49 are folded into the boundary ages", {
  births <- data.frame(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2024, age = 14:50,
    female = 0, total = 0
  )
  births$total[births$age %in% c(14, 15, 49, 50)] <- c(10, 20, 30, 40)
  births$female <- births$total / 2
  population <- data.frame(nuts3_code = "ES111", year = 2024, age = 15:49, female = 1000)
  asfr <- suppressWarnings(age_specific_fertility_rate(births, population))
  expect_equal(total_fertility_rate(asfr)$tfr, 0.10) # 0.1.0: 0.05
  expect_equal(asfr$asfr[asfr$age == 15], 30 / 1000)
  expect_equal(asfr$asfr[asfr$age == 49], 70 / 1000)
})

test_that("INE's under-15 group (age NA, age_group 'under15') folds into age 15", {
  births <- data.frame(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2024,
    age = c(NA, 15), age_group = c("under15", "15"), female = c(1, 2), total = c(2, 4)
  )
  population <- data.frame(nuts3_code = "ES111", year = 2024:2025, age = 15, female = 100)
  asfr <- age_specific_fertility_rate(births, population)
  expect_equal(asfr$asfr, 6 / 100)
})

# ---- Caching ---------------------------------------------------------------

test_that("use_cache = TRUE with cache_dir = NULL reuses results within the session", {
  key <- paste0("demo_", as.integer(stats::runif(1, 1, 1e9)))
  first <- suppressMessages(inedemogR:::with_ine_cache(key, TRUE, FALSE, NULL, tempfile))
  second <- suppressMessages(inedemogR:::with_ine_cache(key, TRUE, FALSE, NULL, tempfile))
  expect_identical(first, second) # 0.1.0: different values
})
