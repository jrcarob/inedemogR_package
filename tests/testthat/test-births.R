make_series <- function(geo, sex_newborn, age_label, year, value) {
  list(
    Nombre = sprintf(
      "%s. Nacimiento. %s. %s. Total. Lugar de residencia de la madre. Dato base. ",
      geo, sex_newborn, age_label
    ),
    Data = list(list(Anyo = year, Valor = value))
  )
}

make_raw_births_by_age_fixture <- function() {
  list(
    make_series("A Coruna", "Total", "Menos de 15 años", 2023, 1),
    make_series("A Coruna", "Total", "20 años", 2023, 20),
    make_series("A Coruna", "Mujeres", "20 años", 2023, 10),
    make_series("A Coruna", "Hombres", "20 años", 2023, 10),
    make_series("A Coruna", "Total", "50 y más años", 2023, 1),
    make_series("Total Nacional", "Total", "20 años", 2023, 500)
  )
}

test_that("parse_births_by_age extracts geo, sex, and age from series names", {
  tidy <- parse_births_by_age(make_raw_births_by_age_fixture())

  # "Total Nacional" doesn't match any province in province_lookup, so it's dropped
  expect_true(all(tidy$province_name_raw != "Total Nacional"))
  expect_setequal(tidy$sex_newborn, c("Total", "Mujeres", "Hombres"))

  under15 <- tidy[tidy$age_group == "under15", ]
  expect_true(is.na(under15$age))

  plus50 <- tidy[tidy$age_group == "50plus", ]
  expect_equal(plus50$age, 50L)

  age20 <- tidy[tidy$age_group == "20", ]
  expect_true(all(age20$age == 20L))
})

test_that("clean_births_by_age pivots sex-of-newborn to female/male/total columns", {
  tidy <- parse_births_by_age(make_raw_births_by_age_fixture())
  clean <- clean_births_by_age(tidy)

  age20 <- clean[clean$age_group == "20", ]
  expect_equal(age20$total, 20)
  expect_equal(age20$female, 10)
  expect_equal(age20$male, 10)
  expect_equal(age20$nuts3_code, "ES111")
  expect_equal(age20$province_name, "A Coruna")
})

test_that("validate_births_by_age flags negative/missing counts", {
  clean <- tibble::tibble(
    ine_code = "15", nuts3_code = "ES111", nuts2_code = "ES11",
    province_name = "A Coruna", year = 2023, age = 20L, age_group = "20",
    female = -1, male = 10, total = 9
  )

  qc <- suppressWarnings(validate_births_by_age(clean))

  expect_false(qc$passed)
  expect_true("invalid_counts" %in% names(qc$issues))
})

test_that("validate_births_by_age flags duplicate province-year-age rows", {
  clean <- tibble::tibble(
    ine_code = "15", nuts3_code = "ES111", nuts2_code = "ES11",
    province_name = "A Coruna", year = 2023, age = c(20L, 20L),
    age_group = c("20", "20"), female = c(10, 10), male = c(10, 10), total = c(20, 20)
  )

  qc <- suppressWarnings(validate_births_by_age(clean))

  expect_false(qc$passed)
  expect_true("duplicate_rows" %in% names(qc$issues))
})

test_that("get_ine_births_by_age caches results via with_ine_cache", {
  tmp_dir <- withr::local_tempdir()
  raw <- make_raw_births_by_age_fixture()

  mockery::stub(get_ine_births_by_age, "fetch_ine_json", raw)

  result1 <- suppressWarnings(
    get_ine_births_by_age(table_id = 6508, n_periods = 5, cache_dir = tmp_dir)
  )
  expect_true(nrow(result1$data) > 0)

  # Second call should hit the cache and not need fetch_ine_json at all;
  # stub it to error so a cache miss would fail the test loudly.
  mockery::stub(
    get_ine_births_by_age, "fetch_ine_json",
    function(...) stop("should not be called - cache should have been used")
  )
  result2 <- suppressWarnings(
    get_ine_births_by_age(table_id = 6508, n_periods = 5, cache_dir = tmp_dir)
  )

  expect_identical(result1, result2)
})

test_that("validate_births does not flag normal small-sample SRB noise", {
  # A small province with only 40 births/year and a plausible SRB of
  # 1.10 (22 male, 18 female) - the old fixed [1.030, 1.070] band would
  # have flagged this every time despite it being unremarkable sampling
  # noise for such a small count (this exact pattern was found to flag
  # ~60% of real INE data before recalibration).
  df <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2020:2023,
    female = c(18, 19, 17, 20), male = c(22, 21, 23, 20), total = female + male
  )
  qc <- validate_births(df)

  expect_true(qc$passed)
  expect_null(qc$issues$srb_out_of_range)
})

test_that("validate_births flags a genuinely implausible SRB", {
  # A large province (10,000 total births/year - tiny sampling error)
  # with an SRB of 1.30, far outside any plausible range regardless of
  # sample size.
  df <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    female = 4348, male = 5652, total = female + male
  )
  qc <- suppressWarnings(validate_births(df))

  expect_false(qc$passed)
  expect_true("srb_out_of_range" %in% names(qc$issues))
})
