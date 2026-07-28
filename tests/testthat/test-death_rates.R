test_that("compute_death_rates treats ages absent from deaths as zero, not dropped", {
  # Regression test: INE's age-specific deaths table only publishes rows
  # for age/year/sex cells with at least one death; a missing row means
  # zero deaths, not a gap. compute_mx_1x1_one() used to inner_join deaths
  # onto exposure, silently dropping those zero-death ages and breaking
  # the age-contiguity the life-table recursion requires.
  exposure <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = 0:2,
    female = c(1000, 990, 980), male = c(1050, 1040, 1030),
    total = c(2050, 2030, 2010)
  )
  deaths <- tibble::tibble(
    nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
    age = c(0, 2), # age 1 has no row: zero deaths that year, not missing data
    female = c(1, 1), male = c(1, 1), total = c(2, 2)
  )

  rates <- compute_death_rates(deaths, exposure)

  expect_equal(nrow(rates$mx_1x1), 3)
  age1 <- rates$mx_1x1[rates$mx_1x1$age == 1, ]
  expect_equal(age1$mx_total, 0)
  expect_false(is.na(age1$mx_total))
})

test_that("compute_mx_5x1_one keeps age groups with no deaths at all", {
  exposure <- tibble::tibble(year = 2023, age = 0:9, female = 100, male = 100, total = 200)
  # No deaths at all in ages 5-9: that whole age group has no row in deaths.
  deaths <- tibble::tibble(year = 2023, age = 0:4, female = 1, male = 1, total = 2)

  mx5 <- compute_mx_5x1_one(deaths, exposure)

  expect_equal(nrow(mx5), 2)
  older <- mx5[mx5$age_group == "05-09", ]
  expect_equal(older$mx_total, 0)
})
