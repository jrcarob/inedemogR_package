test_that("plot_population_pyramid negates male counts for the mirrored layout", {
  pop <- tibble::tibble(
    province_name = "A Coruna", year = 2023,
    age = c(0, 1), female = c(100, 90), male = c(105, 95)
  )

  p <- plot_population_pyramid(pop, year = 2023)

  expect_s3_class(p, "ggplot")
  male_rows <- p$data[p$data$sex == "male", ]
  female_rows <- p$data[p$data$sex == "female", ]
  expect_true(all(male_rows$count <= 0))
  expect_true(all(female_rows$count >= 0))
  expect_equal(sort(abs(male_rows$count)), sort(c(105, 95)))
})

test_that("plot_population_pyramid warns when region matches nothing", {
  pop <- tibble::tibble(
    province_name = "A Coruna", year = 2023, age = 0, female = 1, male = 1
  )

  expect_warning(
    plot_population_pyramid(pop, year = 2023, region = "^Nowhere$"),
    "did not match"
  )
})

test_that("plot_demog_trend plots the requested value column", {
  df <- tibble::tibble(
    nuts3_code = c("ES111", "ES111"), province_name = "A Coruna",
    year = c(2022, 2023), aging_index = c(150, 160)
  )

  p <- plot_demog_trend(df, "aging_index")

  expect_s3_class(p, "ggplot")
  expect_equal(p$data$aging_index, c(150, 160))
})

test_that("map_indicator joins a value column onto geometry by GEOID", {
  raw_sf <- sf::st_sf(
    GEOID = c("15", "27"),
    NAME = c("A Coruna", "Lugo"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),
    crs = 25830
  )
  mockery::stub(map_indicator, "get_ine_geo", raw_sf)

  df <- tibble::tibble(GEOID = c("15", "27"), value = c(10, 20))

  p <- map_indicator(df, "value", geo_level = "province")

  expect_s3_class(p, "ggplot")
  expect_equal(p$data$value[p$data$GEOID == "15"], 10)
  expect_equal(p$data$value[p$data$GEOID == "27"], 20)
})

test_that("map_indicator warns when a region has no matching value", {
  raw_sf <- sf::st_sf(
    GEOID = c("15", "28"),
    NAME = c("A Coruna", "Madrid"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),
    crs = 25830
  )
  mockery::stub(map_indicator, "get_ine_geo", raw_sf)

  df <- tibble::tibble(GEOID = "15", value = 10)

  expect_warning(
    map_indicator(df, "value", geo_level = "province"),
    "no matching value"
  )
})

test_that("map_indicator uses a binned scale by default and continuous when binned = FALSE", {
  raw_sf <- sf::st_sf(
    GEOID = c("15", "27"),
    NAME = c("A Coruna", "Lugo"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1))),
    crs = 25830
  )
  mockery::stub(map_indicator, "get_ine_geo", raw_sf)
  df <- tibble::tibble(GEOID = c("15", "27"), value = c(0.5, 2))

  p_binned <- map_indicator(df, "value", geo_level = "province")
  p_continuous <- map_indicator(df, "value", geo_level = "province", binned = FALSE)

  expect_s3_class(p_binned$scales$scales[[1]], "ScaleBinned")
  expect_s3_class(p_continuous$scales$scales[[1]], "ScaleContinuous")
})

test_that("map_life_expectancy filters by year/sex and bridges nuts3_code to GEOID before delegating to map_indicator", {
  # mockery::stub only intercepts a direct (one-hop) call, so get_ine_geo
  # can't be mocked here through map_life_expectancy -> map_indicator ->
  # get_ine_geo (two hops, resolved via the package namespace); instead
  # stub map_indicator itself (map_life_expectancy's direct dependency)
  # and inspect what it was called with.
  captured <- NULL
  fake_map_indicator <- function(df, value_col, geo_level, ...) {
    captured <<- df
    "not a real plot"
  }
  mockery::stub(map_life_expectancy, "map_indicator", fake_map_indicator)

  le <- tibble::tibble(
    nuts3_code = c("ES111", "ES112", "ES111"),
    year = c(2022, 2023, 2023), sex = c("female", "female", "male"),
    e0 = c(80, 85.5, 86.97)
  )

  map_life_expectancy(le, year = 2023, sex = "female")

  expect_equal(nrow(captured), 1)
  expect_equal(captured$GEOID, "27") # ES112 -> Lugo's ine_code
  expect_equal(captured$e0, 85.5)
})
