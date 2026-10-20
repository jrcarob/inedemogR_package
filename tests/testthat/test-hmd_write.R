test_that("write_births_txt writes only the matching province's rows per file", {
  # Regression test: write_txt_by_province()'s per-province filter used an
  # unqualified `nuts3_code` on both sides of the comparison, so a function
  # argument named identically to the `nuts3_code` column shadowed it under
  # dplyr's data-mask precedence, making the filter always true and mixing
  # every province into every file.
  df <- tibble::tibble(
    nuts3_code = c("ES300", "ES111"),
    province_name = c("Madrid", "A Coruna"),
    year = c(2023, 2023),
    female = c(24000, 3000),
    male = c(25000, 3100),
    total = c(49000, 6100)
  )
  tmp <- withr::local_tempdir()
  write_births_txt(df, tmp)

  madrid_lines <- readLines(file.path(tmp, "Births_ES300_Madrid.txt"))
  header_idx <- which(grepl("Year", madrid_lines))
  data_lines <- madrid_lines[(header_idx + 1):length(madrid_lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  expect_equal(length(data_lines), 1)
  expect_true(any(grepl("49000", data_lines)))
  expect_false(any(grepl("6100", data_lines)))

  coruna_lines <- readLines(file.path(tmp, "Births_ES111_A_Coruna.txt"))
  header_idx2 <- which(grepl("Year", coruna_lines))
  coruna_data <- coruna_lines[(header_idx2 + 1):length(coruna_lines)]
  coruna_data <- coruna_data[nzchar(trimws(coruna_data))]

  expect_equal(length(coruna_data), 1)
  expect_true(any(grepl("6100", coruna_data)))
  expect_false(any(grepl("49000", coruna_data)))
})

test_that("write_population_txt separates provinces across many age rows", {
  df <- tibble::tibble(
    nuts3_code = rep(c("ES300", "ES111"), each = 3),
    province_name = rep(c("Madrid", "A Coruna"), each = 3),
    year = 2023,
    age = rep(0:2, times = 2),
    female = c(1000, 1001, 1002, 100, 101, 102),
    male = c(1050, 1051, 1052, 105, 106, 107),
    total = c(2050, 2052, 2054, 205, 207, 209)
  )
  tmp <- withr::local_tempdir()
  write_population_txt(df, tmp)

  madrid_lines <- readLines(file.path(tmp, "Population_ES300_Madrid.txt"))
  header_idx <- which(grepl("Year", madrid_lines))
  data_lines <- madrid_lines[(header_idx + 1):length(madrid_lines)]
  data_lines <- data_lines[nzchar(trimws(data_lines))]

  expect_equal(length(data_lines), 3)
  expect_false(any(grepl("A Coruna", madrid_lines)))
})
