make_mx_fixture <- function() {
  tibble::tibble(
    nuts3_code = rep(c("ES111", "ES112"), each = 6),
    province_name = rep(c("A Coruna", "Lugo"), each = 6),
    year = rep(2020:2022, times = 4),
    age = rep(c(0, 50), each = 3, times = 2),
    mx_total = c(0.004, 0.0038, 0.0035, 0.15, 0.14, 0.16, 0.005, 0.0048, 0.0045, 0.12, 0.11, 0.13)
  )
}

test_that("plot_lexis_diagram filters to the matched province and plots the requested sex column", {
  mx <- make_mx_fixture()

  p <- plot_lexis_diagram(mx, province = "A Coruna", sex = "total")

  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$province_name == "A Coruna"))
  expect_equal(nrow(p$data), 6)
})

test_that("plot_lexis_diagram errors when province matches nothing", {
  mx <- make_mx_fixture()

  expect_error(
    plot_lexis_diagram(mx, province = "^Nowhere$"),
    "did not match"
  )
})

test_that("plot_lexis_diagram errors when province matches more than one province", {
  mx <- make_mx_fixture()

  expect_error(
    plot_lexis_diagram(mx, province = "A Coruna|Lugo"),
    "matched more than one"
  )
})

test_that("plot_lexis_diagram defaults the title to the matched province name", {
  mx <- make_mx_fixture()

  p <- plot_lexis_diagram(mx, province = "Lugo")

  expect_equal(p$labels$title, "Lugo")
})

test_that("plot_lexis_diagram uses a log10 transform by default and linear when log_scale = FALSE", {
  mx <- make_mx_fixture()

  p_log <- plot_lexis_diagram(mx, province = "Lugo", log_scale = TRUE)
  p_linear <- plot_lexis_diagram(mx, province = "Lugo", log_scale = FALSE)

  expect_equal(p_log$scales$scales[[1]]$trans$name, "log-10")
  expect_equal(p_linear$scales$scales[[1]]$trans$name, "identity")
})

test_that("plot_lexis_diagram omits cohort diagonals when cohort_lines = FALSE", {
  mx <- make_mx_fixture()

  p_with <- plot_lexis_diagram(mx, province = "Lugo", cohort_lines = TRUE)
  p_without <- plot_lexis_diagram(mx, province = "Lugo", cohort_lines = FALSE)

  expect_true(length(p_with$layers) > length(p_without$layers))
})

test_that("plot_lexis_diagram rescales the fill values by rate_per and labels the legend accordingly", {
  mx <- make_mx_fixture()

  p_default <- plot_lexis_diagram(mx, province = "Lugo")
  p_raw <- plot_lexis_diagram(mx, province = "Lugo", rate_per = 1)

  expect_equal(p_default$data$plotted_rate, p_raw$data$plotted_rate * 1000)
  expect_match(p_default$scales$scales[[1]]$name, "1,000")
  expect_match(p_raw$scales$scales[[1]]$name, "per person")
})

test_that("plot_lexis_diagram adds a 'Missing data' legend key only when NAs are present", {
  mx_with_na <- make_mx_fixture()
  mx_with_na$mx_total[1] <- NA_real_
  mx_without_na <- make_mx_fixture()

  p_with_na <- plot_lexis_diagram(mx_with_na, province = "A Coruna")
  p_without_na <- plot_lexis_diagram(mx_without_na, province = "A Coruna")

  expect_true(any(vapply(p_with_na$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
  expect_false(any(vapply(p_without_na$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
})
