#' @include helpers.R
NULL

# Central death rate computation for the SHMD pipeline, per SHMD Protocol
# Section 12, Step 5 (HMD Methods Protocol V6):
#   m(x,t)   = D(x,t) / E(x,t)                          (1x1)
#   m(x,n,t) = sum_k D(x+k,t) / sum_k E(x+k,t)           (nx1, pooled)
#   ASDR(t)  = sum_x m(x,t) * w(x)                       (ESP 2013 weights)

# European Standard Population 2013 (Eurostat 2013, Table 2.1; reproduced by
# ONS), per 100,000. The final reference group is 95+ (200 in total). The
# package's open interval is 100+, so the 95+ weight is spread evenly over the
# six single-age cells 95, 96, ..., 99, 100+ (200/6 each); every other group is
# spread evenly over its five single ages.
esp_2013_weights <- tibble::tribble(
  ~age_group_start, ~age_group_end, ~weight,
  0, 4, 5000,
  5, 9, 5500,
  10, 14, 5500,
  15, 19, 5500,
  20, 24, 6000,
  25, 29, 6000,
  30, 34, 6500,
  35, 39, 7000,
  40, 44, 7000,
  45, 49, 7000,
  50, 54, 7000,
  55, 59, 6500,
  60, 64, 6000,
  65, 69, 5500,
  70, 74, 5000,
  75, 79, 4000,
  80, 84, 2500,
  85, 89, 1500,
  90, 94, 800,
  95, MAX_AGE, 200
)

# Missing-data conventions (documented in compute_death_rates()):
# * INE's age-specific deaths table omits cells with zero deaths, so an age
#   absent from a province-year that *is* present in `deaths` is a structural
#   zero.
# * An explicit NA count (unavailable/suppressed) stays NA and propagates to
#   the rate, and from there to grouped rates and ASDR.
# * A province-year with no rows at all in `deaths` is unavailable, not zero:
#   it is dropped from the output.
#' @noRd
compute_mx_1x1_one <- function(deaths_df, exposure_df) {
  deaths_df <- deaths_df |>
    dplyr::select("year", "age", d_female = "female", d_male = "male", d_total = "total")
  exposure_df |>
    dplyr::filter(.data$year %in% deaths_df$year) |>
    dplyr::select("year", "age", e_female = "female", e_male = "male", e_total = "total") |>
    dplyr::left_join(deaths_df, by = c("year", "age")) |>
    dplyr::mutate(
      absent = !paste(.data$year, .data$age) %in% paste(deaths_df$year, deaths_df$age),
      dplyr::across(c("d_female", "d_male", "d_total"), ~ dplyr::if_else(.data$absent, 0, .x)),
      mx_female = .data$d_female / .data$e_female,
      mx_male = .data$d_male / .data$e_male,
      mx_total = .data$d_total / .data$e_total
    ) |>
    dplyr::select(
      "year", "age", "mx_female", "mx_male", "mx_total",
      "d_female", "d_male", "d_total", "e_female", "e_male", "e_total"
    ) |>
    dplyr::arrange(.data$year, .data$age)
}

#' @noRd
assign_age_group <- function(age) {
  ifelse(
    age >= MAX_AGE, sprintf("%d+", MAX_AGE),
    sprintf("%02d-%02d", (age %/% 5) * 5, (age %/% 5) * 5 + 4)
  )
}

# Pooled grouped rates (sum deaths / sum exposure) from the 1x1 counts, so the
# same missing-data conventions apply; an NA in any member age gives NA.
#' @noRd
compute_mx_5x1_one <- function(mx_1x1_df) {
  mx_1x1_df |>
    dplyr::mutate(age_group = assign_age_group(.data$age)) |>
    dplyr::group_by(.data$year, .data$age_group) |>
    dplyr::summarise(
      dplyr::across(c("d_female", "d_male", "d_total", "e_female", "e_male", "e_total"), sum),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mx_female = .data$d_female / .data$e_female,
      mx_male = .data$d_male / .data$e_male,
      mx_total = .data$d_total / .data$e_total
    ) |>
    dplyr::select(
      "year", "age_group", "mx_female", "mx_male", "mx_total",
      "d_female", "d_male", "d_total", "e_female", "e_male", "e_total"
    ) |>
    dplyr::arrange(.data$year, .data$age_group)
}

#' @noRd
esp_weight_per_age <- function() {
  w <- esp_2013_weights
  tibble::tibble(
    age = 0:MAX_AGE,
    weight_per_age = purrr::map_dbl(0:MAX_AGE, function(a) {
      i <- which(a >= w$age_group_start & a <= w$age_group_end)
      w$weight[i] / (w$age_group_end[i] - w$age_group_start[i] + 1)
    })
  )
}

# ASDR per 100,000 (ESP 2013). NA unless every age 0..MAX_AGE has a finite rate.
#' @noRd
compute_asdr_one <- function(mx_1x1_df) {
  standardise <- function(age, mx, w) {
    if (!setequal(age, 0:MAX_AGE) || any(!is.finite(mx))) return(NA_real_)
    sum(mx * w)
  }
  mx_1x1_df |>
    dplyr::inner_join(esp_weight_per_age(), by = "age") |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      asdr_female = standardise(.data$age, .data$mx_female, .data$weight_per_age),
      asdr_male = standardise(.data$age, .data$mx_male, .data$weight_per_age),
      asdr_total = standardise(.data$age, .data$mx_total, .data$weight_per_age),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year)
}

#' Validate computed central death rates
#'
#' Fails on negative, missing, or infinite rates; on rates >= 1 below age
#' 90; and on any province-year without unique, complete single ages
#' 0-100.
#'
#' @param mx_df Output of the `mx_1x1` element of [compute_death_rates()].
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @examples
#' deaths <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 1), female = c(1, 0), male = c(1, 1), total = c(2, 1)
#' )
#' exposure <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 1), female = c(500, 490), male = c(510, 505), total = c(1010, 995)
#' )
#' rates <- compute_death_rates(deaths, exposure)
#' validate_death_rates(rates$mx_1x1)
#' @export
validate_death_rates <- function(mx_df) {
  issues <- list()

  bad_vals <- mx_df |>
    dplyr::filter(
      .data$mx_female < 0 | .data$mx_male < 0 | .data$mx_total < 0 |
        !is.finite(.data$mx_female) | !is.finite(.data$mx_male) | !is.finite(.data$mx_total)
    )
  if (nrow(bad_vals) > 0) issues$invalid_rates <- bad_vals

  high_young <- mx_df |> dplyr::filter(.data$age < 90, (.data$mx_female >= 1 | .data$mx_male >= 1))
  if (nrow(high_young) > 0) issues$implausibly_high_rate_young_age <- high_young

  coverage <- mx_df |>
    dplyr::group_by(.data$nuts3_code, .data$year) |>
    dplyr::summarise(
      complete = !anyDuplicated(.data$age) && setequal(.data$age, 0:MAX_AGE), .groups = "drop"
    ) |>
    dplyr::filter(!.data$complete)
  if (nrow(coverage) > 0) issues$incomplete_age_coverage <- dplyr::select(coverage, -"complete")

  passed <- length(issues) == 0
  if (!passed) {
    warning("Death rate validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ": ",
            paste(names(issues), collapse = ", "), ".")
  } else {
    message("All death rate validation checks passed.")
  }

  list(passed = passed, issues = issues)
}

#' Compute central death rates for every province
#'
#' Computes 1x1 (single-year age/period), 5x1 (5-year age group), and
#' age-standardised (ESP 2013) death rates for every province present in
#' both `deaths` and `exposure`, per SHMD Protocol Section 12, Step 5.
#'
#' @param deaths Output of the `data_provinces` element of [get_ine_deaths()].
#' @param exposure Output of the `data` element of [compute_exposure()].
#' @section Missing data:
#' INE's age-specific deaths table omits cells with zero deaths, so an age
#' missing from a province-year that is otherwise present is treated as a
#' structural zero. An explicit `NA` count (unavailable or suppressed and not
#' recoverable) is kept as `NA` and propagates to the rate, the grouped rate,
#' and the ASDR, and makes [validate_death_rates()] fail. A province-year with
#' exposure but no death records at all is unavailable, not zero: it is
#' excluded with a warning.
#'
#' @return `list(mx_1x1, mx_5x1, asdr, qc)`:
#'   * `mx_1x1`: `nuts3_code`, `province_name`, `year`, `age`, central death
#'     rates `mx_female`, `mx_male`, `mx_total`, and the deaths (`d_*`) and
#'     exposures (`e_*`) they come from (needed by [build_life_tables()]).
#'   * `mx_5x1`: the same by five-year group (`age_group`), pooled as
#'     sum(deaths) / sum(exposure).
#'   * `asdr`: age-standardised death rates per 100,000 (European Standard
#'     Population 2013; the 95+ weight of 200 is spread evenly over ages
#'     95-99 and 100+). `NA` unless all ages 0-100 have finite rates.
#'   * `qc`: output of [validate_death_rates()] on `mx_1x1`.
#' @examples
#' deaths <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 1), female = c(1, 0), male = c(1, 1), total = c(2, 1)
#' )
#' exposure <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 1), female = c(500, 490), male = c(510, 505), total = c(1010, 995)
#' )
#' rates <- compute_death_rates(deaths, exposure)
#' rates$mx_1x1
#' rates$asdr
#' @export
compute_death_rates <- function(deaths, exposure) {
  provinces <- intersect(unique(deaths$nuts3_code), unique(exposure$nuts3_code))
  if (length(provinces) == 0) stop("No provinces in common between `deaths` and `exposure`.")

  unavailable <- dplyr::anti_join(
    dplyr::distinct(exposure, .data$nuts3_code, .data$year),
    dplyr::distinct(deaths, .data$nuts3_code, .data$year),
    by = c("nuts3_code", "year")
  ) |>
    dplyr::filter(.data$nuts3_code %in% provinces)
  if (nrow(unavailable) > 0) {
    warning(
      nrow(unavailable), " province-year(s) have exposure but no death records and are ",
      "excluded (treated as unavailable, not as zero deaths): ",
      paste(unavailable$nuts3_code, unavailable$year, collapse = ", "), "."
    )
  }

  compute_one <- function(nuts3) {
    d_df <- deaths |> dplyr::filter(.data$nuts3_code == nuts3)
    e_df <- exposure |> dplyr::filter(.data$nuts3_code == nuts3)
    province_name <- d_df$province_name[1]
    add_geo <- function(df) dplyr::mutate(df, nuts3_code = nuts3, province_name = province_name, .before = 1)

    mx_1x1 <- compute_mx_1x1_one(d_df, e_df)
    list(
      mx_1x1 = add_geo(mx_1x1),
      mx_5x1 = add_geo(compute_mx_5x1_one(mx_1x1)),
      asdr = add_geo(compute_asdr_one(mx_1x1))
    )
  }

  per_province <- purrr::map(provinces, compute_one)

  mx_1x1 <- purrr::map_dfr(per_province, "mx_1x1")
  mx_5x1 <- purrr::map_dfr(per_province, "mx_5x1")
  asdr <- purrr::map_dfr(per_province, "asdr")

  qc <- validate_death_rates(mx_1x1)
  if (!qc$passed) message("Validation issues detected in death rates (see returned $qc$issues).")

  list(mx_1x1 = mx_1x1, mx_5x1 = mx_5x1, asdr = asdr, qc = qc)
}
