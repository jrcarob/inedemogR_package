#' @include helpers.R
NULL

# Central death rate computation for the SHMD pipeline, per SHMD Protocol
# Section 12, Step 5 (HMD Methods Protocol V6):
#   m(x,t)   = D(x,t) / E(x,t)                          (1x1)
#   m(x,n,t) = sum_k D(x+k,t) / sum_k E(x+k,t)           (nx1, pooled)
#   ASDR(t)  = sum_x m(x,t) * w(x)                       (ESP 2013 weights)

# Official Eurostat "European Standard Population 2013" 5-year age-group
# weights (per 100,000), used for age-standardised death rate computation.
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
  95, 99, 400,
  100, MAX_AGE, 200
)

#' @noRd
compute_mx_1x1_one <- function(deaths_df, exposure_df) {
  # INE's age-specific deaths table only publishes rows for age/year/sex
  # cells with at least one death; a missing row means zero deaths, not a
  # gap in coverage. Starting from exposure_df (derived from population,
  # which has a complete 0:MAX_AGE grid) and left-joining deaths preserves
  # every age instead of silently dropping zero-death ages, which would
  # otherwise break the age-contiguity that the life-table recursion
  # (build_life_table()) requires.
  exposure_df |>
    dplyr::rename(e_female = "female", e_male = "male", e_total = "total") |>
    dplyr::left_join(
      deaths_df |> dplyr::rename(d_female = "female", d_male = "male", d_total = "total"),
      by = c("year", "age")
    ) |>
    dplyr::mutate(
      dplyr::across(c("d_female", "d_male", "d_total"), ~ tidyr::replace_na(.x, 0)),
      mx_female = .data$d_female / .data$e_female,
      mx_male = .data$d_male / .data$e_male,
      mx_total = .data$d_total / .data$e_total
    ) |>
    dplyr::select("year", "age", "mx_female", "mx_male", "mx_total") |>
    dplyr::arrange(.data$year, .data$age)
}

#' @noRd
assign_age_group <- function(age) {
  ifelse(
    age >= MAX_AGE, sprintf("%d+", MAX_AGE),
    sprintf("%02d-%02d", (age %/% 5) * 5, (age %/% 5) * 5 + 4)
  )
}

#' @noRd
compute_mx_5x1_one <- function(deaths_df, exposure_df) {
  d5 <- deaths_df |>
    dplyr::mutate(age_group = assign_age_group(.data$age)) |>
    dplyr::group_by(.data$year, .data$age_group) |>
    dplyr::summarise(
      d_female = sum(.data$female), d_male = sum(.data$male), d_total = sum(.data$total),
      .groups = "drop"
    )

  e5 <- exposure_df |>
    dplyr::mutate(age_group = assign_age_group(.data$age)) |>
    dplyr::group_by(.data$year, .data$age_group) |>
    dplyr::summarise(
      e_female = sum(.data$female), e_male = sum(.data$male), e_total = sum(.data$total),
      .groups = "drop"
    )

  # As in compute_mx_1x1_one(), start from e5 (complete, from exposure) and
  # left-join d5 (which may be missing an age group entirely if every
  # single-year age within it had zero deaths), filling absent deaths with 0.
  e5 |>
    dplyr::left_join(d5, by = c("year", "age_group")) |>
    dplyr::mutate(
      dplyr::across(c("d_female", "d_male", "d_total"), ~ tidyr::replace_na(.x, 0)),
      mx_female = .data$d_female / .data$e_female,
      mx_male = .data$d_male / .data$e_male,
      mx_total = .data$d_total / .data$e_total
    ) |>
    dplyr::select("year", "age_group", "mx_female", "mx_male", "mx_total") |>
    dplyr::arrange(.data$year, .data$age_group)
}

#' @noRd
compute_asdr_one <- function(mx_1x1_df) {
  weight_for_age <- function(age) {
    row <- esp_2013_weights |>
      dplyr::filter(age >= .data$age_group_start, age <= .data$age_group_end)
    if (nrow(row) == 0) return(NA_real_)
    row$weight[1] / (row$age_group_end[1] - row$age_group_start[1] + 1)
  }

  age_weight_map <- tibble::tibble(age = 0:MAX_AGE) |>
    dplyr::mutate(weight_per_age = purrr::map_dbl(.data$age, weight_for_age))

  mx_1x1_df |>
    dplyr::left_join(age_weight_map, by = "age") |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      asdr_female = sum(.data$mx_female * .data$weight_per_age, na.rm = TRUE),
      asdr_male = sum(.data$mx_male * .data$weight_per_age, na.rm = TRUE),
      asdr_total = sum(.data$mx_total * .data$weight_per_age, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year)
}

#' Validate computed central death rates
#'
#' Checks: no negative/NA/infinite rates; no rates >= 1 for ages below 90
#' (flagged for review, not treated as a hard error).
#'
#' @param mx_df Output of the `mx_1x1` element of [compute_death_rates()].
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @export
validate_death_rates <- function(mx_df) {
  issues <- list()

  bad_vals <- mx_df |>
    dplyr::filter(
      .data$mx_female < 0 | .data$mx_male < 0 | .data$mx_total < 0 |
        is.na(.data$mx_female) | is.na(.data$mx_male) | is.na(.data$mx_total) |
        is.infinite(.data$mx_female) | is.infinite(.data$mx_male) | is.infinite(.data$mx_total)
    )
  if (nrow(bad_vals) > 0) issues$invalid_rates <- bad_vals

  high_young <- mx_df |> dplyr::filter(.data$age < 90, (.data$mx_female >= 1 | .data$mx_male >= 1))
  if (nrow(high_young) > 0) issues$implausibly_high_rate_young_age <- high_young

  passed <- length(issues) == 0
  if (!passed) {
    warning("Death rate validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
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
#' @return `list(mx_1x1, mx_5x1, asdr, qc)`, each a tibble with `nuts3_code`,
#'   `province_name` plus the rate columns; `qc` is the output of
#'   [validate_death_rates()] run on `mx_1x1`.
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

  compute_one <- function(nuts3) {
    d_df <- deaths |> dplyr::filter(.data$nuts3_code == nuts3)
    e_df <- exposure |> dplyr::filter(.data$nuts3_code == nuts3)
    province_name <- d_df$province_name[1]

    mx_1x1 <- compute_mx_1x1_one(d_df, e_df) |>
      dplyr::mutate(nuts3_code = nuts3, province_name = province_name, .before = 1)
    mx_5x1 <- compute_mx_5x1_one(d_df, e_df) |>
      dplyr::mutate(nuts3_code = nuts3, province_name = province_name, .before = 1)
    asdr <- compute_asdr_one(compute_mx_1x1_one(d_df, e_df)) |>
      dplyr::mutate(nuts3_code = nuts3, province_name = province_name, .before = 1)

    list(mx_1x1 = mx_1x1, mx_5x1 = mx_5x1, asdr = asdr)
  }

  per_province <- purrr::map(provinces, compute_one)

  mx_1x1 <- purrr::map_dfr(per_province, "mx_1x1")
  mx_5x1 <- purrr::map_dfr(per_province, "mx_5x1")
  asdr <- purrr::map_dfr(per_province, "asdr")

  qc <- validate_death_rates(mx_1x1)
  if (!qc$passed) message("Validation issues detected in death rates (see returned $qc$issues).")

  list(mx_1x1 = mx_1x1, mx_5x1 = mx_5x1, asdr = asdr, qc = qc)
}
