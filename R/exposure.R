#' @include helpers.R
NULL

# Period exposure-to-risk, HMD Methods Protocol V6 Eq. (57), assuming births
# are uniformly distributed within cohorts:
#
#   E(x,t) = 1/2*[P(x,t) + P(x,t+1)] + 1/6*[D_L(x,t) - D_U(x,t)]
#
# D_L: lower triangle, cohort born in t-x (reaches age x during year t).
# D_U: upper triangle, cohort born in t-x-1 (already aged x on 1 January).
# INE deaths carry no cohort, so they are split evenly between triangles and
# the correction term vanishes: E(x,t) is the mean of the January-1 stocks.

#' @noRd
split_lexis_triangles <- function(deaths_df) {
  has_cohort <- "cohort" %in% names(deaths_df)

  if (has_cohort) {
    message("Using true Lexis-triangle split from cohort column.")
    deaths_df |>
      dplyr::mutate(
        triangle = dplyr::case_when(
          .data$cohort == .data$year - .data$age ~ "lower",
          .data$cohort == .data$year - .data$age - 1 ~ "upper",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::filter(!is.na(.data$triangle)) |>
      dplyr::group_by(.data$nuts3_code, .data$year, .data$age, .data$triangle) |>
      dplyr::summarise(
        female = sum(.data$female), male = sum(.data$male), total = sum(.data$total),
        .groups = "drop"
      ) |>
      tidyr::pivot_wider(
        names_from = "triangle", values_from = c("female", "male", "total"), values_fill = 0
      ) |>
      dplyr::rename(
        d_upper_female = "female_upper", d_lower_female = "female_lower",
        d_upper_male = "male_upper", d_lower_male = "male_lower",
        d_upper_total = "total_upper", d_lower_total = "total_lower"
      )
  } else {
    warning(
      "No cohort column in `deaths`: splitting deaths evenly between Lexis ",
      "triangles, so exposure is the mean of the two January-1 populations ",
      "(the HMD V6 Eq. 57 triangle correction is zero)."
    )
    deaths_df |>
      dplyr::mutate(
        d_upper_female = .data$female / 2, d_lower_female = .data$female / 2,
        d_upper_male = .data$male / 2, d_lower_male = .data$male / 2,
        d_upper_total = .data$total / 2, d_lower_total = .data$total / 2
      ) |>
      dplyr::select(
        "nuts3_code", "year", "age", dplyr::starts_with("d_upper"), dplyr::starts_with("d_lower")
      )
  }
}

#' @noRd
compute_exposure_one <- function(pop_df, deaths_df) {
  years_available <- unique(pop_df$year)

  pop_t <- pop_df |>
    dplyr::select("year", "age", p_female = "female", p_male = "male", p_total = "total")
  pop_t1 <- pop_df |>
    dplyr::mutate(year = .data$year - 1L) |>
    dplyr::select("year", "age", p1_female = "female", p1_male = "male", p1_total = "total")

  merged <- pop_t |>
    dplyr::left_join(pop_t1, by = c("year", "age")) |>
    dplyr::left_join(deaths_df, by = c("year", "age"))

  merged |>
    dplyr::mutate(
      is_boundary_year = !(.data$year + 1L) %in% years_available,
      p1_female = ifelse(.data$is_boundary_year, .data$p_female, .data$p1_female),
      p1_male = ifelse(.data$is_boundary_year, .data$p_male, .data$p1_male),
      p1_total = ifelse(.data$is_boundary_year, .data$p_total, .data$p1_total),
      dplyr::across(dplyr::starts_with("d_upper"), ~ tidyr::replace_na(.x, 0)),
      dplyr::across(dplyr::starts_with("d_lower"), ~ tidyr::replace_na(.x, 0)),
      female = 0.5 * (.data$p_female + .data$p1_female) + (.data$d_lower_female - .data$d_upper_female) / 6,
      male = 0.5 * (.data$p_male + .data$p1_male) + (.data$d_lower_male - .data$d_upper_male) / 6,
      total = 0.5 * (.data$p_total + .data$p1_total) + (.data$d_lower_total - .data$d_upper_total) / 6,
      is_missing_source_pop = is.na(.data$female) | is.na(.data$male) | is.na(.data$total)
    ) |>
    dplyr::select(
      "year", "age", "female", "male", "total", "is_boundary_year", "is_missing_source_pop"
    ) |>
    dplyr::arrange(.data$year, .data$age)
}

#' Validate computed exposure-to-risk values
#'
#' Checks: cells with no usable source population (flagged for audit, not
#' treated as an error); no other negative/NA exposures; exposure should
#' not exceed 2x population (a generous plausibility bound).
#'
#' @param exposure_df Output of the `data` element of [compute_exposure()].
#' @param population_df The population tibble originally passed in.
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @examples
#' exposure_df <- tibble::tibble(
#'   nuts3_code = "ES611", year = 2020, age = c(0, 1),
#'   female = c(495, 480), male = c(520, 500), total = c(1015, 980),
#'   is_boundary_year = FALSE, is_missing_source_pop = FALSE
#' )
#' population_df <- tibble::tibble(
#'   nuts3_code = "ES611", year = 2020, age = c(0, 1), total = c(1000, 970)
#' )
#' validate_exposure(exposure_df, population_df)
#' @export
validate_exposure <- function(exposure_df, population_df) {
  issues <- list()

  missing_source <- exposure_df |> dplyr::filter(.data$is_missing_source_pop)
  if (nrow(missing_source) > 0) {
    issues$missing_source_population <- missing_source |>
      dplyr::select("nuts3_code", "year", "age", "is_boundary_year")
  }

  bad_vals <- exposure_df |>
    dplyr::filter(
      !.data$is_missing_source_pop,
      .data$female < 0 | .data$male < 0 | .data$total < 0 |
        is.na(.data$female) | is.na(.data$male) | is.na(.data$total)
    )
  if (nrow(bad_vals) > 0) issues$invalid_exposure <- bad_vals

  plausibility <- exposure_df |>
    dplyr::filter(!.data$is_missing_source_pop) |>
    dplyr::left_join(
      population_df |> dplyr::select("nuts3_code", "year", "age", p_total = "total"),
      by = c("nuts3_code", "year", "age")
    ) |>
    dplyr::filter(.data$total > 2 * .data$p_total)
  if (nrow(plausibility) > 0) {
    issues$implausible_exposure <- plausibility |>
      dplyr::select("nuts3_code", "year", "age", "total", "p_total")
  }

  passed <- length(issues) == 0
  if (!passed) {
    warning("Exposure validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
  } else {
    message("All exposure validation checks passed.")
  }

  list(passed = passed, issues = issues)
}

#' Compute exposure-to-risk for every province
#'
#' Computes E(x,t) = 1/2\[P(x,t) + P(x,t+1)\] + 1/6\[D_L(x,t) - D_U(x,t)\]
#' (HMD Methods Protocol V6, Eq. 57) for every province present in both
#' `population` and `deaths`, where `D_L` and `D_U` are deaths in the lower
#' (cohort born in `t - x`) and upper (cohort born in `t - x - 1`) Lexis
#' triangles.
#'
#' INE's death tables are not classified by birth cohort. Unless `deaths`
#' carries a `cohort` column, deaths are split evenly between the two
#' triangles, the correction term is zero, and exposure is the mean of the
#' two January-1 populations. This is an adaptation of the HMD protocol,
#' which splits 1x1 deaths by regression (Appendix A) and corrects for the
#' monthly distribution of births (Appendix E); its effect is quantified in
#' the package's validation study (see `reproducibility/`).
#'
#' If `P(x,t+1)` is unavailable for a year that is kept, exposure falls back
#' to `P(x,t)` and the row is flagged with `is_boundary_year = TRUE`.
#'
#' Death registration lags population estimates by about a year, so
#' `population` commonly includes a most-recent year (e.g. a Jan-1 stock
#' snapshot) with no corresponding rows anywhere in `deaths` yet. That
#' year is still used as the `P(x,t+1)` boundary for computing the
#' *previous* year's exposure, but is **not** included in the returned
#' exposure rows itself — treating an entirely-absent year as zero deaths
#' (rather than missing data) would mechanically produce zero mortality
#' for that year, and downstream a nonsensical inflated life expectancy.
#'
#' @param population Output of the `data` element of [get_ine_population()].
#' @param deaths Output of the `data_provinces` element of [get_ine_deaths()].
#' @return `list(data, qc)`: `data` is a tibble with columns `nuts3_code`,
#'   `province_name`, `year`, `age`, `female`, `male`, `total` (exposure),
#'   `is_boundary_year`, `is_missing_source_pop`; `qc` is the output of
#'   [validate_exposure()].
#' @examples
#' population <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna",
#'   year = c(2022, 2023), age = 0,
#'   female = c(100, 105), male = c(105, 110), total = c(205, 215)
#' )
#' deaths <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna",
#'   year = c(2022, 2023), age = 0,
#'   female = c(1, 1), male = c(1, 2), total = c(2, 3)
#' )
#' compute_exposure(population, deaths)
#' @export
compute_exposure <- function(population, deaths) {
  lexis <- split_lexis_triangles(deaths)

  provinces <- intersect(unique(population$nuts3_code), unique(deaths$nuts3_code))
  if (length(provinces) == 0) {
    stop("No provinces in common between `population` and `deaths`.")
  }

  years_without_deaths <- setdiff(unique(population$year), unique(deaths$year))
  if (length(years_without_deaths) > 0) {
    message(
      "Excluding year(s) ", paste(sort(years_without_deaths), collapse = ", "),
      " from the returned exposure: present in `population` but absent from ",
      "`deaths` (death registration for the most recent year is typically ",
      "incomplete). Still used as the P(x,t+1) boundary for the prior year."
    )
  }

  results <- purrr::map_dfr(provinces, function(nuts3) {
    pop_df <- population |> dplyr::filter(.data$nuts3_code == nuts3)
    deaths_lexis_df <- lexis |>
      dplyr::filter(.data$nuts3_code == nuts3) |>
      dplyr::select("year", "age", dplyr::starts_with("d_upper"), dplyr::starts_with("d_lower"))

    exposure <- compute_exposure_one(pop_df, deaths_lexis_df)
    exposure$nuts3_code <- nuts3
    exposure$province_name <- pop_df$province_name[1]
    exposure
  })

  results <- results |>
    dplyr::filter(!.data$year %in% years_without_deaths) |>
    dplyr::select(
      "nuts3_code", "province_name", "year", "age", "female", "male", "total",
      "is_boundary_year", "is_missing_source_pop"
    )

  qc <- validate_exposure(results, population)
  if (!qc$passed) message("Validation issues detected in exposure data (see returned $qc$issues).")

  list(data = results, qc = qc)
}
