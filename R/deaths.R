#' @include helpers.R
NULL

# Parse + clean + validate helpers for get_ine_deaths(). Internal; see
# R/helpers.R for shared INE-fetch/province-matching helpers.

#' @noRd
parse_deaths <- function(raw_data) {
  if (is.null(raw_data) || length(raw_data) == 0) {
    warning("parse_deaths(): empty input")
    return(tibble::tibble())
  }

  parse_one_series <- function(series) {
    nm <- series$Nombre %||% NA_character_

    sex <- dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(nm), "mujer|hembra") ~ "Female",
      stringr::str_detect(stringr::str_to_lower(nm), "hombre|var[o\u00f3]n|varones") ~ "Male",
      stringr::str_detect(stringr::str_to_lower(nm), "ambos sexos|total") ~ "Total",
      TRUE ~ NA_character_
    )

    geo_raw <- nm |> stringr::str_split("\\.") |> purrr::pluck(1, 1) |> stringr::str_trim()

    data_points <- series$Data
    if (is.null(data_points) || length(data_points) == 0) return(tibble::tibble())

    purrr::map_dfr(data_points, function(dp) {
      tibble::tibble(
        province_name_raw = geo_raw, sex = sex,
        year = as.integer(dp$Anyo), deaths = as.numeric(dp$Valor)
      )
    })
  }

  tidy <- purrr::map_dfr(raw_data, parse_one_series)
  tidy$ine_code <- resolve_province_code(tidy$province_name_raw)

  is_national <- stringr::str_detect(
    normalize_province_name(tidy$province_name_raw),
    paste(NATIONAL_KEYWORDS, collapse = "|")
  )
  tidy$ine_code[is_national] <- "00"
  tidy$geo_type <- dplyr::if_else(is_national, "national", "province")

  tidy |>
    dplyr::filter(!is.na(.data$ine_code), !is.na(.data$sex)) |>
    dplyr::select("geo_type", "ine_code", "province_name_raw", "sex", "year", "deaths")
}

#' @noRd
parse_deaths_by_age <- function(raw_data) {
  if (is.null(raw_data) || length(raw_data) == 0) {
    warning("parse_deaths_by_age(): empty input")
    return(tibble::tibble())
  }

  parse_one_series <- function(series) {
    nm <- series$Nombre %||% NA_character_

    sex <- dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(nm), "mujer|hembra") ~ "Female",
      stringr::str_detect(stringr::str_to_lower(nm), "hombre|var[o\u00f3]n|varones") ~ "Male",
      stringr::str_detect(stringr::str_to_lower(nm), "ambos sexos|total") ~ "Total",
      TRUE ~ NA_character_
    )

    age <- dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(nm), "menores de un a[n\u00f1]o") ~ 0L,
      stringr::str_detect(stringr::str_to_lower(nm), "100 y m[a\u00e1]s") ~ MAX_AGE,
      TRUE ~ suppressWarnings(as.integer(
        stringr::str_extract(nm, "\\d{1,3}(?=\\s*a[n\u00f1]os?\\.)")
      ))
    )

    geo_raw <- nm |> stringr::str_split("\\.") |> purrr::pluck(1, 1) |> stringr::str_trim()

    data_points <- series$Data
    if (is.null(data_points) || length(data_points) == 0 || is.na(age)) return(tibble::tibble())

    purrr::map_dfr(data_points, function(dp) {
      tibble::tibble(
        province_name_raw = geo_raw, sex = sex, age = age,
        year = as.integer(dp$Anyo), deaths = as.numeric(dp$Valor)
      )
    })
  }

  tidy <- purrr::map_dfr(raw_data, parse_one_series)
  tidy$ine_code <- resolve_province_code(tidy$province_name_raw)

  tidy |>
    dplyr::filter(!is.na(.data$ine_code), !is.na(.data$sex), !is.na(.data$age)) |>
    dplyr::select("ine_code", "province_name_raw", "sex", "age", "year", "deaths")
}

#' @noRd
reconcile_total <- function(wide, mismatch_label) {
  if (!"total" %in% names(wide)) {
    return(dplyr::mutate(wide, total = .data$female + .data$male))
  }
  wide <- wide |>
    dplyr::mutate(
      total_check = .data$female + .data$male,
      total_mismatch = abs(.data$total - .data$total_check) > 0
    )
  n_mismatch <- sum(wide$total_mismatch, na.rm = TRUE)
  if (n_mismatch > 0) {
    warning(n_mismatch, " ", mismatch_label, " rows: INE total != Female+Male. ",
            "Using INE's reported total; investigate before finalising.")
  }
  dplyr::select(wide, -"total_check", -"total_mismatch")
}

#' @noRd
clean_deaths_provinces <- function(tidy_df) {
  wide <- tidy_df |>
    dplyr::filter(.data$geo_type == "province") |>
    dplyr::select("ine_code", "sex", "year", "deaths") |>
    tidyr::pivot_wider(names_from = "sex", values_from = "deaths", values_fn = sum) |>
    dplyr::rename_with(stringr::str_to_lower, dplyr::any_of(c("Female", "Male", "Total"))) |>
    reconcile_total("province-year")

  wide |>
    dplyr::left_join(province_lookup, by = "ine_code") |>
    dplyr::filter(!is.na(.data$nuts3_code)) |>
    dplyr::select(
      "ine_code", "nuts3_code", "nuts2_code", "province_name", "year",
      "female", "male", "total"
    ) |>
    dplyr::arrange(.data$nuts3_code, .data$year)
}

#' @noRd
clean_deaths_provinces_age <- function(tidy_df) {
  wide <- tidy_df |>
    dplyr::select("ine_code", "age", "sex", "year", "deaths") |>
    tidyr::pivot_wider(names_from = "sex", values_from = "deaths", values_fn = sum) |>
    dplyr::rename_with(stringr::str_to_lower, dplyr::any_of(c("Female", "Male", "Total")))

  # INE suppresses individual sex-specific counts for small-cell privacy but
  # still publishes the cell's Total; when exactly one of female/male is
  # missing and total is known, the suppressed value is exactly recoverable
  # by subtraction (deaths partition by sex with no other category).
  if (all(c("female", "male", "total") %in% names(wide))) {
    n_recovered <- sum(
      (is.na(wide$female) & !is.na(wide$male) & !is.na(wide$total)) |
        (is.na(wide$male) & !is.na(wide$female) & !is.na(wide$total))
    )
    if (n_recovered > 0) {
      message(n_recovered, " province-age-year cells recovered via Total - ",
              "other-sex subtraction (INE small-cell privacy suppression).")
    }
    wide <- wide |>
      dplyr::mutate(
        female = dplyr::if_else(is.na(.data$female) & !is.na(.data$male) & !is.na(.data$total),
                                 .data$total - .data$male, .data$female),
        male = dplyr::if_else(is.na(.data$male) & !is.na(.data$female) & !is.na(.data$total),
                               .data$total - .data$female, .data$male)
      )
  }

  wide <- reconcile_total(wide, "province-age-year")

  wide |>
    dplyr::left_join(province_lookup, by = "ine_code") |>
    dplyr::filter(!is.na(.data$nuts3_code)) |>
    dplyr::select(
      "ine_code", "nuts3_code", "nuts2_code", "province_name", "year", "age",
      "female", "male", "total"
    ) |>
    dplyr::arrange(.data$nuts3_code, .data$year, .data$age)
}

#' @noRd
clean_deaths_national <- function(tidy_df) {
  wide <- tidy_df |>
    dplyr::filter(.data$geo_type == "national") |>
    dplyr::select("sex", "year", "deaths") |>
    tidyr::pivot_wider(names_from = "sex", values_from = "deaths", values_fn = sum) |>
    dplyr::rename_with(stringr::str_to_lower, dplyr::any_of(c("Female", "Male", "Total")))

  if (nrow(wide) == 0) {
    warning("No national-total death series found in the data.")
    return(tibble::tibble())
  }

  wide |>
    reconcile_total("national year") |>
    dplyr::mutate(geo_code = "ES", geo_name = "Spain") |>
    dplyr::select("geo_code", "geo_name", "year", "female", "male", "total") |>
    dplyr::arrange(.data$year)
}

#' Validate cleaned provincial death data
#'
#' Checks: sex ratio at death in \[0.90, 1.50\]; no negative/missing
#' counts; no duplicate province-year rows; continuous year coverage;
#' provincial sum vs. national total (if `nat_df` supplied).
#'
#' @param prov_df Province-level cleaned deaths tibble.
#' @param nat_df National-level cleaned deaths tibble (optional).
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @export
validate_deaths <- function(prov_df, nat_df = tibble::tibble()) {
  issues <- list()

  srd_check <- prov_df |>
    dplyr::mutate(srd = .data$male / .data$female) |>
    dplyr::filter(!is.na(.data$srd), (.data$srd < 0.90 | .data$srd > 1.50))
  if (nrow(srd_check) > 0) {
    issues$srd_out_of_range <- srd_check |>
      dplyr::select("nuts3_code", "province_name", "year", "female", "male", "srd")
  }

  bad_counts <- prov_df |>
    dplyr::filter(
      .data$female < 0 | .data$male < 0 | .data$total < 0 |
        is.na(.data$female) | is.na(.data$male) | is.na(.data$total)
    )
  if (nrow(bad_counts) > 0) issues$invalid_counts <- bad_counts

  dupes <- prov_df |> dplyr::count(.data$nuts3_code, .data$year) |> dplyr::filter(.data$n > 1)
  if (nrow(dupes) > 0) issues$duplicate_rows <- dupes

  gaps <- prov_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name) |>
    dplyr::summarise(
      min_year = min(.data$year), max_year = max(.data$year), n_years = dplyr::n(),
      expected = .data$max_year - .data$min_year + 1, .groups = "drop"
    ) |>
    dplyr::filter(.data$n_years != .data$expected)
  if (nrow(gaps) > 0) issues$year_gaps <- gaps

  if (nrow(nat_df) > 0) {
    prov_sum <- prov_df |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(prov_total = sum(.data$total, na.rm = TRUE), .groups = "drop")
    nat_check <- nat_df |>
      dplyr::select("year", nat_total = "total") |>
      dplyr::inner_join(prov_sum, by = "year") |>
      dplyr::mutate(rel_diff = abs(.data$nat_total - .data$prov_total) / .data$nat_total) |>
      dplyr::filter(.data$rel_diff > 0.01)
    if (nrow(nat_check) > 0) issues$national_vs_provincial_mismatch <- nat_check
  }

  passed <- length(issues) == 0
  if (!passed) {
    warning("Deaths validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
  } else {
    message("All death validation checks passed.")
  }

  list(passed = passed, issues = issues)
}

#' Validate cleaned age-specific provincial death data
#'
#' Checks: no negative/missing counts; no duplicate province-age-year rows;
#' continuous year coverage per province.
#'
#' @param df Age-specific province deaths tibble.
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @export
validate_deaths_age <- function(df) {
  issues <- list()

  bad_counts <- df |>
    dplyr::filter(
      .data$female < 0 | .data$male < 0 | .data$total < 0 |
        is.na(.data$female) | is.na(.data$male) | is.na(.data$total)
    )
  if (nrow(bad_counts) > 0) issues$invalid_counts <- bad_counts

  dupes <- df |>
    dplyr::count(.data$nuts3_code, .data$year, .data$age) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dupes) > 0) issues$duplicate_rows <- dupes

  gaps <- df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name) |>
    dplyr::summarise(
      min_year = min(.data$year), max_year = max(.data$year),
      n_years = dplyr::n_distinct(.data$year),
      expected = .data$max_year - .data$min_year + 1, .groups = "drop"
    ) |>
    dplyr::filter(.data$n_years != .data$expected)
  if (nrow(gaps) > 0) issues$year_gaps <- gaps

  passed <- length(issues) == 0
  if (!passed) {
    warning("Age-specific deaths validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
  } else {
    message("All age-specific deaths validation checks passed.")
  }

  list(passed = passed, issues = issues)
}

#' Retrieve province-level deaths
#'
#' Retrieves, cleans, and validates Spanish annual death counts by province
#' and sex from INE's Tempus3 JSON API, per the SHMD Protocol (Section 12,
#' Step 1: "Death Count Assembly"). Fetches both an age-less table (for the
#' national total and a national-vs-provincial reconciliation check) and an
#' age-specific table (needed by [compute_exposure()] /
#' [compute_death_rates()], which require D(x,t), not just D(t)).
#'
#' @param table_id INE table ID for the age-less (province + sex) series.
#'   Default `6545`, confirmed for "Defunciones por lugar de residencia y
#'   sexo. Total nacional y provincias" (operation MNPD).
#' @param age_table_id INE table ID for the age-specific series. Default
#'   `6547`.
#' @param n_periods Number of most recent years to fetch.
#' @param use_cache Logical (default `TRUE`). If a previous call already
#'   cached a result for this `table_id`/`age_table_id`/`n_periods`
#'   combination, return it directly instead of re-fetching from INE —
#'   this function's live fetch (two tables) is one of the package's
#'   slowest. Set `FALSE` to always hit the network.
#' @param force Logical (default `FALSE`). If `TRUE`, ignore any existing
#'   cache and re-fetch from INE, refreshing the cache afterwards.
#' @param cache_dir Directory for cached data. Defaults to the same
#'   location [update_ine_data()] uses.
#' @return `list(data_provinces, data_national, qc, qc_age)`:
#'   `data_provinces` is age-specific (`ine_code`, `nuts3_code`,
#'   `nuts2_code`, `province_name`, `year`, `age`, `female`, `male`,
#'   `total`); `data_national` is Spain's age-less total.
#' @examples
#' \dontrun{
#' # Not run: requires live network access to the INE API.
#' deaths <- get_ine_deaths(n_periods = 30)
#' deaths$data_provinces
#'
#' # Re-fetch even if a cached copy exists:
#' deaths <- get_ine_deaths(n_periods = 30, force = TRUE)
#' }
#' @export
get_ine_deaths <- function(table_id = 6545, age_table_id = 6547, n_periods = 100,
                            use_cache = TRUE, force = FALSE,
                            cache_dir = tools::R_user_dir("inedemogR", "cache")) {
  cache_key <- sprintf("deaths_%s_%s_%s", table_id, age_table_id, n_periods)

  fetch <- function() {
    message("Fetching deaths from INE (table ", table_id, ") ...")
    raw <- fetch_ine_json(table_id, n_periods = n_periods)
    if (is.null(raw)) stop("Failed to retrieve deaths data from INE API.")

    tidy <- parse_deaths(raw)
    if (nrow(tidy) == 0) stop("Parsed deaths data is empty - check table_id and API response structure.")

    clean_prov <- clean_deaths_provinces(tidy)
    clean_nat <- clean_deaths_national(tidy)
    qc <- validate_deaths(clean_prov, clean_nat)
    if (!qc$passed) message("Validation issues detected in deaths data (see returned $qc$issues).")

    message("Fetching age-specific deaths from INE (table ", age_table_id, ") ...")
    raw_age <- fetch_ine_json(age_table_id, n_periods = n_periods)
    if (is.null(raw_age)) stop("Failed to retrieve age-specific deaths data from INE API.")

    tidy_age <- parse_deaths_by_age(raw_age)
    if (nrow(tidy_age) == 0) {
      stop("Parsed age-specific deaths data is empty - check age_table_id and API response structure.")
    }

    clean_prov_age <- clean_deaths_provinces_age(tidy_age)
    qc_age <- validate_deaths_age(clean_prov_age)
    if (!qc_age$passed) {
      message("Validation issues detected in age-specific deaths data (see returned $qc_age$issues).")
    }

    list(data_provinces = clean_prov_age, data_national = clean_nat, qc = qc, qc_age = qc_age)
  }

  with_ine_cache(cache_key, use_cache, force, cache_dir, fetch)
}
