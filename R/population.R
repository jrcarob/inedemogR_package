#' @include helpers.R
NULL

# Parse + clean + validate helpers for get_ine_population(). Internal;
# see R/helpers.R for shared INE-fetch/province-matching helpers.

#' @noRd
parse_population <- function(raw_data) {
  if (is.null(raw_data) || length(raw_data) == 0) {
    warning("parse_population(): empty input")
    return(tibble::tibble())
  }

  parse_one_series <- function(series) {
    nm <- series$Nombre %||% NA_character_
    ine_code <- stringr::str_extract(nm, "^\\d{2}")

    sex <- dplyr::case_when(
      stringr::str_detect(stringr::str_to_lower(nm), "mujer|hembra") ~ "Female",
      stringr::str_detect(stringr::str_to_lower(nm), "hombre|var[o\u00f3]n|varones") ~ "Male",
      stringr::str_detect(stringr::str_to_lower(nm), "ambos sexos|total") ~ "Total",
      TRUE ~ NA_character_
    )

    age_chr <- stringr::str_extract(nm, "\\d{1,3}(?=\\s*a[n\u00f1]o)")
    is_open_interval <- stringr::str_detect(stringr::str_to_lower(nm), "y m[a\u00e1]s")
    age <- if (is_open_interval) MAX_AGE else suppressWarnings(as.integer(age_chr))

    data_points <- series$Data
    if (is.null(data_points) || length(data_points) == 0 || is.na(age)) return(tibble::tibble())

    purrr::map_dfr(data_points, function(dp) {
      tibble::tibble(
        ine_code = ine_code, age = age, sex = sex,
        year = as.integer(dp$Anyo), population = as.numeric(dp$Valor)
      )
    })
  }

  purrr::map_dfr(raw_data, parse_one_series) |>
    dplyr::filter(!is.na(.data$ine_code), !is.na(.data$sex), !is.na(.data$age))
}

#' @noRd
parse_population_csv <- function(csv_df, n_periods = 30) {
  if (is.null(csv_df) || nrow(csv_df) == 0) {
    warning("parse_population_csv(): empty input")
    return(tibble::tibble())
  }

  # The "Edad simple" column mixes single-year ages (0..99), the true HMD
  # open interval ("100 y mas anos"), the "Todas las edades" grand total
  # (excluded below), AND at least one redundant coarser open interval
  # ("85 y mas anos") that overlaps single-year ages 85-99 and would
  # double-count if merged into the same bucket as "100 y mas anos" - so
  # only the exact "100 y mas anos" label maps to MAX_AGE; any other
  # "y mas" aggregate is dropped.
  parsed <- csv_df |>
    dplyr::filter(
      stringr::str_detect(.data[["Periodo"]], "de enero de"),
      .data[["Edad simple"]] != "Todas las edades"
    ) |>
    dplyr::transmute(
      ine_code = stringr::str_extract(.data[["Provincias"]], "^\\d{2}"),
      sex = dplyr::case_when(
        .data[["Sexo"]] == "Hombres" ~ "Male",
        .data[["Sexo"]] == "Mujeres" ~ "Female",
        .data[["Sexo"]] == "Total" ~ "Total",
        TRUE ~ NA_character_
      ),
      year = as.integer(stringr::str_extract(.data[["Periodo"]], "\\d{4}$")),
      age = dplyr::case_when(
        .data[["Edad simple"]] == "100 y m\u00e1s a\u00f1os" ~ MAX_AGE,
        stringr::str_detect(.data[["Edad simple"]], "y m[a\u00e1]s") ~ NA_integer_,
        TRUE ~ suppressWarnings(as.integer(stringr::str_extract(.data[["Edad simple"]], "^\\d{1,3}")))
      ),
      population = as.numeric(stringr::str_remove_all(.data[["Total"]], "\\."))
    ) |>
    dplyr::filter(!is.na(.data$ine_code), !is.na(.data$sex), !is.na(.data$age))

  keep_years <- parsed$year |> unique() |> sort(decreasing = TRUE) |> utils::head(n_periods)
  parsed |> dplyr::filter(.data$year %in% keep_years)
}

#' @noRd
clean_population <- function(tidy_df) {
  wide <- tidy_df |>
    dplyr::select("ine_code", "age", "sex", "year", "population") |>
    tidyr::pivot_wider(names_from = "sex", values_from = "population", values_fn = sum) |>
    dplyr::rename_with(stringr::str_to_lower, dplyr::any_of(c("Female", "Male", "Total")))

  if (!"total" %in% names(wide)) {
    wide <- wide |> dplyr::mutate(total = .data$female + .data$male)
  } else {
    wide <- wide |>
      dplyr::mutate(
        total_check = .data$female + .data$male,
        total_mismatch = abs(.data$total - .data$total_check) > 1
      )
    n_mismatch <- sum(wide$total_mismatch, na.rm = TRUE)
    if (n_mismatch > 0) {
      warning(n_mismatch, " province-age-year rows: INE total != Female+Male. ",
              "Using INE's reported total; investigate before finalising.")
    }
    wide <- wide |> dplyr::select(-"total_check", -"total_mismatch")
  }

  wide |>
    dplyr::left_join(province_lookup, by = "ine_code") |>
    dplyr::filter(!is.na(.data$nuts3_code)) |>
    dplyr::select(
      "ine_code", "nuts3_code", "nuts2_code", "province_name", "year", "age",
      "female", "male", "total"
    ) |>
    dplyr::arrange(.data$nuts3_code, .data$year, .data$age)
}

#' Validate cleaned provincial population data
#'
#' Checks: no negative/missing counts; full age range (0..100) present per
#' province-year; no duplicate province-age-year rows; continuous year
#' coverage; no implausible age-to-age jumps (>50%, informational only).
#'
#' @param df Output of the `data` element of [get_ine_population()].
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @export
validate_population <- function(df) {
  issues <- list()

  bad_counts <- df |>
    dplyr::filter(
      .data$female < 0 | .data$male < 0 | .data$total < 0 |
        is.na(.data$female) | is.na(.data$male) | is.na(.data$total)
    )
  if (nrow(bad_counts) > 0) issues$invalid_counts <- bad_counts

  age_coverage <- df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(n_ages = dplyr::n_distinct(.data$age), .groups = "drop") |>
    dplyr::filter(.data$n_ages != (MAX_AGE + 1))
  if (nrow(age_coverage) > 0) issues$incomplete_age_range <- age_coverage

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

  jumps <- df |>
    dplyr::arrange(.data$nuts3_code, .data$year, .data$age) |>
    dplyr::group_by(.data$nuts3_code, .data$year) |>
    dplyr::mutate(pct_change = (.data$total - dplyr::lag(.data$total)) / dplyr::lag(.data$total)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$age > 0, .data$age < MAX_AGE, abs(.data$pct_change) > 0.5)
  if (nrow(jumps) > 0) {
    issues$large_age_jumps <- jumps |>
      dplyr::select("nuts3_code", "province_name", "year", "age", "total", "pct_change")
  }

  passed <- length(issues) == 0
  if (!passed) {
    warning("Population validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
  } else {
    message("All population validation checks passed.")
  }

  list(passed = passed, issues = issues)
}

#' Retrieve province-level population by age and sex
#'
#' Retrieves, cleans, and validates Spanish January-1st provincial
#' population estimates by single-year age and sex from INE's Padron
#' Municipal Continuo, per the SHMD Protocol (Section 12, Step 3:
#' "Population Estimates"). Covers 1996-present; pre-1996 years require the
#' intercensal survival method applied to the 1981/1991 censuses, which is
#' out of scope here.
#'
#' Some province x age x sex population tables are too large for INE's JSON
#' API and are rejected outright; this function automatically falls back to
#' INE's full CSV bulk export in that case (a larger download, but with no
#' series-count limit).
#'
#' @param table_id INE table ID. Default `56945` ("Poblacion residente por
#'   fecha, sexo y edad (desde 1971)", provincial variant, operation ECP).
#'   Pass `NULL` to auto-discover.
#' @param n_periods Number of most recent years to fetch.
#' @param use_cache Logical (default `TRUE`). If a previous call already
#'   cached a result for this `table_id`/`n_periods` combination, return
#'   it directly instead of re-fetching from INE, since this function's live
#'   fetch (often via the slower CSV bulk-export fallback) is one of the
#'   package's slowest. Set `FALSE` to always hit the network.
#' @param force Logical (default `FALSE`). If `TRUE`, ignore any existing
#'   cache and re-fetch from INE, refreshing the cache afterwards.
#' @param cache_dir Directory for cached data. Defaults to the same
#'   location [update_ine_data()] uses.
#' @return `list(data, qc)`: `data` is a tibble with columns `ine_code`,
#'   `nuts3_code`, `nuts2_code`, `province_name`, `year`, `age`, `female`,
#'   `male`, `total`; `qc` is the output of [validate_population()].
#' @examples
#' \dontrun{
#' # Not run: requires live network access to the INE API.
#' pop <- get_ine_population(n_periods = 10)
#' pop$data
#'
#' # Re-fetch even if a cached copy exists:
#' pop <- get_ine_population(n_periods = 10, force = TRUE)
#' }
#' @export
get_ine_population <- function(table_id = 56945, n_periods = 30,
                                use_cache = TRUE, force = FALSE,
                                cache_dir = tools::R_user_dir("inedemogR", "cache")) {
  cache_key <- sprintf("population_%s_%s", table_id %||% "auto", n_periods)

  fetch <- function() {
    if (is.null(table_id)) {
      message("No table_id supplied - searching INE catalogue ...")
      candidates <- find_ine_table("pmc", c("poblaci[o\u00f3]n", "provincia", "edad", "sexo"))
      if (is.null(candidates) || nrow(candidates) == 0) {
        stop("Could not auto-discover the INE population table ID. Pass table_id explicitly.")
      }
      table_id <- candidates$Id[1]
      message("Using INE table_id = ", table_id)
    }

    message("Fetching population from INE (table ", table_id, ") ...")
    raw <- fetch_ine_json(table_id, n_periods = n_periods)

    tidy <- if (!is.null(raw)) parse_population(raw) else tibble::tibble()

    if (nrow(tidy) == 0) {
      message("JSON API path unavailable for table ", table_id, " - falling back ",
              "to INE's full CSV bulk export ...")
      csv_raw <- fetch_ine_csv(table_id)
      if (is.null(csv_raw)) {
        stop("Failed to retrieve population data from INE (both the JSON API ",
             "and the CSV bulk export failed).")
      }
      tidy <- parse_population_csv(csv_raw, n_periods = n_periods)
    }

    if (nrow(tidy) == 0) stop("Parsed population data is empty - check table_id and API/CSV response structure.")

    clean <- clean_population(tidy)
    qc <- validate_population(clean)
    if (!qc$passed) message("Validation issues detected in population data (see returned $qc$issues).")

    list(data = clean, qc = qc)
  }

  with_ine_cache(cache_key, use_cache, force, cache_dir, fetch)
}
