#' @include helpers.R
NULL

# Parse + clean + validate helpers for get_ine_births(). Internal; see
# R/helpers.R for shared INE-fetch/province-matching helpers.

#' @noRd
parse_births <- function(raw_data) {
  if (is.null(raw_data) || length(raw_data) == 0) {
    warning("parse_births(): empty input")
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

    prov_raw <- nm |>
      stringr::str_split("\\.") |>
      purrr::pluck(1, 1) |>
      stringr::str_trim()

    data_points <- series$Data
    if (is.null(data_points) || length(data_points) == 0) return(tibble::tibble())

    purrr::map_dfr(data_points, function(dp) {
      tibble::tibble(
        province_name_raw = prov_raw, sex = sex,
        year = as.integer(dp$Anyo), births = as.numeric(dp$Valor)
      )
    })
  }

  tidy <- purrr::map_dfr(raw_data, parse_one_series)
  tidy$ine_code <- resolve_province_code(tidy$province_name_raw)

  tidy |>
    dplyr::filter(!is.na(.data$ine_code), !is.na(.data$sex)) |>
    dplyr::select("ine_code", "province_name_raw", "sex", "year", "births")
}

#' @noRd
clean_births <- function(tidy_df) {
  wide <- tidy_df |>
    dplyr::select("ine_code", "sex", "year", "births") |>
    tidyr::pivot_wider(names_from = "sex", values_from = "births", values_fn = sum) |>
    dplyr::rename_with(stringr::str_to_lower, dplyr::any_of(c("Female", "Male", "Total")))

  if (!"total" %in% names(wide)) {
    wide <- wide |> dplyr::mutate(total = .data$female + .data$male)
  } else {
    wide <- wide |>
      dplyr::mutate(
        total_check = .data$female + .data$male,
        total_mismatch = abs(.data$total - .data$total_check) > 0
      )
    n_mismatch <- sum(wide$total_mismatch, na.rm = TRUE)
    if (n_mismatch > 0) {
      warning(n_mismatch, " province-year rows: INE total != Female+Male. ",
              "Using INE's reported total; investigate before finalising.")
    }
    wide <- wide |> dplyr::select(-"total_check", -"total_mismatch")
  }

  wide |>
    dplyr::left_join(province_lookup, by = "ine_code") |>
    dplyr::filter(!is.na(.data$nuts3_code)) |>
    dplyr::select(
      "ine_code", "nuts3_code", "nuts2_code", "province_name", "year",
      "female", "male", "total"
    ) |>
    dplyr::arrange(.data$nuts3_code, .data$year)
}

#' Validate cleaned provincial birth data
#'
#' Checks: sex ratio at birth (SRB) implausibly far from the expected
#' value, accounting for sample size; no negative/missing counts; no
#' duplicate province-year rows; continuous year coverage.
#'
#' @details
#' The SRB check does not use a single fixed band (an earlier version
#' used \[1.030, 1.070\], which flagged ~60% of all province-years,
#' including 38% of Madrid/Barcelona's — clearly miscalibrated: annual
#' province-level SRB is naturally noisy for small birth counts, and this
#' data's own pooled SRB runs about 1.06, already above that band's
#' upper bound even before any noise). Instead it flags a province-year
#' only if *both* (a) the observed SRB is statistically implausible given
#' that year's birth count, assuming a true underlying SRB of 1.06 (a
#' two-tailed z-test on the male-birth proportion at *z* > 4, i.e. roughly
#' a 1-in-16,000 event under the null - deliberately strict, since
#' thousands of province-years are tested at once), and (b) the deviation
#' is also practically meaningful (SRB more than 0.03 from 1.06), so
#' statistically-significant-but-tiny deviations in large provinces
#' aren't flagged just because their sample size makes the test
#' hyper-sensitive. An SRB outside \[0.90, 1.25\] is additionally always
#' flagged as a sanity floor for gross data errors, but only once total
#' births reach 200: below that, even a large swing (e.g. 23 vs. 17
#' births) is unremarkable binomial noise, not a data problem, and the
#' z-test above already accounts for the small sample correctly.
#'
#' @param df Output of the `data` element of [get_ine_births()].
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @export
validate_births <- function(df) {
  issues <- list()

  srb_reference <- 1.06 # expected SRB (males per female); see @details
  srb_check <- df |>
    dplyr::mutate(
      total_births = .data$female + .data$male,
      srb = .data$male / .data$female,
      se = sqrt((srb_reference / (1 + srb_reference)) *
        (1 / (1 + srb_reference)) / .data$total_births),
      z = abs(.data$male / .data$total_births - srb_reference / (1 + srb_reference)) / .data$se
    ) |>
    dplyr::filter(
      !is.na(.data$srb),
      (.data$total_births >= 200 & (.data$srb < 0.90 | .data$srb > 1.25)) |
        (.data$z > 4 & abs(.data$srb - srb_reference) > 0.03)
    )
  if (nrow(srb_check) > 0) {
    issues$srb_out_of_range <- srb_check |>
      dplyr::select("nuts3_code", "province_name", "year", "female", "male", "srb")
  }

  bad_counts <- df |>
    dplyr::filter(
      .data$female < 0 | .data$male < 0 | .data$total < 0 |
        is.na(.data$female) | is.na(.data$male) | is.na(.data$total)
    )
  if (nrow(bad_counts) > 0) issues$invalid_counts <- bad_counts

  dupes <- df |>
    dplyr::count(.data$nuts3_code, .data$year) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dupes) > 0) issues$duplicate_rows <- dupes

  gaps <- df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name) |>
    dplyr::summarise(
      min_year = min(.data$year), max_year = max(.data$year), n_years = dplyr::n(),
      expected = .data$max_year - .data$min_year + 1, .groups = "drop"
    ) |>
    dplyr::filter(.data$n_years != .data$expected)
  if (nrow(gaps) > 0) issues$year_gaps <- gaps

  passed <- length(issues) == 0
  if (!passed) {
    warning("Births validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
  } else {
    message("All birth validation checks passed.")
  }

  list(passed = passed, issues = issues)
}

#' Retrieve province-level births
#'
#' Retrieves, cleans, and validates Spanish annual live-birth counts by
#' province and sex from INE's Tempus3 JSON API, per the SHMD Protocol
#' (Section 12, Step 1: "Birth Count Assembly"). Unlike [get_ine_demog()]
#' (which returns a `Total`-sex-only province count), this returns
#' sex-disaggregated counts joined to NUTS-3 codes, suitable as an
#' HMD-format Births input file - see [download_ine_data()] to write it to disk.
#'
#' @param table_id INE Tempus3 table ID. Default `6506`, confirmed for
#'   "Nacimientos por lugar de residencia de la madre y sexo. Total nacional
#'   y provincias" (operation MNPN). Pass `NULL` to auto-discover.
#' @param n_periods Number of most recent years to fetch.
#' @param use_cache Logical (default `TRUE`). If a previous call already
#'   cached a result for this `table_id`/`n_periods` combination, return
#'   it directly instead of re-fetching from INE — this function's live
#'   fetch is one of the package's slowest. Set `FALSE` to always hit the
#'   network.
#' @param force Logical (default `FALSE`). If `TRUE`, ignore any existing
#'   cache and re-fetch from INE, refreshing the cache afterwards.
#' @param cache_dir Directory for cached data. Default `NULL` means no
#'   persistent caching (each call re-fetches from INE). Pass a directory,
#'   e.g. `tools::R_user_dir("inedemogR", "cache")`, to enable caching.
#' @return `list(data, qc)`: `data` is a tibble with columns `ine_code`,
#'   `nuts3_code`, `nuts2_code`, `province_name`, `year`, `female`, `male`,
#'   `total`; `qc` is the output of [validate_births()].
#' @examples
#' \dontrun{
#' # Not run: requires live network access to the INE API.
#' births <- get_ine_births(n_periods = 30)
#' births$data
#' births$qc$issues
#'
#' # Re-fetch even if a cached copy exists:
#' births <- get_ine_births(n_periods = 30, force = TRUE)
#' }
#' @export
get_ine_births <- function(table_id = 6506, n_periods = 100,
                            use_cache = TRUE, force = FALSE,
                            cache_dir = NULL) {
  cache_key <- sprintf("births_%s_%s", table_id %||% "auto", n_periods)

  fetch <- function() {
    if (is.null(table_id)) {
      message("No table_id supplied - searching INE catalogue ...")
      candidates <- find_ine_table("MNPN", c("nacimiento", "provincia"))
      if (is.null(candidates) || nrow(candidates) == 0) {
        stop("Could not auto-discover the INE births table ID. Pass table_id explicitly.")
      }
      table_id <- candidates$Id[1]
      message("Using INE table_id = ", table_id)
    }

    message("Fetching births from INE (table ", table_id, ") ...")
    raw <- fetch_ine_json(table_id, n_periods = n_periods)
    if (is.null(raw)) stop("Failed to retrieve births data from INE API.")

    tidy <- parse_births(raw)
    if (nrow(tidy) == 0) stop("Parsed births data is empty - check table_id and API response structure.")

    clean <- clean_births(tidy)
    qc <- validate_births(clean)
    if (!qc$passed) {
      message("Validation issues detected in births data (see returned $qc$issues).")
    }

    list(data = clean, qc = qc)
  }

  with_ine_cache(cache_key, use_cache, force, cache_dir, fetch)
}

#' @noRd
parse_births_by_age <- function(raw_data) {
  if (is.null(raw_data) || length(raw_data) == 0) {
    warning("parse_births_by_age(): empty input")
    return(tibble::tibble())
  }

  parse_one_series <- function(series) {
    parts <- stringr::str_split(series$Nombre, "\\.")[[1]]
    prov_raw <- stringr::str_trim(parts[1])
    sex_newborn <- stringr::str_trim(parts[3])
    age_raw <- stringr::str_trim(parts[4])

    age <- dplyr::case_when(
      stringr::str_detect(age_raw, "Menos de") ~ NA_integer_,
      stringr::str_detect(age_raw, "y m") ~ 50L,
      TRUE ~ as.integer(stringr::str_extract(age_raw, "\\d+"))
    )
    age_group <- dplyr::case_when(
      stringr::str_detect(age_raw, "Menos de") ~ "under15",
      stringr::str_detect(age_raw, "y m") ~ "50plus",
      TRUE ~ as.character(age)
    )

    data_points <- series$Data
    if (is.null(data_points) || length(data_points) == 0) return(tibble::tibble())

    purrr::map_dfr(data_points, function(dp) {
      tibble::tibble(
        province_name_raw = prov_raw, sex_newborn = sex_newborn,
        age = age, age_group = age_group,
        year = as.integer(dp$Anyo), births = as.numeric(dp$Valor)
      )
    })
  }

  tidy <- purrr::map_dfr(raw_data, parse_one_series)
  tidy$ine_code <- resolve_province_code(tidy$province_name_raw)

  tidy |>
    dplyr::filter(!is.na(.data$ine_code), !is.na(.data$sex_newborn)) |>
    dplyr::select(
      "ine_code", "province_name_raw", "sex_newborn", "age", "age_group",
      "year", "births"
    )
}

#' @noRd
clean_births_by_age <- function(tidy_df) {
  # INE's age-of-mother table omits a (province, age_group, sex_newborn,
  # year) series entirely when its value is 0 across the whole fetched
  # window - the same "silently drops zero counts" behaviour previously
  # found and fixed in compute_death_rates() (R/death_rates.R). A naive
  # pivot_wider() here would leave those true zeros as NA instead, and
  # entire age groups can be missing for small provinces/years. Build the
  # complete (ine_code, age_group, sex_newborn, year) grid explicitly and
  # fill absent combinations with 0 births before pivoting.
  age_groups <- c("under15", as.character(15:49), "50plus")
  age_lookup <- tibble::tibble(
    age_group = age_groups, age = c(NA_integer_, 15:49, 50L)
  )

  complete_grid <- tidyr::expand_grid(
    ine_code = unique(tidy_df$ine_code),
    age_group = age_groups,
    sex_newborn = c("Total", "Hombres", "Mujeres"),
    year = sort(unique(tidy_df$year))
  )

  filled <- tidy_df |>
    dplyr::select("ine_code", "sex_newborn", "age_group", "year", "births") |>
    dplyr::right_join(complete_grid, by = c("ine_code", "sex_newborn", "age_group", "year")) |>
    dplyr::mutate(births = dplyr::coalesce(.data$births, 0))

  wide <- filled |>
    tidyr::pivot_wider(names_from = "sex_newborn", values_from = "births", values_fn = sum) |>
    dplyr::rename_with(stringr::str_to_lower, dplyr::any_of(c("Total", "Hombres", "Mujeres"))) |>
    dplyr::left_join(age_lookup, by = "age_group")

  wide |>
    dplyr::rename(male = "hombres", female = "mujeres") |>
    dplyr::left_join(province_lookup, by = "ine_code") |>
    dplyr::filter(!is.na(.data$nuts3_code)) |>
    dplyr::select(
      "ine_code", "nuts3_code", "nuts2_code", "province_name", "year",
      "age", "age_group", "female", "male", "total"
    ) |>
    dplyr::arrange(.data$nuts3_code, .data$year, .data$age_group)
}

#' Validate cleaned age-of-mother birth data
#'
#' Checks: no negative/missing counts; no duplicate province-year-age
#' rows; single-year ages (15-49) present for every province-year found
#' in the data (the two open intervals, `under15`/`50plus`, are not
#' required to be present since they are typically very small or zero).
#'
#' @param df Output of the `data` element of [get_ine_births_by_age()].
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @export
validate_births_by_age <- function(df) {
  issues <- list()

  bad_counts <- df |>
    dplyr::filter(
      .data$female < 0 | .data$male < 0 | .data$total < 0 |
        is.na(.data$female) | is.na(.data$male) | is.na(.data$total)
    )
  if (nrow(bad_counts) > 0) issues$invalid_counts <- bad_counts

  dupes <- df |>
    dplyr::count(.data$nuts3_code, .data$year, .data$age_group) |>
    dplyr::filter(.data$n > 1)
  if (nrow(dupes) > 0) issues$duplicate_rows <- dupes

  age_coverage <- df |>
    dplyr::filter(!is.na(.data$age), .data$age >= 15, .data$age <= 49) |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(n_ages = dplyr::n_distinct(.data$age), .groups = "drop") |>
    dplyr::filter(.data$n_ages != 35)
  if (nrow(age_coverage) > 0) issues$incomplete_age_coverage <- age_coverage

  passed <- length(issues) == 0
  if (!passed) {
    warning("Age-of-mother births validation flagged ", length(issues),
            " issue categor", if (length(issues) == 1) "y" else "ies", ".")
  } else {
    message("All age-of-mother birth validation checks passed.")
  }

  list(passed = passed, issues = issues)
}

#' Retrieve province-level births by age of mother
#'
#' Retrieves, cleans, and validates Spanish annual live-birth counts by
#' province, single year of age of the mother (15-49, plus the two open
#' intervals `under15`/`50plus`), and sex of the newborn, from INE's raw
#' Tempus3 JSON API. Unlike [get_ine_births()] (which has no age-of-mother
#' breakdown), this is the data source needed for age-specific fertility
#' rates - see [age_specific_fertility_rate()], [total_fertility_rate()],
#' [mean_age_at_childbearing()], [gross_reproduction_rate()], and
#' [net_reproduction_rate()].
#'
#' @param table_id INE Tempus3 table ID. Default `6508`, confirmed for
#'   "Nacimientos por lugar de residencia de la madre, sexo y edad de la
#'   madre. Total nacional y provincias" (operation MNPN).
#' @param n_periods Number of most recent years to fetch. This table has
#'   ~6,000 series (province x age x newborn-sex), so fetches are slower
#'   than [get_ine_births()]'s; caching (see `use_cache`) matters more here.
#' @param use_cache Logical (default `TRUE`). See [get_ine_births()].
#' @param force Logical (default `FALSE`). See [get_ine_births()].
#' @param cache_dir Directory for cached data. Default `NULL` means no
#'   persistent caching (each call re-fetches from INE). Pass a directory,
#'   e.g. `tools::R_user_dir("inedemogR", "cache")`, to enable caching.
#' @return `list(data, qc)`: `data` is a tibble with columns `ine_code`,
#'   `nuts3_code`, `nuts2_code`, `province_name`, `year`, `age` (integer,
#'   `NA` for the `under15` group), `age_group` (character: `"under15"`,
#'   `"15"`..`"49"`, `"50plus"`), `female`, `male`, `total` (births to
#'   mothers of that age, by sex of the newborn); `qc` is the output of
#'   [validate_births_by_age()].
#' @examples
#' \dontrun{
#' # Not run: requires live network access to the INE API.
#' births_age <- get_ine_births_by_age(n_periods = 10)
#' births_age$data
#' }
#' @export
get_ine_births_by_age <- function(table_id = 6508, n_periods = 100,
                                   use_cache = TRUE, force = FALSE,
                                   cache_dir = NULL) {
  cache_key <- sprintf("births_by_age_%s_%s", table_id, n_periods)

  fetch <- function() {
    message("Fetching births by age of mother from INE (table ", table_id, ") ...")
    raw <- fetch_ine_json(table_id, n_periods = n_periods)
    if (is.null(raw)) stop("Failed to retrieve age-of-mother births data from INE API.")

    tidy <- parse_births_by_age(raw)
    if (nrow(tidy) == 0) {
      stop("Parsed age-of-mother births data is empty - check table_id and API response structure.")
    }

    clean <- clean_births_by_age(tidy)
    qc <- validate_births_by_age(clean)
    if (!qc$passed) {
      message("Validation issues detected in age-of-mother births data (see returned $qc$issues).")
    }

    list(data = clean, qc = qc)
  }

  with_ine_cache(cache_key, use_cache, force, cache_dir, fetch)
}
