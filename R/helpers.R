# Internal helpers shared by the pipeline functions (R/births.R,
# R/deaths.R, R/population.R, R/exposure.R,
# R/death_rates.R, R/life_tables.R).
#
# These implement the SHMD (Spanish subnational Human Mortality Database)
# protocol pipeline: retrieve province-level births/deaths/population from
# INE's raw Tempus3 JSON API (not via ineapir — Tempus3's `tip = "AM"` series
# names/metadata shape is parsed directly here, which predates and differs
# from the ineapir::get_data_table() registry used by get_ine_demog()),
# then derive exposure-to-risk, central death rates, and period life tables
# per HMD Methods Protocol V6.

MAX_AGE <- 100L        # HMD convention: ages 0..99 single-year, "100" = 100+ open interval
LIFE_TABLE_MAX <- 110L # HMD convention: life tables extend to 110+
KANNISTO_FIT_MIN <- 80L
KANNISTO_FIT_MAX <- 95L

INE_API_BASE <- "https://servicios.ine.es/wstempus/js/ES"

# Official INE provincia codes (01-52) mapped to Eurostat NUTS-3 codes and
# NUTS-2 (CC.AA.) parent codes, per SHMD Protocol Section 10.3. INE reports
# the Balearic and Canary Islands as single provinces (07, 35, 38) even
# though Eurostat NUTS-3 subdivides them into individual islands; this is
# the standard 52-unit INE provincial series.
province_lookup <- tibble::tribble(
  ~ine_code, ~nuts3_code, ~nuts2_code, ~province_name,
  "02", "ES421", "ES42", "Albacete",
  "03", "ES521", "ES52", "Alicante/Alacant",
  "04", "ES611", "ES61", "Almeria",
  "01", "ES211", "ES21", "Araba/Alava",
  "33", "ES120", "ES12", "Asturias",
  "05", "ES411", "ES41", "Avila",
  "06", "ES431", "ES43", "Badajoz",
  "07", "ES532", "ES53", "Balears, Illes",
  "08", "ES511", "ES51", "Barcelona",
  "09", "ES412", "ES41", "Burgos",
  "10", "ES432", "ES43", "Caceres",
  "11", "ES612", "ES61", "Cadiz",
  "39", "ES130", "ES13", "Cantabria",
  "12", "ES522", "ES52", "Castellon/Castello",
  "13", "ES422", "ES42", "Ciudad Real",
  "14", "ES613", "ES61", "Cordoba",
  "15", "ES111", "ES11", "A Coruna",
  "16", "ES423", "ES42", "Cuenca",
  "20", "ES212", "ES21", "Gipuzkoa",
  "17", "ES512", "ES51", "Girona",
  "18", "ES614", "ES61", "Granada",
  "19", "ES424", "ES42", "Guadalajara",
  "21", "ES615", "ES61", "Huelva",
  "22", "ES241", "ES24", "Huesca",
  "23", "ES616", "ES61", "Jaen",
  "24", "ES413", "ES41", "Leon",
  "25", "ES513", "ES51", "Lleida",
  "27", "ES112", "ES11", "Lugo",
  "28", "ES300", "ES30", "Madrid",
  "29", "ES617", "ES61", "Malaga",
  "30", "ES620", "ES62", "Murcia",
  "31", "ES220", "ES22", "Navarra",
  "32", "ES113", "ES11", "Ourense",
  "34", "ES414", "ES41", "Palencia",
  "35", "ES705", "ES70", "Palmas, Las",
  "36", "ES114", "ES11", "Pontevedra",
  "26", "ES230", "ES23", "La Rioja",
  "37", "ES415", "ES41", "Salamanca",
  "38", "ES709", "ES70", "Santa Cruz de Tenerife",
  "40", "ES416", "ES41", "Segovia",
  "41", "ES618", "ES61", "Sevilla",
  "42", "ES417", "ES41", "Soria",
  "43", "ES514", "ES51", "Tarragona",
  "44", "ES242", "ES24", "Teruel",
  "45", "ES425", "ES42", "Toledo",
  "46", "ES523", "ES52", "Valencia/Valencia",
  "47", "ES418", "ES41", "Valladolid",
  "48", "ES213", "ES21", "Bizkaia",
  "49", "ES419", "ES41", "Zamora",
  "50", "ES243", "ES24", "Zaragoza",
  "51", "ES630", "ES63", "Ceuta",
  "52", "ES640", "ES64", "Melilla"
)

# Keywords INE uses in series names to identify the national-total series.
NATIONAL_KEYWORDS <- c("total nacional", "total  nacional", "espana", "national")

#' Normalise a province name for fuzzy matching (strip accents, lowercase,
#' trim) so INE's free-text series names can be matched against
#' `province_lookup`, whose own naming conventions ("A Coruna", "La
#' Rioja") don't always agree with INE's ("Coruna, A", "Rioja, La").
#' @noRd
normalize_province_name <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  stringr::str_to_lower(stringr::str_trim(x))
}

#' @noRd
comma_swap_name <- function(x) {
  parts <- stringr::str_split_fixed(x, ",", 2)
  has_comma <- parts[, 2] != ""
  x[has_comma] <- stringr::str_trim(
    paste(stringr::str_trim(parts[has_comma, 2]), stringr::str_trim(parts[has_comma, 1]))
  )
  x
}

#' Resolve INE province codes for a vector of raw province names, matching
#' against `lookup$province_name` (directly, or with comma parts swapped)
#' after accent/case normalisation. Returns NA for names that don't match
#' any known province (e.g. INE's "Total Nacional" series).
#' @noRd
resolve_province_code <- function(prov_raw, lookup = province_lookup) {
  key <- normalize_province_name(prov_raw)
  key_swapped <- normalize_province_name(comma_swap_name(prov_raw))
  lookup_keys <- normalize_province_name(lookup$province_name)

  idx <- match(key, lookup_keys)
  missing <- is.na(idx)
  idx[missing] <- match(key_swapped[missing], lookup_keys)

  lookup$ine_code[idx]
}

#' Search INE's Tempus3 table catalogue for tables under `operation_code`
#' whose name matches every pattern in `name_filters` (case-insensitive).
#' @noRd
find_ine_table <- function(operation_code, name_filters) {
  url <- sprintf("%s/TABLAS_OPERACION/%s?det=0", INE_API_BASE, operation_code)

  resp <- tryCatch(
    httr2::request(url) |> httr2::req_perform(),
    error = function(e) {
      warning("INE API request failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NULL)

  tables <- resp |> httr2::resp_body_json(simplifyVector = TRUE)
  if (is.null(tables) || length(tables) == 0) {
    warning("No tables returned for operation '", operation_code, "'")
    return(NULL)
  }
  tables <- tibble::as_tibble(tables)

  keep <- Reduce(`&`, lapply(name_filters, function(pat) {
    stringr::str_detect(stringr::str_to_lower(tables$Nombre), pat)
  }))
  candidates <- tables[keep, ]

  if (nrow(candidates) == 0) {
    warning(
      "No table matched filters (", paste(name_filters, collapse = ", "),
      ") in operation '", operation_code, "'. Inspect the full table list ",
      "manually and set table_id by hand."
    )
    return(tables)
  }

  candidates[, c("Id", "Nombre")]
}

#' Fetch raw series data from a given INE Tempus3 table ID via the JSON API.
#' Returns NULL (with a warning) if the request fails or the table exceeds
#' the API's undocumented volume limit (some province x age x sex tables
#' are rejected outright regardless of `n_periods`/date filters) — the
#' caller should fall back to `fetch_ine_csv()` in that case.
#' @noRd
fetch_ine_json <- function(table_id, n_periods = NULL,
                                 date_from = NULL, date_to = NULL) {
  query <- list(tip = "AM")
  if (!is.null(n_periods)) {
    query$nult <- n_periods
  } else if (!is.null(date_from) && !is.null(date_to)) {
    query$date <- sprintf("%s:%s", date_from, date_to)
  }

  url <- sprintf("%s/DATOS_TABLA/%s", INE_API_BASE, table_id)

  resp <- tryCatch(
    httr2::request(url) |> httr2::req_url_query(!!!query) |> httr2::req_perform(),
    error = function(e) {
      warning("INE data request failed for table ", table_id, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NULL)

  body <- resp |> httr2::resp_body_json(simplifyVector = FALSE)

  if (is.list(body) && !is.null(body$status)) {
    warning("INE table ", table_id, " exceeds the JSON API's size limit (\"",
            body$status, "\"). Falling back to the CSV bulk export.")
    return(NULL)
  }

  body
}

#' Download an INE table's full bulk CSV export (fallback for tables too
#' large for `fetch_ine_json()`). Serves the complete table with no
#' series-count limit, at the cost of a much larger download.
#' @noRd
fetch_ine_csv <- function(table_id) {
  url <- sprintf("https://www.ine.es/jaxiT3/files/t/es/csv_bd/%s.csv?nocab=1", table_id)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  resp <- tryCatch(
    httr2::request(url) |> httr2::req_perform(path = tmp),
    error = function(e) {
      warning("INE CSV bulk download failed for table ", table_id, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(resp)) return(NULL)

  readr::read_tsv(tmp, col_types = readr::cols(.default = "c"), progress = FALSE)
}

#' Disk-cache wrapper for the slow SHMD retrieval functions
#' (`get_ine_births()`/`get_ine_deaths()`/`get_ine_population()`).
#' Mirrors [update_ine_data()]'s existing cache location and `.rds`
#' convention. If a cache file matching `cache_key` exists and
#' `use_cache = TRUE` and `force = FALSE`, returns it directly, skipping
#' `fetch_fun()` (and therefore the live INE request) entirely. Otherwise
#' runs `fetch_fun()`, and if `use_cache = TRUE`, saves the result to
#' cache before returning it.
#' @noRd
with_ine_cache <- function(cache_key, use_cache, force, cache_dir, fetch_fun) {
  if (!use_cache) return(fetch_fun())

  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

  if (!force && file.exists(cache_file)) {
    message(
      "Using cached data from ", cache_file,
      " (pass force = TRUE to re-fetch from INE)."
    )
    return(readRDS(cache_file))
  }

  result <- fetch_fun()
  saveRDS(result, cache_file)
  result
}
