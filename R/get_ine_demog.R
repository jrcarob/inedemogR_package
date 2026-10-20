#' Get INE Demographic Data
#'
#' Retrieves demographic data from INE via the official
#' [ineapir::get_data_table()] API wrapper, and tidies it into a data frame
#' with one row per geography x year and one column per indicator.
#'
#' Each `indicator` is mapped to a real INE table id via the [ine_variables]
#' registry (see `list_ine_indicators()`). Different indicators are only
#' available at different geographic granularities in INE's source tables
#' (e.g. population is published by municipality, but births/deaths only
#' down to province) — all requested indicators must share the same
#' `geo_level`, or request them separately.
#'
#' @param indicator Character vector of indicator codes (see
#'   [list_ine_indicators()]).
#' @param geo_level Character, the geographic level to fetch. Must match the
#'   level at which every requested indicator is actually published (see
#'   [list_ine_indicators()]). If `NULL` (default), inferred from `indicator`.
#' @param year Numeric or character, the year of data. Default (`NULL`)
#'   returns the latest available period only.
#' @param region Optional character vector to subset by region name
#'   (regular expressions allowed, matched against the geography name).
#' @param sex Character, one of `"Total"`, `"Hombres"`, `"Mujeres"`.
#' @param geometry Logical, join geometries from [get_ine_geo()] (default
#'   `FALSE`).
#' @return A tidy tibble with columns `GEOID`, `NAME`, `year`, and one column
#'   per requested indicator; an `sf` object if `geometry = TRUE`.
#' @examples
#' \dontrun{
#' # Not run: requires live network access to the INE API.
#' pop_data <- get_ine_demog(indicator = "population_total", year = 2023)
#' births_deaths <- get_ine_demog(
#'   indicator = c("births_total", "deaths_total"), year = 2023
#' )
#' }
#' @export
get_ine_demog <- function(indicator,
                           geo_level = NULL,
                           year = NULL,
                           region = NULL,
                           sex = "Total",
                           geometry = FALSE) {
  if (missing(indicator) || length(indicator) == 0) {
    stop("`indicator` is required and must be a character vector of one or more indicators.")
  }

  unknown <- setdiff(indicator, ine_variables$indicator)
  if (length(unknown) > 0) {
    stop("Unknown indicator(s): ", paste(unknown, collapse = ", "),
         ". Use list_ine_indicators() to see available options.")
  }

  spec <- ine_variables[match(indicator, ine_variables$indicator), ]

  not_wired <- spec$indicator[is.na(spec$id_table)]
  if (length(not_wired) > 0) {
    stop("Indicator(s) not yet wired to a live INE table: ",
         paste(not_wired, collapse = ", "))
  }

  levels_needed <- unique(spec$geo_level)
  if (length(levels_needed) > 1) {
    mismatched <- unique(spec$indicator[match(levels_needed, spec$geo_level)])
    stop("Requested indicators are published at different geographic levels (",
         paste(mismatched, collapse = ", "), "). Fetch them in separate calls.")
  }
  if (!is.null(geo_level) && geo_level != levels_needed) {
    stop("geo_level = '", geo_level,
         "' does not match the level indicator(s) are published at ('",
         levels_needed, "').")
  }
  geo_level <- levels_needed

  fetch_one <- function(row) {
    args <- list(
      idTable = row$id_table,
      unnest = TRUE,
      tip = "AM",
      metanames = TRUE,
      metacodes = TRUE
    )
    if (is.null(year)) {
      args$nlast <- 1
    } else {
      args$dateStart <- sprintf("%s/01/01", year)
      args$dateEnd <- sprintf("%s/12/31", year)
    }

    df <- do.call(ineapir::get_data_table, args)
    df <- tibble::as_tibble(df)

    geo_name_col <- row$geo_var
    geo_code_col <- paste0(row$geo_var, ".Codigo")

    df |>
      dplyr::filter(.data$Sexo == sex) |>
      dplyr::transmute(
        GEOID = .data[[geo_code_col]],
        NAME = .data[[geo_name_col]],
        year = .data$Anyo,
        indicator = row$indicator,
        value = .data$Valor
      )
  }

  long <- purrr::map_dfr(seq_len(nrow(spec)), function(i) fetch_one(spec[i, ]))

  if (nrow(long) == 0) {
    stop("No data returned for the requested indicator(s)/year. ",
         "Check that `year` has been published yet.")
  }

  wide <- long |>
    tidyr::pivot_wider(names_from = "indicator", values_from = "value")

  if (!is.null(region)) {
    pattern <- paste(region, collapse = "|")
    wide <- wide |>
      dplyr::filter(stringr::str_detect(.data$NAME, pattern))
  }

  if (isTRUE(geometry)) {
    geo_data <- get_ine_geo(geo_level = geo_level, region = region)
    wide <- sf::st_as_sf(
      dplyr::left_join(geo_data, dplyr::select(wide, -"NAME"), by = "GEOID")
    )
  }

  wide
}
