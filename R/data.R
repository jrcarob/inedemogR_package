#' INE demographic indicator reference table
#'
#' A registry mapping indicator codes used throughout inedemogR to the real
#' INE table each is retrieved from via [ineapir::get_data_table()].
#'
#' @format A tibble with 3 rows and 6 variables:
#' \describe{
#'   \item{indicator}{Indicator code, e.g. `"population_total"`.}
#'   \item{name}{Short human-readable name.}
#'   \item{description}{Longer description of what the indicator measures.}
#'   \item{id_table}{INE `idTable` identifier, or `NA` if not yet wired to a
#'     live table.}
#'   \item{geo_var}{Name of the geographic dimension column returned by
#'     `ineapir::get_data_table(..., metanames = TRUE)` for this table.}
#'   \item{geo_level}{Geographic granularity the table is published at
#'     (`"municipality"` or `"province"`).}
#' }
#' @source Instituto Nacional de Estadistica (INE), "fenomenos demograficos".
"ine_variables"
