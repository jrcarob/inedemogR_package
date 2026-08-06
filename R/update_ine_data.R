#' Update INE Data
#'
#' Checks each indicator wired in [ine_variables] for its latest published
#' year (via [get_ine_demog()]) and, if newer than the local cache, downloads
#' and caches it for offline use.
#'
#' @param force Logical, force update even if data is current.
#' @param data_dir Character, directory for cached data. No default: this
#'   function only writes when a directory is explicitly supplied, per CRAN
#'   policy against writing to the user's home filespace by default. Pass
#'   e.g. `tools::R_user_dir("inedemogR", "cache")` for a persistent cache.
#' @return Invisibly, a named list of the latest cached year per indicator.
#' @examples
#' \dontrun{
#' # Not run: requires live network access to the INE API, and writes to
#' # the supplied cache directory.
#' update_ine_data(data_dir = tools::R_user_dir("inedemogR", "cache"))
#' }
#' @export
update_ine_data <- function(force = FALSE, data_dir) {
  if (missing(data_dir)) {
    stop(
      "`data_dir` must be supplied explicitly (e.g. ",
      "tools::R_user_dir(\"inedemogR\", \"cache\")) - inedemogR does not ",
      "write to the home filespace by default."
    )
  }
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  wired <- ine_variables[!is.na(ine_variables$id_table), ]
  latest_years <- list()

  for (i in seq_len(nrow(wired))) {
    ind <- wired$indicator[i]
    cache_file <- file.path(data_dir, paste0(ind, ".rds"))
    cached_year <- if (file.exists(cache_file)) {
      attr(readRDS(cache_file), "year")
    } else {
      0
    }

    latest <- get_ine_demog(indicator = ind)
    latest_year <- max(latest$year, na.rm = TRUE)

    if (force || latest_year > cached_year) {
      attr(latest, "year") <- latest_year
      saveRDS(latest, cache_file)
      message("Updated '", ind, "' to year ", latest_year)
    } else {
      message("'", ind, "' is up-to-date (year ", cached_year, ")")
    }

    latest_years[[ind]] <- latest_year
  }

  invisible(latest_years)
}
