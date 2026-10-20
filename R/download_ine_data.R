#' @include helpers.R
NULL

#' Download SHMD pipeline data to a local folder
#'
#' Runs the requested stage(s) of the SHMD (Spanish subnational Human
#' Mortality Database) protocol pipeline — retrieval of province-level
#' births/deaths/population from INE, exposure-to-risk, central death
#' rates, and period life tables — and writes the results into `out_dir`
#' as `.csv` and/or HMD-format `.txt` files, so they can be inspected,
#' shared, or fed into other HMD-compatible tooling without staying in the
#' R session.
#'
#' Later stages depend on earlier ones (`exposure` needs `population` and
#' `deaths`; `mx` needs `deaths` and `exposure`; `life_tables` needs `mx`).
#' Requesting a later stage automatically fetches/computes and writes its
#' dependencies too, since they were already retrieved along the way.
#'
#' @param out_dir Directory to write into (created if it doesn't exist).
#' @param stages Character vector of stages to include. One or more of
#'   `"births"`, `"deaths"`, `"population"`, `"exposure"`, `"mx"`,
#'   `"life_tables"`. Default: all of them.
#' @param format Character vector, one or both of `"csv"` and `"txt"`.
#'   `"csv"` writes one combined file per data table; `"txt"` writes
#'   HMD-format files, one per province. Default: both.
#' @param n_periods Number of most recent years to fetch for the
#'   births/deaths/population retrieval stages.
#' @return Invisibly, a list with the in-memory results of every stage that
#'   was run (`births`, `deaths`, `population`, `exposure`, `death_rates`,
#'   `life_tables`, each `NULL` if not run) and `files`, a character vector
#'   of every path written.
#' @examples
#' \dontrun{
#' # Not run: requires live network access to the INE API, and writes files
#' # to disk.
#' # Everything, both formats, into ./ine_data
#' download_ine_data("ine_data")
#'
#' # Just births and deaths, CSV only
#' download_ine_data("ine_data", stages = c("births", "deaths"), format = "csv")
#'
#' # Full pipeline through life tables
#' download_ine_data("ine_data", stages = "life_tables", n_periods = 40)
#' }
#' @export
download_ine_data <- function(out_dir,
                           stages = c("births", "deaths", "population",
                                      "exposure", "mx", "life_tables"),
                           format = c("csv", "txt"),
                           n_periods = 30) {
  stages <- match.arg(stages, several.ok = TRUE)
  format <- match.arg(format, several.ok = TRUE)

  needed <- stages
  if ("life_tables" %in% needed) needed <- union(needed, "mx")
  if ("mx" %in% needed) needed <- union(needed, c("deaths", "exposure"))
  if ("exposure" %in% needed) needed <- union(needed, c("population", "deaths"))
  stage_order <- c("births", "deaths", "population", "exposure", "mx", "life_tables")
  needed <- stage_order[stage_order %in% needed]

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  files <- character(0)
  result <- list(births = NULL, deaths = NULL, population = NULL,
                 exposure = NULL, death_rates = NULL, life_tables = NULL)

  write_stage <- function(csv_list, txt_fun) {
    if ("csv" %in% format) {
      for (nm in names(csv_list)) {
        files <<- c(files, write_csv_file(csv_list[[nm]], nm, out_dir))
      }
    }
    if ("txt" %in% format) files <<- c(files, unlist(txt_fun(), use.names = FALSE))
  }

  if ("births" %in% needed) {
    message("== Stage: births ==")
    result$births <- get_ine_births(n_periods = n_periods)
    write_stage(
      list(births = result$births$data),
      function() write_births_txt(result$births$data, out_dir)
    )
  }

  if ("deaths" %in% needed) {
    message("== Stage: deaths ==")
    result$deaths <- get_ine_deaths(n_periods = n_periods)
    write_stage(
      list(deaths_provinces = result$deaths$data_provinces,
           deaths_national = result$deaths$data_national),
      function() c(
        write_deaths_age_txt(result$deaths$data_provinces, out_dir),
        write_deaths_national_txt(result$deaths$data_national, out_dir)
      )
    )
  }

  if ("population" %in% needed) {
    message("== Stage: population ==")
    result$population <- get_ine_population(n_periods = n_periods)
    write_stage(
      list(population = result$population$data),
      function() write_population_txt(result$population$data, out_dir)
    )
  }

  if ("exposure" %in% needed) {
    message("== Stage: exposure ==")
    result$exposure <- compute_exposure(result$population$data, result$deaths$data_provinces)
    write_stage(
      list(exposure = result$exposure$data),
      function() write_exposure_txt(result$exposure$data, out_dir)
    )
  }

  if ("mx" %in% needed) {
    message("== Stage: death rates (mx) ==")
    result$death_rates <- compute_death_rates(result$deaths$data_provinces, result$exposure$data)
    write_stage(
      list(mx_1x1 = result$death_rates$mx_1x1, mx_5x1 = result$death_rates$mx_5x1,
           asdr = result$death_rates$asdr),
      function() c(
        write_mx_1x1_txt(result$death_rates$mx_1x1, out_dir),
        write_mx_5x1_txt(result$death_rates$mx_5x1, out_dir),
        write_asdr_txt(result$death_rates$asdr, out_dir)
      )
    )
  }

  if ("life_tables" %in% needed) {
    message("== Stage: life tables ==")
    result$life_tables <- build_life_tables(result$death_rates$mx_1x1)
    write_stage(
      list(life_table_female = result$life_tables$fltper,
           life_table_male = result$life_tables$mltper,
           life_table_both = result$life_tables$bltper),
      function() c(
        write_life_table_txt(result$life_tables$fltper, "f", out_dir),
        write_life_table_txt(result$life_tables$mltper, "m", out_dir),
        write_life_table_txt(result$life_tables$bltper, "b", out_dir)
      )
    )
  }

  message("Done. Wrote ", length(files), " file(s) to '", out_dir, "'.")
  result$files <- files
  invisible(result)
}
