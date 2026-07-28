#' @include helpers.R
NULL

# Internal HMD-format .txt writers used by download_ine_data(). Mirror the
# HMD's own file layout: two header lines, a blank line, then a
# right-aligned data table.

#' @noRd
write_txt_by_province <- function(df, prefix, header_suffix, col_spec, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  provinces <- df |> dplyr::distinct(.data$nuts3_code, .data$province_name)

  paths <- purrr::pmap_chr(provinces, function(nuts3_code, province_name) {
    # .env$ is required here: nuts3_code is both a column in df (via .data)
    # and this function's own argument: an unqualified `nuts3_code` on the
    # right-hand side would resolve to the *column* under dplyr's default
    # data-mask precedence (data before environment), making the filter
    # always true and silently mixing every province into each file.
    prov_df <- df |> dplyr::filter(.data$nuts3_code == .env$nuts3_code)
    safe_name <- stringr::str_replace_all(province_name, "[/ ,]", "_")
    filename <- file.path(out_dir, sprintf("%s_%s_%s.txt", prefix, nuts3_code, safe_name))

    header1 <- sprintf("%s (NUTS-3: %s), %s", province_name, nuts3_code, header_suffix)
    header2 <- sprintf("Last modified: %s; inedemogR (HMD Methods Protocol V6 aligned)",
                        format(Sys.Date(), "%d %b %Y"))

    lines <- col_spec(prov_df)
    writeLines(c(header1, header2, "", lines$header, lines$data), filename)
    filename
  })

  message("Wrote ", length(paths), " province files to '", out_dir, "' (", header_suffix, ")")
  invisible(paths)
}

#' @noRd
age_label <- function(age) ifelse(age >= MAX_AGE, sprintf("%d+", MAX_AGE), as.character(age))

#' @noRd
write_births_txt <- function(df, out_dir) {
  write_txt_by_province(
    dplyr::arrange(df, .data$year), "Births", "Births",
    function(d) list(
      header = sprintf("%8s%12s%12s%12s", "Year", "Female", "Male", "Total"),
      data = sprintf("%8d%12d%12d%12d", d$year,
                      as.integer(round(d$female)), as.integer(round(d$male)), as.integer(round(d$total)))
    ),
    out_dir
  )
}

#' @noRd
write_deaths_national_txt <- function(df, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  if (nrow(df) == 0) {
    message("No national-total series found; Deaths_ES_National.txt not written.")
    return(invisible(NULL))
  }
  df <- dplyr::arrange(df, .data$year)
  filename <- file.path(out_dir, "Deaths_ES_National.txt")
  header1 <- "Spain (ES), Deaths by place of residence - National Total"
  header2 <- sprintf("Last modified: %s; inedemogR (HMD Methods Protocol V6 aligned)",
                      format(Sys.Date(), "%d %b %Y"))
  col_headers <- sprintf("%8s%12s%12s%12s", "Year", "Female", "Male", "Total")
  data_lines <- sprintf("%8d%12d%12d%12d", df$year,
                        as.integer(round(df$female)), as.integer(round(df$male)), as.integer(round(df$total)))
  writeLines(c(header1, header2, "", col_headers, data_lines), filename)
  invisible(filename)
}

#' @noRd
write_deaths_age_txt <- function(df, out_dir) {
  write_txt_by_province(
    dplyr::arrange(df, .data$year, .data$age), "Deaths", "Deaths by place of residence",
    function(d) list(
      header = sprintf("%8s%8s%12s%12s%12s", "Year", "Age", "Female", "Male", "Total"),
      data = sprintf("%8d%8s%12d%12d%12d", d$year, age_label(d$age),
                      as.integer(round(d$female)), as.integer(round(d$male)), as.integer(round(d$total)))
    ),
    out_dir
  )
}

#' @noRd
write_population_txt <- function(df, out_dir) {
  write_txt_by_province(
    dplyr::arrange(df, .data$year, .data$age), "Population", "Population (January 1st)",
    function(d) list(
      header = sprintf("%8s%8s%14s%14s%14s", "Year", "Age", "Female", "Male", "Total"),
      data = sprintf("%8d%8s%14.2f%14.2f%14.2f", d$year, age_label(d$age), d$female, d$male, d$total)
    ),
    out_dir
  )
}

#' @noRd
write_exposure_txt <- function(df, out_dir) {
  writable <- df |> dplyr::filter(!.data$is_missing_source_pop)
  write_txt_by_province(
    dplyr::arrange(writable, .data$year, .data$age), "Exposures_1x1", "Exposure-to-Risk (1x1)",
    function(d) list(
      header = sprintf("%8s%8s%14s%14s%14s", "Year", "Age", "Female", "Male", "Total"),
      data = sprintf("%8d%8s%14.2f%14.2f%14.2f", d$year, age_label(d$age), d$female, d$male, d$total)
    ),
    out_dir
  )
}

#' @noRd
write_mx_1x1_txt <- function(df, out_dir) {
  write_txt_by_province(
    dplyr::arrange(df, .data$year, .data$age), "Mx_1x1", "Central Death Rates (1x1)",
    function(d) list(
      header = sprintf("%8s%8s%16s%16s%16s", "Year", "Age", "Female", "Male", "Total"),
      data = sprintf("%8d%8s%16.6f%16.6f%16.6f", d$year, age_label(d$age), d$mx_female, d$mx_male, d$mx_total)
    ),
    out_dir
  )
}

#' @noRd
write_mx_5x1_txt <- function(df, out_dir) {
  write_txt_by_province(
    dplyr::arrange(df, .data$year, .data$age_group), "Mx_5x1", "Central Death Rates (5x1)",
    function(d) list(
      header = sprintf("%8s%10s%16s%16s%16s", "Year", "AgeGrp", "Female", "Male", "Total"),
      data = sprintf("%8d%10s%16.6f%16.6f%16.6f", d$year, d$age_group, d$mx_female, d$mx_male, d$mx_total)
    ),
    out_dir
  )
}

#' @noRd
write_asdr_txt <- function(df, out_dir) {
  write_txt_by_province(
    dplyr::arrange(df, .data$year), "ASDR", "Age-Standardized Death Rate (ESP 2013, per 100,000)",
    function(d) list(
      header = sprintf("%8s%16s%16s%16s", "Year", "Female", "Male", "Total"),
      data = sprintf("%8d%16.4f%16.4f%16.4f", d$year, d$asdr_female, d$asdr_male, d$asdr_total)
    ),
    out_dir
  )
}

#' @noRd
write_life_table_txt <- function(df, sex_label, out_dir) {
  prefix <- switch(sex_label, f = "fltper_1x1", m = "mltper_1x1", b = "bltper_1x1")
  sex_desc <- switch(sex_label, f = "Female", m = "Male", b = "Both Sexes")

  write_txt_by_province(
    dplyr::arrange(df, .data$year, .data$age), prefix,
    sprintf("Period Life Table (1x1) - %s", sex_desc),
    function(d) list(
      header = sprintf("%8s%6s%12s%10s%8s%12s%10s%12s%14s%10s",
                        "Year", "Age", "mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex"),
      data = sprintf(
        "%8d%6s%12.6f%10.6f%8.3f%12.1f%10.1f%12.1f%14.1f%10.2f",
        d$year, ifelse(d$age >= LIFE_TABLE_MAX, sprintf("%d+", LIFE_TABLE_MAX), as.character(d$age)),
        d$mx, d$qx, d$ax, d$lx, d$dx, d$Lx, d$Tx, d$ex
      )
    ),
    out_dir
  )
}

#' @noRd
write_csv_file <- function(df, name, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(out_dir, paste0(name, ".csv"))
  utils::write.csv(df, path, row.names = FALSE)
  message("Wrote ", path)
  invisible(path)
}
