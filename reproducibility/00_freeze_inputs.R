# Freeze every input used by the validation study and the manuscript.
#
# Run once, from the package root, with network access. Each file is written
# only if absent, so re-running never silently replaces frozen data; delete a
# file deliberately to refresh it. Every object carries its retrieval time.
#
#   Rscript reproducibility/00_freeze_inputs.R

devtools::load_all(quiet = TRUE)
dir <- "reproducibility/data"
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

# INE inputs, cached as returned by the package (with a `provenance`
# attribute: request, retrieval time UTC, API, package version).
invisible(get_ine_population(n_periods = 100, cache_dir = dir))    # table 56945
invisible(get_ine_deaths(n_periods = 100, cache_dir = dir))        # 6545, 6547
invisible(get_ine_births_by_age(n_periods = 100, cache_dir = dir)) # table 6508
invisible(get_ine_births(n_periods = 100, cache_dir = dir))        # table 6506

# Quick-lookup totals for the births-per-death map (Fig. 1a).
demog_file <- file.path(dir, "demog_totals_2023.rds")
if (!file.exists(demog_file)) {
  vitals <- get_ine_demog(indicator = c("births_total", "deaths_total"), year = 2023)
  attr(vitals, "provenance") <- list(
    request = "get_ine_demog(c('births_total', 'deaths_total'), year = 2023)",
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    api = "ineapir", package_version = as.character(utils::packageVersion("inedemogR"))
  )
  saveRDS(vitals, demog_file)
}

# INE reference: provincial life tables, operation TM, table 27155 (1991-),
# complete single-age tables from death microdata, published aggregated to
# 0, 1-4, 5-9, ..., 95+. Methodology:
# https://www.ine.es/metodologia/t20/t2020319a.pdf (Section 3).
ref_file <- file.path(dir, "ine_life_tables_27155.rds")
if (!file.exists(ref_file)) {
  url <- "https://www.ine.es/jaxiT3/files/t/es/csv_bd/27155.csv?nocab=1"
  tmp <- tempfile(fileext = ".csv")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  ref <- readr::read_tsv(
    tmp, col_types = readr::cols(.default = "c"),
    locale = readr::locale(encoding = "UTF-8")
  )
  names(ref) <- c("province", "sex", "age_group", "life_table_function", "year", "value")
  ref$value <- readr::parse_number(ref$value, locale = readr::locale(decimal_mark = ","))
  ref$year <- as.integer(ref$year)
  attr(ref, "provenance") <- list(
    request = url, retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = "INE, Tablas de mortalidad por año, provincia, sexo, edad y funciones (27155)"
  )
  saveRDS(ref, ref_file)
}

# INE reference: fertility indicators by province, Indicadores Demograficos
# Basicos (operation IDB), all birth orders. Table 1478: Indicador Coyuntural
# de Fecundidad (TFR); table 1581: Edad Media a la Maternidad (MAC).
# Methodology: https://ine.es/metodologia/t20/metodologia_idb.pdf
for (tab in c(tfr = 1478, mac = 1581)) {
  f <- file.path(dir, sprintf("ine_idb_%d.rds", tab))
  if (file.exists(f)) next
  url <- sprintf("https://www.ine.es/jaxiT3/files/t/es/csv_bd/%d.csv?nocab=1", tab)
  tmp <- tempfile(fileext = ".csv")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  ref <- readr::read_tsv(tmp, col_types = readr::cols(.default = "c"),
                         locale = readr::locale(encoding = "UTF-8"))
  names(ref) <- c("province", "birth_order", "year", "value")
  ref$value <- readr::parse_number(ref$value, locale = readr::locale(decimal_mark = ","))
  ref$year <- as.integer(ref$year)
  attr(ref, "provenance") <- list(
    request = url, retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source = sprintf("INE, Indicadores Demograficos Basicos, table %d", tab)
  )
  saveRDS(ref, f)
}

for (f in list.files(dir, full.names = TRUE)) {
  p <- attr(readRDS(f), "provenance")
  cat(basename(f), ":", p$request, "retrieved", p$retrieved_utc, "\n")
}
