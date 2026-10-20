# Fertility benchmark: total fertility rate and mean age at childbearing from
# age_specific_fertility_rate() against INE's published provincial series
# (Indicadores Demograficos Basicos, tables 1478 and 1581, all birth orders),
# all 52 provinces, 1975-2024.
#
# Tolerance fixed before the comparison: INE publishes both indicators to two
# decimals, so agreement means |difference| <= 0.01 (one unit of the
# published precision; rounding alone accounts for up to 0.005).
#
#   Rscript reproducibility/05_fertility_benchmark.R

devtools::load_all(quiet = TRUE)
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
d <- "reproducibility/data"
tol <- 0.01

pop <- readRDS(file.path(d, "population_56945_100.rds"))$data
births_age <- readRDS(file.path(d, "births_by_age_6508_100.rds"))$data
asfr <- quiet(age_specific_fertility_rate(births_age, pop))
ours <- dplyr::inner_join(
  total_fertility_rate(asfr), mean_age_at_childbearing(asfr),
  by = c("nuts3_code", "province_name", "year")
) |>
  tidyr::pivot_longer(c("tfr", "mac"), names_to = "indicator", values_to = "inedemogR")

ine <- dplyr::bind_rows(
  tfr = readRDS(file.path(d, "ine_idb_1478.rds")),
  mac = readRDS(file.path(d, "ine_idb_1581.rds")),
  .id = "indicator"
) |>
  dplyr::filter(.data$birth_order == "Todos", grepl("^[0-9]{2} ", .data$province)) |>
  dplyr::mutate(ine_code = substr(.data$province, 1, 2)) |>
  dplyr::left_join(dplyr::select(province_lookup, "ine_code", "nuts3_code"), by = "ine_code") |>
  dplyr::select("indicator", "nuts3_code", "year", ine = "value")

cmp <- dplyr::inner_join(ours, ine, by = c("indicator", "nuts3_code", "year")) |>
  dplyr::filter(is.finite(.data$ine)) |>
  dplyr::mutate(
    diff = .data$inedemogR - .data$ine,
    period = cut(.data$year, c(1974, 2001, 2024), labels = c("1975-2001", "2002-2024"))
  )

summarise_diff <- function(df, ...) {
  df |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n = dplyr::n(), mean_signed = mean(.data$diff), mean_abs = mean(abs(.data$diff)),
      max_abs = max(abs(.data$diff)), within_tol = mean(abs(.data$diff) <= tol),
      .groups = "drop"
    )
}
by_indicator <- summarise_diff(cmp, .data$indicator)
by_period <- summarise_diff(cmp, .data$indicator, .data$period)
worst <- cmp |>
  dplyr::arrange(dplyr::desc(abs(.data$diff))) |>
  dplyr::select("indicator", "province_name", "year", "inedemogR", "ine", "diff") |>
  utils::head(10)

out <- "reproducibility/output"
dir.create(out, showWarnings = FALSE)
readr::write_csv(cmp, file.path(out, "fertility_benchmark_all.csv"))
readr::write_csv(by_indicator, file.path(out, "fertility_benchmark_summary.csv"))
readr::write_csv(by_period, file.path(out, "fertility_benchmark_by_period.csv"))
print(as.data.frame(by_indicator), digits = 4)
print(as.data.frame(by_period), digits = 4)
print(as.data.frame(worst), digits = 5)
