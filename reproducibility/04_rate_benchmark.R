# Intermediate-quantity benchmark: grouped central death rates from
# compute_death_rates() (observed deaths / mean-of-stocks exposure) against
# the group death rates ("Tasa de mortalidad") in INE's provincial life
# tables, 2002-2024, all provinces, both sexes, groups 1-4 to 85-89.
# INE's group rates come from a complete table built on exact exposure from
# death microdata, so small differences are expected; large ones would point
# to an input problem (deaths or population).
#
#   Rscript reproducibility/04_rate_benchmark.R

devtools::load_all(quiet = TRUE)
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
d <- "reproducibility/data"
pop <- readRDS(file.path(d, "population_56945_100.rds"))$data
deaths <- readRDS(file.path(d, "deaths_6545_6547_100.rds"))$data_provinces
ine <- readRDS(file.path(d, "ine_life_tables_27155.rds"))
years <- 2002:2024

rates <- quiet(compute_death_rates(deaths, quiet(compute_exposure(pop, deaths))$data))
starts <- c(1, seq(5, 85, 5))
ours <- rates$mx_1x1 |>
  dplyr::filter(.data$year %in% years, .data$age >= 1, .data$age < 90) |>
  dplyr::mutate(group = ifelse(.data$age < 5, 1, (.data$age %/% 5) * 5)) |>
  dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year, .data$group) |>
  dplyr::summarise(
    female = sum(.data$d_female) / sum(.data$e_female),
    male = sum(.data$d_male) / sum(.data$e_male),
    deaths_female = sum(.data$d_female), deaths_male = sum(.data$d_male), .groups = "drop"
  ) |>
  tidyr::pivot_longer(c("female", "male"), names_to = "sex", values_to = "rate") |>
  dplyr::mutate(deaths = ifelse(.data$sex == "female", .data$deaths_female, .data$deaths_male)) |>
  dplyr::select(-"deaths_female", -"deaths_male")

label <- function(g) ifelse(g == 1, "De 1 a 4 años", sprintf("De %d a %d años", g, g + 4))
ref <- ine |>
  dplyr::filter(.data$life_table_function == "Tasa de mortalidad", .data$year %in% years,
                .data$sex %in% c("Mujeres", "Hombres"), .data$age_group %in% label(starts)) |>
  dplyr::mutate(
    ine_code = substr(.data$province, 1, 2),
    sex = ifelse(.data$sex == "Mujeres", "female", "male"),
    group = match(.data$age_group, label(starts)), group = starts[.data$group],
    ine_rate = .data$value / 1000
  ) |>
  dplyr::left_join(dplyr::select(province_lookup, "ine_code", "nuts3_code"), by = "ine_code") |>
  dplyr::select("nuts3_code", "year", "sex", "group", "ine_rate")

cmp <- dplyr::inner_join(ours, ref, by = c("nuts3_code", "year", "sex", "group")) |>
  dplyr::filter(.data$ine_rate > 0) |>
  dplyr::mutate(rel_diff = .data$rate / .data$ine_rate - 1)

summary_by_size <- cmp |>
  dplyr::mutate(deaths_class = cut(.data$deaths, c(-Inf, 9, 99, Inf), labels = c("<10", "10-99", ">=100"))) |>
  dplyr::group_by(.data$deaths_class) |>
  dplyr::summarise(
    n = dplyr::n(), median_rel = stats::median(.data$rel_diff),
    median_abs_rel = stats::median(abs(.data$rel_diff)),
    p95_abs_rel = stats::quantile(abs(.data$rel_diff), 0.95),
    within_2pct = mean(abs(.data$rel_diff) <= 0.02), .groups = "drop"
  )
dir.create("reproducibility/output", showWarnings = FALSE)
readr::write_csv(cmp, "reproducibility/output/rate_benchmark_all.csv")
readr::write_csv(summary_by_size, "reproducibility/output/rate_benchmark_summary.csv")
print(as.data.frame(summary_by_size), digits = 3)
