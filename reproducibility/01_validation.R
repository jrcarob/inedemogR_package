# Validation study for inedemogR 0.2.0 (SoftwareX revision).
#
# Uses only the frozen inputs in reproducibility/data (see
# 00_freeze_inputs.R), so every number is reproducible offline:
#
#   Rscript reproducibility/01_validation.R
#
# Writes CSV tables and validation_summary.md to reproducibility/output.
#
# Predefined tolerance (fixed before the comparison was run): the package and
# INE estimate the same quantity (period e0/e65 from a complete single-age
# table) but differ in exposure (mean of January-1 stocks vs exact exposure
# from death microdata), ax (0.5 / Andreev-Kingkade vs observed), and old-age
# rates (Kannisto from Y vs observed to 100+). We regard |difference| <= 0.25
# years in e0 and in e65 as agreement.

devtools::load_all(quiet = TRUE)
set.seed(20261005)
data_dir <- "reproducibility/data"
out_dir <- "reproducibility/output"
dir.create(out_dir, showWarnings = FALSE)
tol <- 0.25
years <- 2002:2024

quiet <- function(expr) suppressWarnings(suppressMessages(expr))
pop <- readRDS(file.path(data_dir, "population_56945_100.rds"))$data
deaths <- readRDS(file.path(data_dir, "deaths_6545_6547_100.rds"))$data_provinces
ine <- readRDS(file.path(data_dir, "ine_life_tables_27155.rds"))

# `exposure_deaths` may carry a cohort column (Lexis-triangle sensitivity);
# rates always use the INE deaths by age and year.
pipeline <- function(deaths, kannisto_age = NULL, fit_min = 80L, exposure_deaths = deaths) {
  ex <- quiet(compute_exposure(pop, exposure_deaths))$data
  rates <- quiet(compute_death_rates(deaths, ex))
  lt <- quiet(build_life_tables(rates$mx_1x1, kannisto_age = kannisto_age, fit_min = fit_min))
  list(rates = rates, lt = lt)
}
e_summary <- function(lt) {
  dplyr::bind_rows(
    female = life_expectancy_summary(lt$fltper, "female"),
    male = life_expectancy_summary(lt$mltper, "male"),
    total = life_expectancy_summary(lt$bltper, "both"),
    .id = "sex_key"
  ) |>
    dplyr::select(-"sex") |>
    dplyr::rename(sex = "sex_key") |>
    dplyr::filter(.data$year %in% years)
}

# ---- A. Main pipeline on frozen inputs -------------------------------------
base <- pipeline(deaths)
lt <- base$lt
coverage <- tibble::tibble(
  tables_built = nrow(lt$qc), tables_flagged = sum(!lt$qc$passed),
  province_years_withheld = nrow(lt$failed) / 3,
  withheld_years = paste(range(lt$failed$year), collapse = "-"),
  built_years = paste(range(lt$qc$year), collapse = "-")
)
ours <- e_summary(lt)

size <- pop |>
  dplyr::filter(.data$year == 2024) |>
  dplyr::group_by(.data$nuts3_code) |>
  dplyr::summarise(pop_2024 = sum(.data$total)) |>
  dplyr::mutate(size_class = cut(.data$pop_2024, c(0, 2e5, 1e6, Inf),
                                 labels = c("<200k", "200k-1M", ">1M")))

# ---- B. Benchmark against INE provincial life tables -----------------------
sex_map <- c("Mujeres" = "female", "Hombres" = "male", "Ambos sexos" = "total")
ine_e <- ine |>
  dplyr::filter(
    .data$life_table_function == "Esperanza de vida",
    .data$age_group %in% c("0 años", "De 65 a 69 años"),
    .data$year %in% years, grepl("^[0-9]{2} ", .data$province)
  ) |>
  dplyr::mutate(
    ine_code = substr(.data$province, 1, 2), sex = unname(sex_map[.data$sex]),
    measure = ifelse(.data$age_group == "0 años", "e0", "e65")
  ) |>
  dplyr::left_join(dplyr::select(province_lookup, "ine_code", "nuts3_code"), by = "ine_code") |>
  dplyr::select("nuts3_code", "year", "sex", "measure", ine = "value")

benchmark <- ours |>
  tidyr::pivot_longer(c("e0", "e65"), names_to = "measure", values_to = "inedemogR") |>
  dplyr::inner_join(ine_e, by = c("nuts3_code", "year", "sex", "measure")) |>
  dplyr::left_join(size, by = "nuts3_code") |>
  dplyr::mutate(diff = .data$inedemogR - .data$ine)

summarise_diff <- function(df, ...) {
  df |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n = dplyr::n(), mean_signed = mean(.data$diff), mean_abs = mean(abs(.data$diff)),
      p95_abs = stats::quantile(abs(.data$diff), 0.95), max_abs = max(abs(.data$diff)),
      within_tol = mean(abs(.data$diff) <= tol), .groups = "drop"
    )
}
bench_by_measure <- summarise_diff(benchmark, .data$measure, .data$sex)
bench_by_size <- summarise_diff(benchmark, .data$measure, .data$size_class)
worst <- benchmark |> dplyr::arrange(dplyr::desc(abs(.data$diff))) |> utils::head(10)

# INE closes the Ceuta and Melilla tables at 90+ (methodology, Section 3,
# footnote 1) with the observed mean time lived by that year's decedents;
# every other province closes at 95+. Report the two groups separately and
# compare remaining life expectancy at 90 directly.
cm <- c("ES630", "ES640")
bench_by_closure <- summarise_diff(
  dplyr::mutate(benchmark, ine_open_age = ifelse(.data$nuts3_code %in% cm, "90+ (Ceuta, Melilla)", "95+ (50 provinces)")),
  .data$measure, .data$ine_open_age
)
ine_e90 <- ine |>
  dplyr::filter(.data$life_table_function == "Esperanza de vida", .data$age_group == "90 y más años",
                .data$year %in% years, substr(.data$province, 1, 2) %in% c("51", "52")) |>
  dplyr::mutate(ine_code = substr(.data$province, 1, 2), sex = unname(sex_map[.data$sex])) |>
  dplyr::left_join(dplyr::select(province_lookup, "ine_code", "nuts3_code"), by = "ine_code") |>
  dplyr::select("nuts3_code", "year", "sex", ine_e90 = "value")
e90_cm <- dplyr::bind_rows(female = lt$fltper, male = lt$mltper, total = lt$bltper, .id = "sex") |>
  dplyr::filter(.data$age == 90, .data$nuts3_code %in% cm, .data$year %in% years) |>
  dplyr::select("nuts3_code", "province_name", "year", "sex", inedemogR_e90 = "ex") |>
  dplyr::inner_join(ine_e90, by = c("nuts3_code", "year", "sex")) |>
  dplyr::group_by(.data$province_name) |>
  dplyr::summarise(n = dplyr::n(), mean_inedemogR_e90 = mean(.data$inedemogR_e90),
                   mean_ine_e90 = mean(.data$ine_e90),
                   mean_diff = mean(.data$inedemogR_e90 - .data$ine_e90), .groups = "drop")

# ---- C. Where do the differences come from? (Arriaga by age group) ---------
groups <- c(0, 1, seq(5, 95, 5))
ine_group_age <- function(g) {
  ifelse(g == "0 años", 0, ifelse(g == "95 y más años", 95,
         as.numeric(sub("De ([0-9]+) a.*", "\\1", g))))
}
ine_tables <- ine |>
  dplyr::filter(
    .data$year %in% years, grepl("^[0-9]{2} ", .data$province),
    .data$life_table_function %in% c("Supervivientes", "Tiempo por vivir"),
    .data$age_group != "90 y más años"
  ) |>
  dplyr::mutate(
    ine_code = substr(.data$province, 1, 2), sex = unname(sex_map[.data$sex]),
    age = ine_group_age(.data$age_group),
    fun = ifelse(.data$life_table_function == "Supervivientes", "lx", "Tx")
  ) |>
  dplyr::left_join(dplyr::select(province_lookup, "ine_code", "nuts3_code"), by = "ine_code") |>
  dplyr::select("nuts3_code", "year", "sex", "age", "fun", "value") |>
  tidyr::pivot_wider(names_from = "fun", values_from = "value")

group_table <- function(lx, Tx, age) {
  ok <- is.finite(lx) & is.finite(Tx)
  lx <- lx[ok]
  Tx <- Tx[ok]
  age <- age[ok]
  Lx <- Tx - c(Tx[-1], 0)
  tibble::tibble(age = as.numeric(age), mx = NA_real_, lx = lx, Lx = Lx, Tx = Tx, ex = Tx / lx)
}
ours_grouped <- function(t) {
  t <- t[t$age %in% groups, ]
  group_table(t$lx, t$Tx, t$age)
}
age_band <- function(a) cut(a, c(-1, 0, 79, Inf), labels = c("age 0", "ages 1-79", "ages 80+"))

all_lt <- dplyr::bind_rows(female = lt$fltper, male = lt$mltper, total = lt$bltper, .id = "sex") |>
  dplyr::filter(.data$year %in% years)
keys <- dplyr::distinct(benchmark, .data$nuts3_code, .data$year, .data$sex)
decomp <- purrr::pmap_dfr(keys, function(nuts3_code, year, sex) {
  o <- all_lt[all_lt$nuts3_code == nuts3_code & all_lt$year == year & all_lt$sex == sex, ]
  i <- ine_tables[ine_tables$nuts3_code == nuts3_code & ine_tables$year == year & ine_tables$sex == sex, ]
  i <- i[order(i$age), ]
  ti <- group_table(i$lx, i$Tx, i$age)
  to <- ours_grouped(o)
  to <- to[to$age %in% ti$age, ]
  to$Lx <- to$Tx - c(to$Tx[-1], 0)
  d <- decompose_life_expectancy(ti, to, "arriaga")
  tibble::tibble(
    nuts3_code = nuts3_code, year = year, sex = sex,
    band = age_band(d$age), contribution = d$contribution
  )
}) |>
  dplyr::group_by(.data$nuts3_code, .data$year, .data$sex, .data$band) |>
  dplyr::summarise(contribution = sum(.data$contribution), .groups = "drop")
decomp_summary <- decomp |>
  dplyr::group_by(.data$band) |>
  dplyr::summarise(
    mean_signed = mean(.data$contribution), mean_abs = mean(abs(.data$contribution)),
    max_abs = max(abs(.data$contribution)), .groups = "drop"
  )

# ---- D. Sensitivity: Lexis-triangle split of deaths ------------------------
with_split <- function(pi_lower, pi_lower_age0 = pi_lower) {
  share <- ifelse(deaths$age == 0, pi_lower_age0, pi_lower)
  lower <- dplyr::mutate(deaths, cohort = .data$year - .data$age,
                         dplyr::across(c("female", "male", "total"), ~ .x * share))
  upper <- dplyr::mutate(deaths, cohort = .data$year - .data$age - 1,
                         dplyr::across(c("female", "male", "total"), ~ .x * (1 - share)))
  dplyr::bind_rows(lower, upper)
}
compare_to_base <- function(alt, scenario) {
  e_summary(alt$lt) |>
    tidyr::pivot_longer(c("e0", "e65"), names_to = "measure", values_to = "alt") |>
    dplyr::inner_join(
      tidyr::pivot_longer(ours, c("e0", "e65"), names_to = "measure", values_to = "base"),
      by = c("nuts3_code", "province_name", "year", "sex", "measure")
    ) |>
    dplyr::left_join(size, by = "nuts3_code") |>
    dplyr::mutate(scenario = scenario, diff = .data$alt - .data$base)
}
exposure_sens <- dplyr::bind_rows(
  compare_to_base(pipeline(deaths, exposure_deaths = with_split(0.45)), "45% of deaths in lower triangle"),
  compare_to_base(pipeline(deaths, exposure_deaths = with_split(0.55)), "55% of deaths in lower triangle"),
  compare_to_base(pipeline(deaths, exposure_deaths = with_split(0.5, 0.85)), "85% of infant deaths in lower triangle")
)

# ---- E. Sensitivity: old-age model -----------------------------------------
no_smoothing <- function() {
  m <- base$rates$mx_1x1
  out <- list()
  for (s in c("female", "male")) {
    tabs <- dplyr::group_by(m, .data$nuts3_code, .data$province_name, .data$year) |>
      dplyr::group_modify(function(d, k) {
        r <- tryCatch(build_life_table(data.frame(age = d$age, mx = d[[paste0("mx_", s)]]), s),
                      error = function(e) NULL)
        if (is.null(r)) tibble::tibble() else r
      }) |>
      dplyr::ungroup()
    out[[s]] <- tabs
  }
  list(lt = list(fltper = out$female, mltper = out$male,
                 bltper = out$female[0, ]))
}
unsmoothed <- no_smoothing()
oldage_sens <- dplyr::bind_rows(
  compare_to_base(pipeline(deaths, kannisto_age = 80), "Y fixed at 80"),
  compare_to_base(pipeline(deaths, kannisto_age = 85), "Y fixed at 85"),
  compare_to_base(pipeline(deaths, kannisto_age = 90), "Y fixed at 90"),
  compare_to_base(pipeline(deaths, kannisto_age = 95), "Y fixed at 95"),
  compare_to_base(pipeline(deaths, fit_min = 85), "fit window 85-99"),
  compare_to_base(unsmoothed, "no smoothing (observed rates to 100+)")
)

# Attribution check: if old-age smoothing explains the INE gap, tables built
# from observed rates (like INE's) should agree more closely with INE.
unsmoothed_bench <- dplyr::bind_rows(
  female = life_expectancy_summary(unsmoothed$lt$fltper, "female"),
  male = life_expectancy_summary(unsmoothed$lt$mltper, "male"), .id = "sex_key"
) |>
  dplyr::select(-"sex") |>
  dplyr::rename(sex = "sex_key") |>
  dplyr::filter(.data$year %in% years) |>
  tidyr::pivot_longer(c("e0", "e65"), names_to = "measure", values_to = "unsmoothed") |>
  dplyr::inner_join(benchmark, by = c("nuts3_code", "province_name", "year", "sex", "measure")) |>
  dplyr::mutate(diff_smoothed = .data$diff, diff = .data$unsmoothed - .data$ine)
attribution <- dplyr::bind_rows(
  summarise_diff(dplyr::mutate(unsmoothed_bench, diff = .data$diff_smoothed), .data$measure, .data$size_class) |>
    dplyr::mutate(tables = "Kannisto (package default)", .before = 1),
  summarise_diff(unsmoothed_bench, .data$measure, .data$size_class) |>
    dplyr::mutate(tables = "observed rates to 100+", .before = 1)
)
sens_summary <- function(df) {
  df |>
    dplyr::group_by(.data$scenario, .data$measure, .data$size_class) |>
    dplyr::summarise(
      n = dplyr::n(), mean_signed = mean(.data$diff), mean_abs = mean(abs(.data$diff)),
      max_abs = max(abs(.data$diff)), .groups = "drop"
    )
}

# ---- F. Stability: Poisson bootstrap of deaths (2024) ----------------------
B <- 200
d24 <- dplyr::filter(deaths, .data$year == 2024)
boot <- purrr::map_dfr(seq_len(B), function(b) {
  d <- d24
  d$female <- stats::rpois(nrow(d), d$female)
  d$male <- stats::rpois(nrow(d), d$male)
  d$total <- d$female + d$male
  r <- pipeline(d)
  e_summary(r$lt) |>
    dplyr::filter(.data$year == 2024, .data$sex != "total") |>
    dplyr::mutate(b = b)
})
boot_sd <- boot |>
  dplyr::group_by(.data$nuts3_code, .data$province_name, .data$sex) |>
  dplyr::summarise(sd_e0 = stats::sd(.data$e0), sd_e65 = stats::sd(.data$e65), .groups = "drop") |>
  dplyr::left_join(size, by = "nuts3_code")
boot_by_size <- boot_sd |>
  dplyr::group_by(.data$sex, .data$size_class) |>
  dplyr::summarise(median_sd_e0 = stats::median(.data$sd_e0), max_sd_e0 = max(.data$sd_e0),
                   median_sd_e65 = stats::median(.data$sd_e65), max_sd_e65 = max(.data$sd_e65),
                   .groups = "drop")

# Figure 3 decomposition (A Coruna vs Madrid, females, 2024) under resampling.
fig3 <- function(lt) {
  f <- lt$fltper[lt$fltper$year == 2024, ]
  a <- f[f$province_name == "A Coruna", ]
  m <- f[f$province_name == "Madrid", ]
  purrr::map_dfr(c("arriaga", "pollard"), function(meth) {
    d <- decompose_life_expectancy(a, m, meth)
    tibble::tibble(
      method = meth, gap = attr(d, "e0_difference"), residual = attr(d, "residual"),
      ages_0_64 = sum(d$contribution[d$age < 65]),
      ages_65_79 = sum(d$contribution[d$age >= 65 & d$age < 80]),
      ages_80_plus = sum(d$contribution[d$age >= 80])
    )
  })
}
fig3_point <- fig3(lt)
fig3_boot <- purrr::map_dfr(seq_len(B), function(b) {
  d <- d24
  d$female <- stats::rpois(nrow(d), d$female)
  d$male <- stats::rpois(nrow(d), d$male)
  d$total <- d$female + d$male
  fig3(pipeline(d)$lt)
}) |>
  dplyr::group_by(.data$method) |>
  dplyr::summarise(dplyr::across(
    c("gap", "residual", "ages_0_64", "ages_65_79", "ages_80_plus"),
    list(lo = ~ stats::quantile(.x, 0.025), hi = ~ stats::quantile(.x, 0.975))
  ))

# ---- Write outputs ---------------------------------------------------------
write <- function(x, name) readr::write_csv(x, file.path(out_dir, paste0(name, ".csv")))
write(coverage, "coverage")
write(benchmark, "ine_benchmark_all")
write(bench_by_measure, "ine_benchmark_by_sex")
write(bench_by_size, "ine_benchmark_by_size")
write(bench_by_closure, "ine_benchmark_by_closure")
write(e90_cm, "ine_e90_ceuta_melilla")
write(decomp, "ine_gap_decomposition_all")
write(decomp_summary, "ine_gap_decomposition_summary")
write(sens_summary(exposure_sens), "sensitivity_exposure")
write(sens_summary(oldage_sens), "sensitivity_oldage")
write(attribution, "ine_gap_attribution")
write(boot_sd, "bootstrap_sd_2024")
write(boot_by_size, "bootstrap_by_size_2024")
write(fig3_point, "figure3_point")
write(fig3_boot, "figure3_bootstrap")

md <- function(x, digits = 3) {
  x <- dplyr::mutate(x, dplyr::across(dplyr::where(is.numeric), ~ round(.x, digits)))
  c(paste("|", paste(names(x), collapse = " | "), "|"),
    paste("|", paste(rep("---", ncol(x)), collapse = " | "), "|"),
    apply(x, 1, function(r) paste("|", paste(r, collapse = " | "), "|")), "")
}
writeLines(c(
  "# Validation summary (inedemogR 0.2.0)", "",
  paste("Generated", format(Sys.time(), tz = "UTC", usetz = TRUE), "from frozen inputs in reproducibility/data.",
        "Tolerance fixed in advance:", tol, "years."), "",
  "## Coverage", md(coverage),
  "## Benchmark against INE provincial life tables (2002-2024)", md(bench_by_measure), md(bench_by_size),
  "By INE's closing age group:", md(bench_by_closure),
  "Remaining life expectancy at 90, Ceuta and Melilla:", md(e90_cm),
  "Largest differences:", md(dplyr::select(worst, "province_name", "year", "sex", "measure", "inedemogR", "ine", "diff")),
  "## Age decomposition of the inedemogR - INE difference in e0 (Arriaga)", md(decomp_summary),
  "## Attribution: same comparison with unsmoothed tables (same province-sex-years)", md(attribution),
  "## Sensitivity: Lexis-triangle split", md(sens_summary(exposure_sens)),
  "## Sensitivity: old-age model", md(sens_summary(oldage_sens)),
  paste("## Stability: Poisson bootstrap of 2024 deaths (B =", B, ")"), md(boot_by_size),
  "## Figure 3 decomposition (A Coruna vs Madrid, females, 2024)", md(fig3_point, 4), md(fig3_boot, 4)
), file.path(out_dir, "validation_summary.md"))
cat(readLines(file.path(out_dir, "validation_summary.md")), sep = "\n")
