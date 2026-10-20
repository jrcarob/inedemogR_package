#' @include helpers.R life_tables.R
NULL

# Summary demographic indicators built on System B's age/sex-disaggregated
# province data (get_ine_population(), get_ine_births(), build_life_tables()).
# System A (get_ine_demog()) only has total counts with no age breakdown, so
# none of these indicators can be computed from it.

#' @noRd
summarise_age_bands <- function(pop_df, young_max, old_min, sex_col = "total") {
  pop_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(
      young = sum(.data[[sex_col]][.data$age <= young_max]),
      working = sum(.data[[sex_col]][.data$age > young_max & .data$age < old_min]),
      old = sum(.data[[sex_col]][.data$age >= old_min]),
      .groups = "drop"
    )
}

#' Age dependency ratios
#'
#' Computes youth, old-age, and total age dependency ratios per province and
#' year from age-disaggregated population data. Requires an age breakdown,
#' so it operates on [get_ine_population()]'s output, not
#' [get_ine_demog()]'s `population_total` indicator (which has no `age`
#' column and cannot be used here).
#'
#' @param pop_df Population tibble as returned by `get_ine_population()$data`
#'   (columns `nuts3_code`, `province_name`, `year`, `age`, `total`).
#' @param young_max Integer, the maximum age counted as "young" (default
#'   `14`, i.e. ages 0-14).
#' @param old_min Integer, the minimum age counted as "old" (default `65`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`,
#'   `youth_dependency_ratio`, `old_age_dependency_ratio`,
#'   `total_dependency_ratio` (all per 100 working-age population).
#' @examples
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 20, 70), total = c(100, 300, 150)
#' )
#' age_dependency_ratio(pop)
#' @export
age_dependency_ratio <- function(pop_df, young_max = 14L, old_min = 65L) {
  summarise_age_bands(pop_df, young_max, old_min) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name, year = .data$year,
      youth_dependency_ratio = .data$young / .data$working * 100,
      old_age_dependency_ratio = .data$old / .data$working * 100,
      total_dependency_ratio = (.data$young + .data$old) / .data$working * 100
    )
}

#' Aging index
#'
#' Computes the aging index (elderly per 100 children) per province and
#' year from age-disaggregated population data. Requires an age breakdown,
#' so it operates on [get_ine_population()]'s output, not
#' [get_ine_demog()]'s `population_total` indicator.
#'
#' @inheritParams age_dependency_ratio
#' @return A tibble with `nuts3_code`, `province_name`, `year`,
#'   `aging_index`.
#' @examples
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 20, 70), total = c(100, 300, 150)
#' )
#' aging_index(pop)
#' @export
aging_index <- function(pop_df, young_max = 14L, old_min = 65L) {
  summarise_age_bands(pop_df, young_max, old_min) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name, year = .data$year,
      aging_index = .data$old / .data$young * 100
    )
}

#' Sex ratio
#'
#' Computes the sex ratio (males per 100 females) per province and year
#' from age-disaggregated population data. Requires the `female`/`male`
#' columns [get_ine_population()] provides; [get_ine_demog()]'s
#' `population_total` indicator has no sex breakdown and cannot be used
#' here.
#'
#' @param pop_df Population tibble as returned by `get_ine_population()$data`
#'   (columns `nuts3_code`, `province_name`, `year`, `age`, `female`, `male`).
#' @param by_age Logical, keep the `age` column and compute a sex ratio per
#'   age (default `FALSE`, which aggregates over all ages first).
#' @return A tibble with `nuts3_code`, `province_name`, `year` (and `age` if
#'   `by_age = TRUE`), `sex_ratio`.
#' @examples
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 1), female = c(100, 95), male = c(105, 100)
#' )
#' sex_ratio(pop)
#' sex_ratio(pop, by_age = TRUE)
#' @export
sex_ratio <- function(pop_df, by_age = FALSE) {
  group_cols <- if (by_age) {
    c("nuts3_code", "province_name", "year", "age")
  } else {
    c("nuts3_code", "province_name", "year")
  }

  pop_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      female = sum(.data$female), male = sum(.data$male), .groups = "drop"
    ) |>
    dplyr::transmute(dplyr::across(dplyr::all_of(group_cols)), sex_ratio = .data$male / .data$female * 100)
}

#' Crude birth rate
#'
#' Computes the crude birth rate (births per 1,000 population) per province
#' and year, joining [get_ine_births()] and [get_ine_population()] output.
#'
#' @details
#' This is a *crude* birth rate (total births / total population), **not**
#' a total fertility rate (TFR). [get_ine_births()] itself has no
#' age-of-mother breakdown, so `crude_birth_rate()` cannot compute a TFR
#' on its own — for a true TFR, use [get_ine_births_by_age()] with
#' [age_specific_fertility_rate()]/[total_fertility_rate()] instead. Do not
#' substitute `crude_birth_rate()` for TFR in fertility analysis.
#'
#' @param births_df Births tibble as returned by `get_ine_births()$data`
#'   (columns `nuts3_code`, `province_name`, `year`, `total`).
#' @param pop_df Population tibble as returned by `get_ine_population()$data`
#'   (columns `nuts3_code`, `year`, `age`, `total`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `cbr`.
#' @examples
#' births <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023, total = 100
#' )
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", year = 2023, age = c(0, 1), total = c(5000, 5000)
#' )
#' crude_birth_rate(births, pop)
#' @export
crude_birth_rate <- function(births_df, pop_df) {
  pop_totals <- pop_df |>
    dplyr::group_by(.data$nuts3_code, .data$year) |>
    dplyr::summarise(population = sum(.data$total), .groups = "drop")

  births_df |>
    dplyr::select("nuts3_code", "province_name", "year", births = "total") |>
    dplyr::inner_join(pop_totals, by = c("nuts3_code", "year")) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name, year = .data$year,
      cbr = .data$births / .data$population * 1000
    )
}

#' General fertility rate
#'
#' Computes the general fertility rate (births per 1,000 women of
#' reproductive age) per province and year, joining [get_ine_births()]
#' and [get_ine_population()] output.
#'
#' @details
#' GFR improves on [crude_birth_rate()] by dividing births by the female
#' population actually at risk of childbearing (age `age_min`-`age_max`,
#' default 15-49) instead of the total population — this removes the
#' distortion [crude_birth_rate()] suffers when two provinces have
#' different age/sex structures (e.g. one with proportionally more
#' elderly residents) despite similar underlying fertility behavior.
#'
#' GFR is still **not** a total fertility rate (TFR): it is a single
#' aggregate rate over all reproductive-age women, not an age-specific
#' schedule. For a true TFR, use [get_ine_births_by_age()] with
#' [age_specific_fertility_rate()]/[total_fertility_rate()] instead.
#'
#' @param births_df Births tibble as returned by `get_ine_births()$data`
#'   (columns `nuts3_code`, `province_name`, `year`, `total`).
#' @param pop_df Population tibble as returned by `get_ine_population()$data`
#'   (columns `nuts3_code`, `year`, `age`, `female`).
#' @param age_min Integer, the youngest age counted as reproductive-age
#'   (default `15`).
#' @param age_max Integer, the oldest age counted as reproductive-age
#'   (default `49`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `gfr`.
#' @examples
#' births <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023, total = 100
#' )
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", year = 2023, age = c(10, 25, 40, 60),
#'   female = c(2000, 2500, 2500, 3000)
#' )
#' general_fertility_rate(births, pop)
#' @export
general_fertility_rate <- function(births_df, pop_df, age_min = 15L, age_max = 49L) {
  women_reproductive <- pop_df |>
    dplyr::filter(.data$age >= age_min, .data$age <= age_max) |>
    dplyr::group_by(.data$nuts3_code, .data$year) |>
    dplyr::summarise(women = sum(.data$female), .groups = "drop")

  births_df |>
    dplyr::select("nuts3_code", "province_name", "year", births = "total") |>
    dplyr::inner_join(women_reproductive, by = c("nuts3_code", "year")) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name, year = .data$year,
      gfr = .data$births / .data$women * 1000
    )
}

#' Crude death rate
#'
#' Computes the crude death rate (deaths per 1,000 population) per
#' province and year, joining [get_ine_deaths()] and [get_ine_population()]
#' output. The symmetric counterpart to [crude_birth_rate()] — see
#' [rate_of_natural_increase()] to combine the two.
#'
#' @param deaths_df Deaths tibble as returned by `get_ine_deaths()
#'   $data_provinces` (columns `nuts3_code`, `province_name`, `year`,
#'   `age`, `total`). Unlike [crude_birth_rate()]'s `births_df`, this is
#'   age-specific, so it is summed over age internally before computing
#'   the rate.
#' @param pop_df Population tibble as returned by `get_ine_population()$data`
#'   (columns `nuts3_code`, `year`, `age`, `total`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `cdr`.
#' @examples
#' deaths <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 1), total = c(1, 49)
#' )
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", year = 2023, age = c(0, 1), total = c(5000, 5000)
#' )
#' crude_death_rate(deaths, pop)
#' @export
crude_death_rate <- function(deaths_df, pop_df) {
  death_totals <- deaths_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(deaths = sum(.data$total), .groups = "drop")

  pop_totals <- pop_df |>
    dplyr::group_by(.data$nuts3_code, .data$year) |>
    dplyr::summarise(population = sum(.data$total), .groups = "drop")

  death_totals |>
    dplyr::inner_join(pop_totals, by = c("nuts3_code", "year")) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name, year = .data$year,
      cdr = .data$deaths / .data$population * 1000
    )
}

#' Infant mortality rate
#'
#' Computes the infant mortality rate (deaths under age 1 per 1,000 live
#' births) per province and year, joining [get_ine_deaths()] and
#' [get_ine_births()] output.
#'
#' @details
#' This is the standard demographic IMR: infant deaths in a year divided
#' by live births *in that same year* (the birth cohort those deaths are
#' drawn from), not by population — a different denominator convention
#' from [crude_death_rate()]. It is closely related to, but not
#' identical to, `q0` (probability of dying before age 1) in
#' [build_life_table()]'s output: `q0` is derived from the exposure-based
#' central death rate `m0` via the Andreev-Kingkade `a0` (per HMD Methods
#' Protocol V6), while IMR here uses the simpler, more standard
#' deaths/births ratio directly. The two will usually be close but need
#' not match exactly.
#'
#' @param deaths_df Deaths tibble as returned by `get_ine_deaths()
#'   $data_provinces` (columns `nuts3_code`, `province_name`, `year`,
#'   `age`, `total`).
#' @param births_df Births tibble as returned by `get_ine_births()$data`
#'   (columns `nuts3_code`, `year`, `total`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `imr`.
#' @examples
#' deaths <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 1, 65), total = c(3, 1, 40)
#' )
#' births <- tibble::tibble(nuts3_code = "ES111", year = 2023, total = 1000)
#' infant_mortality_rate(deaths, births)
#' @export
infant_mortality_rate <- function(deaths_df, births_df) {
  infant_deaths <- deaths_df |>
    dplyr::filter(.data$age == 0) |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(deaths = sum(.data$total), .groups = "drop")

  births_df |>
    dplyr::select("nuts3_code", "year", births = "total") |>
    dplyr::inner_join(infant_deaths, by = c("nuts3_code", "year")) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name, year = .data$year,
      imr = .data$deaths / .data$births * 1000
    )
}

#' Rate of natural increase
#'
#' Computes the rate of natural increase (RNI = CBR - CDR, per 1,000
#' population) per province and year, combining already-computed
#' [crude_birth_rate()] and [crude_death_rate()] output rather than
#' re-deriving from raw births/deaths/population — consistent with how
#' [life_expectancy_summary()] consumes [build_life_tables()]'s output
#' rather than recomputing it.
#'
#' @param cbr_df Output of [crude_birth_rate()] (columns `nuts3_code`,
#'   `province_name`, `year`, `cbr`).
#' @param cdr_df Output of [crude_death_rate()] (columns `nuts3_code`,
#'   `year`, `cdr`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `rni`. A
#'   positive value means births outnumber deaths (population growing
#'   from natural change alone, ignoring migration); negative means the
#'   reverse.
#' @examples
#' cbr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023, cbr = 6.5
#' )
#' cdr <- tibble::tibble(nuts3_code = "ES111", year = 2023, cdr = 12.3)
#' rate_of_natural_increase(cbr, cdr)
#' @export
rate_of_natural_increase <- function(cbr_df, cdr_df) {
  cbr_df |>
    dplyr::select("nuts3_code", "province_name", "year", "cbr") |>
    dplyr::inner_join(
      dplyr::select(cdr_df, "nuts3_code", "year", "cdr"),
      by = c("nuts3_code", "year")
    ) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name, year = .data$year,
      rni = .data$cbr - .data$cdr
    )
}

#' Birth-to-death ratio
#'
#' Computes the birth-to-death ratio (live births per death) per
#' geography and year. Unlike [age_dependency_ratio()]/[aging_index()]/
#' [sex_ratio()], this needs no age breakdown, so it operates directly on
#' [get_ine_demog()]'s totals (System A) rather than [get_ine_population()]
#' (System B) — fetch both `births_total` and `deaths_total` in one call
#' and pass the result straight through.
#'
#' @param df Output of `get_ine_demog(indicator = c("births_total",
#'   "deaths_total"))` (columns `GEOID`, `NAME`, `year`, `births_total`,
#'   `deaths_total`).
#' @return A tibble with `GEOID`, `NAME`, `year`, `birth_death_ratio`. A
#'   value of `2` means 2 live births per death; `0.5` means 1 birth per
#'   2 deaths.
#' @examples
#' df <- tibble::tibble(
#'   GEOID = c("15", "28"), NAME = c("A Coruna", "Madrid"), year = 2023,
#'   births_total = c(3000, 30000), deaths_total = c(6000, 25000)
#' )
#' birth_death_ratio(df)
#' @export
birth_death_ratio <- function(df) {
  dplyr::transmute(
    df, GEOID = .data$GEOID, NAME = .data$NAME, year = .data$year,
    birth_death_ratio = .data$births_total / .data$deaths_total
  )
}

#' Age-specific fertility rates (ASFR)
#'
#' Computes age-specific fertility rates per province, year, and single year
#' of mother's age, following INE's methodology for its Indicadores
#' Demograficos Basicos (INE, *Indicadores Demograficos Basicos: Metodologia*,
#' pp. 4 and 9-10):
#'
#' * **Exposure** is the mean annual female population, the average of the
#'   January-1 stocks of years `t` and `t + 1`. If the `t + 1` stock is not in
#'   `pop_df`, the `t` stock is used and the row is flagged with
#'   `is_boundary_year = TRUE` (with a warning).
#' * **Out-of-range births** are folded into the boundary ages: births to
#'   mothers younger than `age_min` (including INE's "under 15" group, coded
#'   `age = NA`, `age_group = "under15"`) are added to age `age_min`, and births
#'   to mothers older than `age_max` (INE's "50 and over") to age `age_max`.
#'   No births are dropped.
#'
#' @param births_age_df Tibble as returned by `get_ine_births_by_age()$data`
#'   (columns `nuts3_code`, `province_name`, `year`, `age`, `female`, `total`,
#'   and optionally `age_group`).
#' @param pop_df Population tibble as returned by `get_ine_population()$data`
#'   (columns `nuts3_code`, `year`, `age`, `female`), January-1 stocks.
#' @param age_min,age_max Integer bounds of the reproductive age span
#'   (default 15-49).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `age`,
#'   `births`, `women` (mean annual female population), `asfr` (births per
#'   woman), `asfr_female` (female births per woman, for
#'   [gross_reproduction_rate()] and [net_reproduction_rate()]),
#'   `asfr_per_1000` (`1000 * asfr`, for display), and `is_boundary_year`.
#' @examples
#' births_age <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), female = c(10, 25), total = c(20, 50)
#' )
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", year = rep(2023:2024, each = 2), age = c(20, 30, 20, 30),
#'   female = c(2000, 2500, 2100, 2400)
#' )
#' age_specific_fertility_rate(births_age, pop)
#' @export
age_specific_fertility_rate <- function(births_age_df, pop_df, age_min = 15L, age_max = 49L) {
  b <- births_age_df
  if ("age_group" %in% names(b)) {
    b$age[is.na(b$age) & b$age_group %in% "under15"] <- age_min
  }
  if (anyNA(b$age)) {
    warning(sum(is.na(b$age)), " birth rows with unknown mother's age dropped.")
    b <- b[!is.na(b$age), ]
  }
  births_by_age <- b |>
    dplyr::mutate(age = pmin(pmax(.data$age, age_min), age_max)) |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year, .data$age) |>
    dplyr::summarise(births = sum(.data$total), births_female = sum(.data$female), .groups = "drop")

  stocks <- pop_df |>
    dplyr::filter(.data$age >= age_min, .data$age <= age_max) |>
    dplyr::select("nuts3_code", "year", "age", "female")
  women <- stocks |>
    dplyr::left_join(
      dplyr::mutate(stocks, year = .data$year - 1L) |> dplyr::rename(female_next = "female"),
      by = c("nuts3_code", "year", "age")
    ) |>
    dplyr::mutate(
      is_boundary_year = is.na(.data$female_next),
      women = ifelse(.data$is_boundary_year, .data$female, (.data$female + .data$female_next) / 2)
    ) |>
    dplyr::select("nuts3_code", "year", "age", "women", "is_boundary_year")

  out <- births_by_age |>
    dplyr::inner_join(women, by = c("nuts3_code", "year", "age")) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name,
      year = .data$year, age = .data$age,
      births = .data$births, women = .data$women,
      asfr = .data$births / .data$women,
      asfr_female = .data$births_female / .data$women,
      asfr_per_1000 = 1000 * .data$asfr,
      is_boundary_year = .data$is_boundary_year
    )
  if (any(out$is_boundary_year)) {
    warning("No January-1 population for year t + 1 in ",
            paste(unique(out$year[out$is_boundary_year]), collapse = ", "),
            ": using the January-1 stock of year t as exposure (is_boundary_year = TRUE).")
  }
  out
}

#' Total fertility rate (TFR)
#'
#' Sum of [age_specific_fertility_rate()]'s `asfr` (births per woman) over
#' single years of age: the expected number of children per woman under the
#' year's age schedule.
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `tfr`
#'   (children per woman).
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(0.03, 0.08), asfr_female = c(0.015, 0.04)
#' )
#' total_fertility_rate(asfr)
#' @export
total_fertility_rate <- function(asfr_df) {
  asfr_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(tfr = sum(.data$asfr), .groups = "drop")
}

#' Mean age at childbearing (MAC)
#'
#' ASFR-weighted mean of the mid-points `x + 0.5` of the completed-age
#' intervals, as in INE's methodology.
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `mac` (years).
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(0.03, 0.08), asfr_female = c(0.015, 0.04)
#' )
#' mean_age_at_childbearing(asfr)
#' @export
mean_age_at_childbearing <- function(asfr_df) {
  asfr_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(mac = sum((.data$age + 0.5) * .data$asfr) / sum(.data$asfr), .groups = "drop")
}

#' Gross reproduction rate (GRR)
#'
#' Sum of [age_specific_fertility_rate()]'s `asfr_female` (female births per
#' woman): expected daughters per woman, ignoring mortality. Uses INE's
#' births by sex of the newborn directly rather than scaling the TFR by an
#' assumed sex ratio at birth.
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `grr`
#'   (daughters per woman).
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(0.03, 0.08), asfr_female = c(0.015, 0.04)
#' )
#' gross_reproduction_rate(asfr)
#' @export
gross_reproduction_rate <- function(asfr_df) {
  asfr_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(grr = sum(.data$asfr_female), .groups = "drop")
}

#' Net reproduction rate (NRR)
#'
#' `sum(asfr_female * Lx / l0)`: expected daughters per woman allowing for
#' the mother's survival, with `Lx` from the female period life table of the
#' same province and year ([build_life_tables()]'s `fltper`, radix
#' `l0 = 100000`).
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @param lt_df `build_life_tables()`'s `fltper` tibble (columns
#'   `nuts3_code`, `year`, `age`, `Lx`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `nrr`.
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(0.03, 0.08), asfr_female = c(0.015, 0.04)
#' )
#' fltper <- tibble::tibble(
#'   nuts3_code = "ES111", year = 2023, age = c(20, 30), Lx = c(99000, 98500)
#' )
#' net_reproduction_rate(asfr, fltper)
#' @export
net_reproduction_rate <- function(asfr_df, lt_df) {
  lx_by_age <- lt_df |> dplyr::select("nuts3_code", "year", "age", "Lx")

  asfr_df |>
    dplyr::inner_join(lx_by_age, by = c("nuts3_code", "year", "age")) |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(nrr = sum(.data$asfr_female * .data$Lx / 100000), .groups = "drop")
}

#' Life expectancy summary
#'
#' Extracts life expectancy at birth (`e0`) and at age 65 (`e65`) per
#' province and year from a [build_life_tables()] period life table.
#' Feeds [map_life_expectancy()], which bridges this summary with
#' [get_ine_geo()]'s province geometries.
#'
#' @param lt_df One of `build_life_tables()`'s `fltper`, `mltper`, or
#'   `bltper` tibbles (columns `nuts3_code`, `province_name`, `year`,
#'   `age`, `ex`).
#' @param sex Character label to attach to the output (`"female"`,
#'   `"male"`, or `"both"`) — purely descriptive, matching which life
#'   table (`fltper`/`mltper`/`bltper`) was passed in.
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `e0`,
#'   `e65`, `sex`.
#' @examples
#' lt <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(0, 65), ex = c(86.97, 20.5)
#' )
#' life_expectancy_summary(lt, sex = "female")
#' @export
life_expectancy_summary <- function(lt_df, sex = c("female", "male", "both")) {
  sex <- match.arg(sex)

  e0 <- lt_df |>
    dplyr::filter(.data$age == 0) |>
    dplyr::select("nuts3_code", "province_name", "year", e0 = "ex")
  e65 <- lt_df |>
    dplyr::filter(.data$age == 65) |>
    dplyr::select("nuts3_code", "year", e65 = "ex")

  dplyr::left_join(e0, e65, by = c("nuts3_code", "year")) |>
    dplyr::mutate(sex = sex)
}

#' Life expectancy at birth and at age 65, for one province
#'
#' Convenience wrapper for a single province: builds a period life table
#' (Andreev-Kingkade a0, Kannisto old-age smoothing, per HMD Methods
#' Protocol V6) from already-computed central death rates and extracts
#' life expectancy at birth (`e0`) and at age 65 (`e65`) for every year
#' present. Operates on [compute_death_rates()]'s `mx_1x1` output
#' directly — data already retrieved/computed via the mortality
#' pipeline — the same way [plot_lexis_diagram()] does, rather than
#' requiring [build_life_tables()] to be run for every province first
#' and [life_expectancy_summary()] applied afterwards.
#'
#' @param mx_df Output of the `mx_1x1` element of [compute_death_rates()]
#'   (rates `mx_*`, deaths `d_*`, and exposures `e_*` for all three sexes,
#'   which [build_life_tables()] needs for Kannisto smoothing).
#' @param province Character, a province name (regular expressions
#'   allowed, matched against `province_name` — same convention as
#'   `province` in [plot_lexis_diagram()]). Must match exactly one
#'   province.
#' @param sex Character, one of `"total"`, `"female"`, `"male"`.
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `e0`,
#'   `e65`, `sex`.
#' @examples
#' age <- 0:100
#' e <- 1e5
#' m <- 0.0003 * exp(0.07 * age)
#' mx <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023, age = age,
#'   mx_female = m, mx_male = 1.2 * m, mx_total = 1.1 * m,
#'   d_female = m * e, d_male = 1.2 * m * e, d_total = 2.2 * m * e,
#'   e_female = e, e_male = e, e_total = 2 * e
#' )
#' life_expectancy(mx, province = "A Coruna", sex = "female")
#' @export
life_expectancy <- function(mx_df, province, sex = c("total", "female", "male")) {
  sex <- match.arg(sex)

  df <- dplyr::filter(mx_df, stringr::str_detect(.data$province_name, province))
  matched <- unique(df$province_name)
  if (length(matched) == 0) stop("`province` did not match any province name.")
  if (length(matched) > 1) {
    stop(
      "`province` matched more than one province (", paste(matched, collapse = ", "),
      "); narrow the pattern to match exactly one."
    )
  }
  lt <- build_life_tables(df)
  lt_table <- switch(sex, female = lt$fltper, male = lt$mltper, total = lt$bltper)

  result <- life_expectancy_summary(lt_table, sex = if (sex == "total") "both" else sex)
  result$sex <- sex
  result
}
