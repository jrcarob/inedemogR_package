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
#' Computes age-specific fertility rates (births per 1,000 women of that
#' age) per province, year, and single year of age, joining
#' [get_ine_births_by_age()] and [get_ine_population()] output. This is
#' the schedule underlying [total_fertility_rate()],
#' [mean_age_at_childbearing()], [gross_reproduction_rate()], and
#' [net_reproduction_rate()] - unlike [general_fertility_rate()], which
#' collapses reproductive age into one aggregate rate,
#' `age_specific_fertility_rate()` keeps the full age schedule, the input
#' a true total fertility rate requires.
#'
#' @param births_age_df Tibble as returned by `get_ine_births_by_age()
#'   $data` (columns `nuts3_code`, `province_name`, `year`, `age`,
#'   `female`, `total`).
#' @param pop_df Population tibble as returned by `get_ine_population()
#'   $data` (columns `nuts3_code`, `year`, `age`, `female`).
#' @param age_min Integer, the youngest single-year age included
#'   (default `15`).
#' @param age_max Integer, the oldest single-year age included
#'   (default `49`).
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `age`,
#'   `asfr` (all births per 1,000 women of that age), `asfr_female`
#'   (births of female newborns per 1,000 women of that age - the input
#'   [gross_reproduction_rate()] and [net_reproduction_rate()] need).
#' @examples
#' births_age <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), female = c(10, 25), total = c(20, 50)
#' )
#' pop <- tibble::tibble(
#'   nuts3_code = "ES111", year = 2023, age = c(20, 30), female = c(2000, 2500)
#' )
#' age_specific_fertility_rate(births_age, pop)
#' @export
age_specific_fertility_rate <- function(births_age_df, pop_df, age_min = 15L, age_max = 49L) {
  births_by_age <- births_age_df |>
    dplyr::filter(!is.na(.data$age), .data$age >= age_min, .data$age <= age_max) |>
    dplyr::select(
      "nuts3_code", "province_name", "year", "age",
      births_total = "total", births_female = "female"
    )

  women <- pop_df |>
    dplyr::filter(.data$age >= age_min, .data$age <= age_max) |>
    dplyr::select("nuts3_code", "year", "age", women = "female")

  births_by_age |>
    dplyr::inner_join(women, by = c("nuts3_code", "year", "age")) |>
    dplyr::transmute(
      nuts3_code = .data$nuts3_code, province_name = .data$province_name,
      year = .data$year, age = .data$age,
      asfr = .data$births_total / .data$women * 1000,
      asfr_female = .data$births_female / .data$women * 1000
    )
}

#' Total fertility rate (TFR)
#'
#' Computes the total fertility rate (expected live births per woman
#' over her reproductive lifetime, under a given year's fertility
#' schedule) per province and year, by summing [age_specific_fertility_rate()]
#' over single years of age. This is the true TFR that
#' [crude_birth_rate()]'s and [general_fertility_rate()]'s documentation
#' both note the package previously could not compute, since it requires
#' the age-of-mother breakdown [get_ine_births_by_age()] retrieves.
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `tfr`.
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(30, 80), asfr_female = c(15, 40)
#' )
#' total_fertility_rate(asfr)
#' @export
total_fertility_rate <- function(asfr_df) {
  asfr_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(tfr = sum(.data$asfr) / 1000, .groups = "drop")
}

#' Mean age at childbearing (MAC)
#'
#' Computes the mean age at childbearing (the ASFR-weighted average age
#' of mothers at birth) per province and year, from
#' [age_specific_fertility_rate()] output.
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `mac`.
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(30, 80), asfr_female = c(15, 40)
#' )
#' mean_age_at_childbearing(asfr)
#' @export
mean_age_at_childbearing <- function(asfr_df) {
  asfr_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(mac = sum(.data$age * .data$asfr) / sum(.data$asfr), .groups = "drop")
}

#' Gross reproduction rate (GRR)
#'
#' Computes the gross reproduction rate (expected daughters per woman
#' over her reproductive lifetime, ignoring mortality) per province and
#' year, summing [age_specific_fertility_rate()]'s `asfr_female` column -
#' the actual female-newborn-specific rate INE's age-of-mother table
#' provides, rather than the more common approximation of scaling
#' [total_fertility_rate()] by an assumed constant sex ratio at birth.
#' See [net_reproduction_rate()] to additionally account for mortality
#' before/during reproductive age.
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `grr`.
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(30, 80), asfr_female = c(15, 40)
#' )
#' gross_reproduction_rate(asfr)
#' @export
gross_reproduction_rate <- function(asfr_df) {
  asfr_df |>
    dplyr::group_by(.data$nuts3_code, .data$province_name, .data$year) |>
    dplyr::summarise(grr = sum(.data$asfr_female) / 1000, .groups = "drop")
}

#' Net reproduction rate (NRR)
#'
#' Computes the net reproduction rate (expected daughters per woman over
#' her reproductive lifetime, accounting for the mother's own survival to
#' each reproductive age) per province and year, weighting
#' [age_specific_fertility_rate()]'s `asfr_female` by the female
#' life-table survivorship function `Lx` from [build_life_tables()]'s
#' `fltper` (person-years lived in the age interval, per HMD Methods
#' Protocol V6 - the same `Lx` [life_expectancy_summary()]'s `ex` is
#' derived from). Unlike [gross_reproduction_rate()], which assumes every
#' woman survives to bear children at every age, NRR is the mortality-
#' adjusted figure: NRR < GRR whenever there is any mortality before the
#' end of the reproductive span.
#'
#' @param asfr_df Output of [age_specific_fertility_rate()].
#' @param lt_df `build_life_tables()`'s `fltper` tibble (columns
#'   `nuts3_code`, `year`, `age`, `Lx`) - must be the *female* life table,
#'   since NRR tracks mothers' own survival.
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `nrr`.
#' @examples
#' asfr <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = c(20, 30), asfr = c(30, 80), asfr_female = c(15, 40)
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
    # 100000 = the life-table radix (l0), matching build_life_table()'s lx[1].
    dplyr::summarise(nrr = sum(.data$asfr_female / 1000 * .data$Lx / 100000), .groups = "drop")
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
#'   (columns `nuts3_code`, `province_name`, `year`, `age`, `mx_female`,
#'   `mx_male`, `mx_total`). All three `mx_*` columns must be present
#'   regardless of `sex`, since the underlying life-table construction
#'   builds all three sex tables together.
#' @param province Character, a province name (regular expressions
#'   allowed, matched against `province_name` — same convention as
#'   `province` in [plot_lexis_diagram()]). Must match exactly one
#'   province.
#' @param sex Character, one of `"total"`, `"female"`, `"male"`.
#' @return A tibble with `nuts3_code`, `province_name`, `year`, `e0`,
#'   `e65`, `sex`.
#' @examples
#' # A Gompertz-like mx curve spanning to a realistic terminal age (100):
#' # build_life_table()'s open-interval formula at the terminal age
#' # (Lx = lx / mx) assumes that age is genuinely old (high mx), so a
#' # short age range with a low terminal mx would produce a nonsensical
#' # "remaining life expectancy" in the thousands of years.
#' age <- 0:100
#' mx <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023, age = age,
#'   mx_female = 0.0003 * exp(0.07 * age),
#'   mx_male = 0.00035 * exp(0.072 * age),
#'   mx_total = 0.00033 * exp(0.071 * age)
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
  nuts3 <- unique(df$nuts3_code)

  lt <- build_life_tables_one(df, nuts3, matched)
  lt_table <- switch(sex, female = lt$fltper, male = lt$mltper, total = lt$bltper)

  result <- life_expectancy_summary(lt_table, sex = if (sex == "total") "both" else sex)
  result$sex <- sex
  result
}
