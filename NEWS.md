# inedemogR 0.1.0

## System A: quick multi-geography totals

* `get_ine_demog()` retrieves live population, births, and deaths totals
  from INE via `ineapir::get_data_table()`, at whichever geographic level
  (municipality or province) each indicator is actually published at.
* `get_ine_geo()` retrieves municipality/province geometries via
  `mapSpain`, with an option to shift the Canary Islands next to the
  mainland for compact national maps.
* `list_ine_indicators()` and `update_ine_data()` round out data
  discovery and local cache refresh.
* `plot_ine_map()` provides a highlight-region diagnostic map; general
  choropleths are covered by System B's `map_indicator()` below.

## System B: age/sex-disaggregated province data and the mortality pipeline

* `get_ine_births()`, `get_ine_births_by_age()`, `get_ine_deaths()`, and
  `get_ine_population()` retrieve province-level, age/sex-disaggregated
  data directly from INE's Tempus3 API, following the Spanish
  subnational Human Mortality Database (SHMD) protocol.
  `get_ine_births_by_age()` retrieves single-year age-of-mother birth
  counts, the input the fertility schedule functions below need.
* `compute_exposure()`, `compute_death_rates()`, and `build_life_table()`/
  `build_life_tables()` implement exposure-to-risk, central death rate
  (1x1, 5x1, age-standardised), and single-year period life table
  construction per HMD Methods Protocol V6 (Andreev-Kingkade a0 at age
  0, Kannisto old-age smoothing).
* `build_abridged_life_table()`/`build_abridged_life_tables()` build
  standard abridged (5-year age group) period life tables directly from
  the 5x1 central death rates, complementing the single-year tables
  above - useful for comparing against other agencies' published
  abridged tables.
* `download_ine_data()` runs any subset of the pipeline and writes
  results to CSV and/or HMD-format `.txt` files for use outside R.
* A `validate_*()` family (`validate_population()`, `validate_births()`,
  `validate_births_by_age()`, `validate_deaths()`,
  `validate_deaths_age()`, `validate_exposure()`,
  `validate_death_rates()`, `validate_life_table()`,
  `validate_abridged_life_table()`) flags data-quality issues
  (suppressed cells, non-monotonic life tables, implausible exposure)
  without failing hard.

## Summary demographic indicators

* `age_dependency_ratio()`, `aging_index()`, and `sex_ratio()` compute
  population-structure indicators from age-disaggregated province data.
* `crude_birth_rate()`, `crude_death_rate()`, `general_fertility_rate()`,
  `infant_mortality_rate()`, and `rate_of_natural_increase()` compute
  CBR, CDR, GFR, IMR, and RNI.
* `age_specific_fertility_rate()`, `total_fertility_rate()`,
  `mean_age_at_childbearing()`, `gross_reproduction_rate()`, and
  `net_reproduction_rate()` build the full age-specific fertility
  schedule (ASFR/TFR/MAC/GRR/NRR) from age-of-mother birth counts - a
  true total fertility rate, which `crude_birth_rate()`/
  `general_fertility_rate()` cannot compute on their own.
* `birth_death_ratio()` computes births per death directly from System
  A's totals, needing no age breakdown.
* `life_expectancy_summary()` and `life_expectancy()` extract e0/e65
  from a period life table.
* `decompose_life_expectancy()` attributes a difference in life
  expectancy at birth between two life tables (two provinces, or one
  province across two years) to age-specific contributions, via
  Arriaga's (1984, exact) and Pollard's (1988, approximate) methods.

## Visualization

* `plot_population_pyramid()` and `plot_demog_trend()` provide
  population pyramid and generic indicator time-series charts.
* `map_indicator()` provides a general-purpose choropleth (binned or
  continuous) for any geography-keyed tibble; `map_life_expectancy()`
  is a thin wrapper bridging the mortality pipeline's life tables onto
  province geometry.
* `plot_lexis_diagram()` draws an age x year mortality surface with
  birth-cohort diagonals.

## Documentation

* `vignette("inedemogR-tutorial")` is a single comprehensive tutorial
  covering data retrieval/cleaning/storage (Part I) and demographic
  analysis/visualization (Part II), with the exact formulas implemented
  in code and worked examples reproducing the scripts in `examples/`.
