# inedemogR 0.2.0

This release corrects numerical errors in 0.1.0 identified in a methodological
audit and validation of the package against the Human Mortality Database
protocol and the official tables of the Spanish statistics institute (INE).
Several corrections change numerical results; their effect on every number
reported in the accompanying article is tabulated by
`reproducibility/02_manuscript_numbers.R` in the GitHub repository.

## Breaking changes and corrected results

* `andreev_kingkade_a0()` now implements the sex-specific, three-segment
  Andreev-Kingkade (2015) formulas of HMD Methods Protocol V6, Table 3. 0.1.0
  used Coale-Demeny-style coefficients (e.g. female a0 at m0 = 0.004 was
  0.064 instead of 0.141).
* Life tables use `ax = 0.5` at every age except 0 (0.1.0 used `a1 = 0.4`);
  combined-sex a0 is the death-weighted average of the sex-specific values
  (HMD V6 Eq. 77).
* `build_life_tables()` replaces the unweighted logit regression with the HMD
  V6 Kannisto model fitted by Poisson maximum likelihood on deaths and
  exposures at ages 80-99 (parameters constrained to be non-negative), with
  the data-dependent replacement age Y (100-deaths rule, 80 <= Y <= 95) and
  smoothed female-exposure weights for the combined-sex table. It now needs
  the `d_*` and `e_*` columns that `compute_death_rates()` returns. New
  arguments `kannisto_age` and `fit_min` support sensitivity analysis.
* `compute_exposure()`: the Lexis correction is `(D_L - D_U) / 6` (HMD V6
  Eq. 57) with conventional triangle labels; 0.1.0 used `(D_U - D_L) / 2`
  with reversed labels. Only affects deaths supplied with a `cohort` column;
  INE deaths use the even split, where the correction is zero.
* European Standard Population 2013 weights sum to 100,000 (95+ = 200); 0.1.0
  gave ages 95+ a weight of 600, inflating ASDRs by about 20 per cent.
* Abridged life tables preserve `nmx = ndx / nLx` in every closed group
  (Greville `nax` with the Chiang conversion, constant-hazard fallback);
  0.1.0 combined an exponential `nqx` with a linear `nLx`.
* `decompose_life_expectancy(method = "pollard")` integrates the open age
  interval exactly and closed intervals by the midpoint rule; 0.1.0 treated
  the open interval as a point and recovered only 22.5 per cent of a change
  confined to it. Both methods return the attributes `e0_difference` and
  `residual`.
* Fertility follows INE's conventions: exposure is the mean of consecutive
  January-1 female stocks; births outside 15-49 are folded into ages 15 and
  49 instead of dropped; the mean age at childbearing uses mid-points
  `x + 0.5`. `asfr` and `asfr_female` are now births **per woman** (TFR, GRR
  and NRR are plain sums); `asfr_per_1000` is provided for display.

## Missing data and validation

* An explicit `NA` death count stays `NA` (0.1.0 turned it into a zero rate);
  a province-year with no death records is excluded rather than treated as
  zero mortality. Ages absent from INE's deaths table within a reported
  province-year remain structural zeros.
* `build_life_table()` rejects non-contiguous ages and non-finite rates.
  `build_life_tables()` withholds province-years with missing exposure (e.g.
  INE populations top-coded at 85+ before 2002) or too few ages to fit the
  Kannisto model, and reports them in `$failed`; `$qc` is now a tibble.
* `validate_life_table()` and `validate_abridged_life_table()` also fail on
  missing ages, non-finite values, a broken `mx = dx / Lx` identity, and
  (full tables) implausible e0; `validate_death_rates()` checks age coverage.
  ASDR is `NA` unless all ages have finite rates.

## Reproducibility

* With `use_cache = TRUE` and `cache_dir = NULL`, retrieval now caches in a
  per-session directory under `tempdir()` (0.1.0 silently did not cache).
* Retrieved objects carry a `provenance` attribute (request, retrieval time,
  API, package version).
* New opt-in live integration test (`INEDEMOGR_LIVE_TESTS=true`) and weekly
  workflow; `reproducibility/` holds frozen inputs, the validation study
  against INE's provincial life tables and fertility indicators, and the
  manuscript-number scripts.

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
