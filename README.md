
# inedemogR <img align="left" width="15%" src="sticker/inedemogR_sticker.png">

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/inedemogR)](https://CRAN.R-project.org/package=inedemogR)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/inedemogR)](https://CRAN.R-project.org/package=inedemogR)
<!-- badges: end -->

inedemogR provides tidy access to demographic data from the Spanish
National Statistics Institute (INE), specifically its "fenómenos
demográficos" domain: population, births, and deaths. Data is retrieved
live via the official [ineapir](https://github.com/es-ine/ineapir) API
wrapper and tidied into long/wide data frames, with optional spatial
integration via [mapSpain](https://ropenspain.github.io/mapSpain/) and `sf`.

## Installation

Install the released version from CRAN with:

``` r
install.packages("inedemogR")
```

Version 0.2.0 corrects numerical errors in 0.1.0 (the Andreev-Kingkade
a0, old-age smoothing, the ESP 2013 weights, abridged tables, the Pollard
decomposition, fertility conventions and missing-data handling); see
[NEWS.md](NEWS.md). Results computed with 0.1.0 should be recomputed with
0.2.0 or later.

You can install the development version of inedemogR from
[GitHub](https://github.com/jrcarob/inedemogR_package) with:

``` r
# install.packages("pak")
pak::pak("jrcarob/inedemogR_package")
```

or

``` r
# install.packages("devtools")
devtools::install_github("jrcarob/inedemogR_package")
```

## Example

``` r
library(inedemogR)

# List available indicators and the INE tables they're wired to
list_ine_indicators()

# Fetch population data for municipalities in 2023 (real INE data)
pop_data <- get_ine_demog(indicator = "population_total", year = 2023)

# Fetch births and deaths by province in the same call
vital_stats <- get_ine_demog(
  indicator = c("births_total", "deaths_total"),
  year = 2023
)

# Fetch the same population data with geometries attached
pop_sf <- get_ine_demog(
  indicator = "population_total",
  year = 2023,
  region = "^Sevilla$",
  geometry = TRUE
)
```

Indicators are only available at the geographic level their source INE
table is actually published at: `population_total` is municipality-level,
`births_total`/`deaths_total` are province-level. Requesting indicators
that span different levels in one call raises an error rather than
silently mixing granularities — see `list_ine_indicators()`.

## Two workflows

inedemogR supports two deliberately parallel, complementary workflows —
neither subsumes the other, since they operate at different geographic
and demographic granularities.

**Workflow (a): export and work with files.** `download_ine_data()` runs
the province-level SHMD mortality pipeline (births, deaths, population,
exposure-to-risk, central death rates, period life tables, per HMD
Methods Protocol V6) and writes the results to a folder as CSV and/or
HMD-format `.txt` files, for use outside R:

``` r
download_ine_data("ine_data")
```

**Workflow (b): stay in R.** For quick multi-geo-level choropleths of
total counts, use `get_ine_demog()`/`get_ine_geo()`/`plot_ine_map()` (see
the example above). For age-structured demographic analysis — dependency
ratios, aging index, sex ratio, population pyramids, life expectancy —
use the province-level, age/sex-disaggregated functions behind
`download_ine_data()` directly:

``` r
pop <- get_ine_population()

# Summary indicators (one row per province x year)
age_dependency_ratio(pop$data)
aging_index(pop$data)
sex_ratio(pop$data)

# Charts
plot_population_pyramid(pop$data, year = max(pop$data$year), region = "A Coruna")

# Life expectancy: full pipeline through life tables, then map it,
# bridging this province-level analysis back onto get_ine_geo()'s
# spatial layer
deaths <- get_ine_deaths()
exposure <- compute_exposure(pop$data, deaths$data_provinces)
rates <- compute_death_rates(deaths$data_provinces, exposure$data)
lt <- build_life_tables(rates$mx_1x1)
le <- life_expectancy_summary(lt$fltper, sex = "female")
map_life_expectancy(le, year = max(le$year), sex = "female")
```

Note `crude_birth_rate()` computes a *crude* birth rate
(births/population), not a total fertility rate. For a true TFR, use
`get_ine_births_by_age()` (age-of-mother birth counts) with
`age_specific_fertility_rate()`/`total_fertility_rate()`:

``` r
births_age <- get_ine_births_by_age()
asfr <- age_specific_fertility_rate(births_age$data, pop$data)
total_fertility_rate(asfr)
```

## Status

inedemogR is under active development. System A (`get_ine_demog()`,
`get_ine_geo()`, `list_ine_indicators()`, `plot_ine_map()`) implements
live retrieval and mapping of municipality/province-level total counts.
Migration indicators are not included in this release.
System B (`get_ine_births()`/`get_ine_deaths()`/`get_ine_population()`,
`compute_exposure()`, `compute_death_rates()`, `build_life_tables()`,
`download_ine_data()`) implements a province-level, age/sex-disaggregated
SHMD mortality pipeline, with summary indicators
(`age_dependency_ratio()`, `aging_index()`, `sex_ratio()`,
`crude_birth_rate()`, `life_expectancy_summary()`) and charts
(`plot_population_pyramid()`, `plot_demog_trend()`,
`map_life_expectancy()`) built on top. A comprehensive tutorial covering
every function, the mortality-pipeline mathematics, and full worked
examples is available via `vignette("inedemogR-tutorial")`.
Cleaning/harmonization helpers and projections are planned.

## Validation and reproducibility

The `reproducibility/` folder of the GitHub repository (it is not part of
the CRAN package) contains the INE inputs used in the SoftwareX article,
frozen on 5 October 2026 with their provenance, INE's published reference
series, and the scripts that regenerate every reported number, table and
figure offline. From a clone of the repository:

``` sh
Rscript reproducibility/01_validation.R        # benchmark vs INE, sensitivity, bootstrap
Rscript reproducibility/02_manuscript_numbers.R  # every number in the article
Rscript reproducibility/03_figures.R             # the article's figures
Rscript reproducibility/04_rate_benchmark.R      # grouped death rates vs INE
Rscript reproducibility/05_fertility_benchmark.R # TFR and mean age at childbearing vs INE
```

Across 2002-2024, provincial e0 and e65 agree with INE's published tables
within 0.25 years for every province whose INE table closes at 95+ (see
`reproducibility/output/validation_summary.md` in the repository), and
provincial TFR and
mean age at childbearing agree with INE's published series within 0.01 in
99% and 98% of province-years, 1975-2024. Life tables are built
from 2002 onwards; INE's earlier provincial populations are top-coded at
85+.

Tests: `devtools::test()` runs the fixed-input suite. The full pipeline
on live INE data runs when the environment variable
`INEDEMOGR_LIVE_TESTS=true` is set (weekly in the repository's continuous
integration).

## Citation

If you use inedemogR in your work, please cite it. Run
`citation("inedemogR")` for the current entry, or use:

> Caro-Barrera, J. R. (2026). *inedemogR: Tidy Access to Spanish INE
> Demographic Data*. R package version 0.2.0.
> <https://CRAN.R-project.org/package=inedemogR>

``` bibtex
@Manual{inedemogR,
  title  = {inedemogR: Tidy Access to Spanish INE Demographic Data},
  author = {J. R. Caro-Barrera},
  year   = {2026},
  note   = {R package version 0.2.0},
  url    = {https://CRAN.R-project.org/package=inedemogR},
}
```
