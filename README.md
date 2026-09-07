
# inedemogR <img align="left" width="15%" src="sticker/inedemogR_sticker.png"> 

<!-- badges: start -->
<!-- badges: end -->

inedemogR provides tidy access to demographic data from the Spanish
National Statistics Institute (INE), specifically its "fenómenos
demográficos" domain: population, births, and deaths. Data is retrieved
live via the official [ineapir](https://github.com/es-ine/ineapir) API
wrapper and tidied into long/wide data frames, with optional spatial
integration via [mapSpain](https://ropenspain.github.io/mapSpain/) and `sf`.

## Installation

You can install from [CRAN](https://cran.r-project.org/web/packages/inedemogR/index.html)

Alternatively, you can install the development version of inedemogR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
pak::pak('jrcarob/inedemogR_package')
or 
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
