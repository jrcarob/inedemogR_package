# Birth-to-death ratio by Spanish province, replicating the style of a
# reference choropleth (binned diverging color scale: pink = more deaths
# than births, green = more births than deaths).
#
# Municipality-level birth/death counts are NOT available: INE's births
# (MNPN) and deaths (MNPD) statistical operations only publish down to
# province level in their standard tables (verified live against INE's
# metadata catalogue via ineapir::get_metadata_tables_operation() - no
# municipality-level table exists for either operation). Population
# *stock* is available at municipality level, which is why
# population_total is wired at that granularity in this package's
# registry, but births/deaths are not. This script therefore reproduces
# the reference plot's *style* (binned pink-to-green choropleth of a
# births:deaths ratio) at the finest level Spain's public data actually
# supports: province.

library(inedemogR)

# 1. Fetch both indicators for the latest year in one call - get_ine_demog()
#    returns one row per province x year with both totals as columns.
vitals <- get_ine_demog(
  indicator = c("births_total", "deaths_total"),
  year = 2023
)

# 2. Compute the ratio (births per death; > 1 means more births than deaths).
#    `vitals` includes two rows besides the 52 provinces - GEOID "00"
#    ("Total Nacional") and GEOID "" ("No residente", i.e. registered
#    abroad) - both standard INE aggregate rows. Harmless here: neither
#    matches any province geometry, so map_indicator() silently excludes
#    them from the map (no warning, since the warning only fires for
#    provinces missing a *value*, not for extra rows with no geometry).
ratio <- birth_death_ratio(vitals)

# 3. Map it: a binned, diverging pink-to-green scale, matching the
#    reference image's style and interpretation (green = birth surplus,
#    pink/magenta = death surplus). Breaks chosen to bracket 1:1 the way
#    the reference plot does.
map_indicator(
  ratio,
  value_col = "birth_death_ratio",
  geo_level = "province",
  breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2, 3),
  palette = "PiYG",
  legend_title = "Births per death",
  title = "Birth-to-death ratio by province, Spain (2023)"
)
