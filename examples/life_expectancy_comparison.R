# Life expectancy at birth, compared across provinces, for the whole
# period available - a time-series chart plus a national map, both
# built with ggplot2 syntax.
#
# Life tables are computed once for every province (build_life_tables()
# processes all of them together anyway), then life_expectancy_summary()
# extracts e0/e65 for all of them. From there:
#   - the time-series chart filters to whichever provinces you want to
#     compare (edit `provinces_to_compare` below - works for any subset,
#     not just the four picked here) and draws one line per province;
#   - the map shows every province at once, for one chosen year, so any
#     region can be visually compared against the rest of Spain without
#     needing to pick a subset.

library(inedemogR)
library(dplyr)
library(ggplot2)

# 1. Retrieve and compute the full mortality pipeline (built-in package
#    functions): province-level population/deaths, exposure, central
#    death rates, then period life tables for every province and year.
pop <- get_ine_population()
deaths <- get_ine_deaths()
exposure <- compute_exposure(pop$data, deaths$data_provinces)
rates <- compute_death_rates(deaths$data_provinces, exposure$data)
lt <- build_life_tables(rates$mx_1x1)

# 2. Extract e0/e65 for every province and year (the whole period the
#    data covers).
sex <- "female"
le <- life_expectancy_summary(lt$fltper, sex = sex)

# 3. Time-series comparison: edit this vector to compare any set of
#    provinces (exact province_name values; see list_ine_indicators()'s
#    sibling helpers or province_lookup for the full list of names).
provinces_to_compare <- c("Madrid", "Barcelona", "Sevilla", "A Coruna")

le_compare <- le |> filter(province_name %in% provinces_to_compare)

ggplot(le_compare, aes(x = year, y = e0, color = province_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    x = "Year", y = "Life expectancy at birth (e0)", color = "Province",
    title = paste0(
      "Life expectancy at birth over time (", sex, "), ",
      min(le_compare$year), "-", max(le_compare$year)
    )
  ) +
  theme_minimal()

# 4. Map comparison: every province, one year (the latest complete year
#    in the data), so any region's position relative to the rest of
#    Spain is visible at a glance. map_life_expectancy() bridges
#    life_expectancy_summary()'s province-level output onto
#    get_ine_geo()'s geometry.
latest_year <- max(le$year)

map_life_expectancy(
  le, year = latest_year, sex = sex,
  title = paste0("Life expectancy at birth by province, Spain, ", latest_year, " (", sex, ")")
)
