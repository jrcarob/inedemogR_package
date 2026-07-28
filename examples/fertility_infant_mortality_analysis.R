# Test general_fertility_rate() and infant_mortality_rate(): retrieve
# data with the package's built-in functions, compute GFR and IMR for
# every province and year, and visualize both trends for a comparison
# set of provinces over the whole period available.

library(inedemogR)
library(dplyr)
library(ggplot2)

# 1. Retrieve births, deaths, population (built-in package functions).
births <- get_ine_births()
deaths <- get_ine_deaths()
pop <- get_ine_population()

# 2. Compute GFR and IMR for every province and year, the whole period
#    the data covers.
gfr <- general_fertility_rate(births$data, pop$data)
imr <- infant_mortality_rate(deaths$data_provinces, births$data)

# 3. Compare a set of provinces (edit this vector for any other set).
provinces_to_compare <- c("Madrid", "Barcelona", "Sevilla", "A Coruna")

# 4. Chart 1: GFR over time - fertility's long secular decline, visible
#    across every province regardless of its age structure (unlike
#    crude_birth_rate(), GFR already accounts for how many
#    reproductive-age women each province actually has).
ggplot(
  gfr |> filter(province_name %in% provinces_to_compare),
  aes(x = year, y = gfr, color = province_name)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    x = "Year", y = "General fertility rate (births per 1,000 women 15-49)",
    color = "Province", title = "General fertility rate over time"
  ) +
  theme_minimal()

# 5. Chart 2: IMR over time - the epidemiological transition, visible as
#    infant mortality falling from several deaths per 1,000 live births
#    in the late 1990s to close to zero today. Year-to-year noise is
#    expected: infant deaths are rare events, so small provinces can
#    show large swings from just one or two extra cases.
ggplot(
  imr |> filter(province_name %in% provinces_to_compare),
  aes(x = year, y = imr, color = province_name)
) +
  geom_line(linewidth = 0.6, alpha = 0.6) +
  geom_smooth(se = FALSE, span = 0.4) +
  labs(
    x = "Year", y = "Infant mortality rate (deaths per 1,000 live births)",
    color = "Province",
    title = "Infant mortality rate over time",
    subtitle = "Thin lines: raw yearly values. Smoothed curves: underlying trend."
  ) +
  theme_minimal()
