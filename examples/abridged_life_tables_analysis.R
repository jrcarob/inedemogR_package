# Test build_abridged_life_table()/build_abridged_life_tables(): retrieve
# data with the package's built-in functions, build the full mortality
# pipeline once, then construct abridged (5-year age group) life tables
# directly from mx_5x1 - the standalone capability that complements
# build_life_tables()'s single-year (1x1) tables (e.g. for comparing
# against other agencies' published abridged tables). Cross-checks the
# abridged e0 against the exact single-year e0 on the same data (the
# check that actually caught a real Lx-scaling bug during development),
# then charts the abridged survivorship (lx) and mortality (nmx) curves.

library(inedemogR)
library(dplyr)
library(ggplot2)

# 1. Retrieve and compute the full mortality pipeline (built-in package
#    functions): province-level population/deaths, exposure, then both
#    1x1 (single-year) and 5x1 (5-year age group) central death rates in
#    one call - compute_death_rates() always returns both.
pop <- get_ine_population()
deaths <- get_ine_deaths()
exposure <- compute_exposure(pop$data, deaths$data_provinces)
rates <- compute_death_rates(deaths$data_provinces, exposure$data)

# 2. Build both the single-year and abridged life tables from the same
#    rates, for every province and year. Some early years (1996-2001)
#    flag QC issues at the oldest age groups (NA Lx) - this traces to a
#    known INE data gap (single-year population/exposure detail for
#    ages 85-100 is missing that far back, already documented for
#    Madrid in the Lexis-diagram work), not a bug in this construction;
#    it clears up from 2002 onward.
lt_1x1 <- build_life_tables(rates$mx_1x1)
lt_abridged <- build_abridged_life_tables(rates$mx_1x1, rates$mx_5x1)

# 3. Cross-check: abridged e0 should closely match the exact single-year
#    e0 for the same province/year (they're built from the same
#    underlying mx data, just grouped differently).
province <- "A Coruna"
sex <- "female"
latest_year <- max(lt_abridged$fltper$year)

e0_1x1 <- lt_1x1$fltper |>
  filter(province_name == province, year == latest_year, age == 0) |>
  pull(ex)
e0_abridged <- lt_abridged$fltper |>
  filter(province_name == province, year == latest_year, age_group == "00-04") |>
  pull(ex)

cat(
  province, latest_year, "e0 (single-year):", e0_1x1, "\n",
  province, latest_year, "e0 (abridged):   ", e0_abridged, "\n",
  "Difference:", round(e0_abridged - e0_1x1, 3), "years\n"
)

# 4. Chart 1: survivorship curve (lx) - single-year vs. abridged, same
#    province/year. The abridged curve is a coarser (21-point) version
#    of the same underlying survivorship, so the two lines should track
#    closely with the abridged one just less granular.
lt_1x1_one <- lt_1x1$fltper |> filter(province_name == province, year == latest_year)
lt_abridged_one <- lt_abridged$fltper |> filter(province_name == province, year == latest_year)

survivorship <- bind_rows(
  lt_1x1_one |> transmute(age = age, lx = lx, table = "Single-year (1x1)"),
  lt_abridged_one |> transmute(age = age_start, lx = lx, table = "Abridged (5x1)")
)

ggplot(survivorship, aes(x = age, y = lx, color = table)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = filter(survivorship, table == "Abridged (5x1)"), size = 1.8) +
  labs(
    x = "Age", y = "Survivors (out of 100,000 births)", color = NULL,
    title = paste0("Survivorship curve, ", province, ", ", latest_year, " (", sex, ")"),
    subtitle = "Abridged points mark the start of each 5-year age group"
  ) +
  theme_minimal()

# 5. Chart 2: the abridged mortality schedule (nmx) on a log scale - the
#    classic age pattern of mortality (high in infancy, a dip through
#    childhood/young adulthood, then a roughly exponential rise with
#    age), for a comparison set of provinces.
provinces_to_compare <- c("Madrid", "Barcelona", "Sevilla", "A Coruna")

mx_compare <- lt_abridged$fltper |>
  filter(province_name %in% provinces_to_compare, year == latest_year, mx > 0)

ggplot(mx_compare, aes(x = age_start, y = mx, color = province_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_y_log10() +
  labs(
    x = "Age group (start)", y = "Central death rate (log scale)", color = "Province",
    title = paste0("Abridged mortality schedule, ", latest_year, " (", sex, ")")
  ) +
  theme_minimal()
