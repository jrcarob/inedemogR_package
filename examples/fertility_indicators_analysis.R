# Test age_specific_fertility_rate()/total_fertility_rate()/
# mean_age_at_childbearing()/gross_reproduction_rate()/
# net_reproduction_rate(): retrieve data with the package's built-in
# functions, compute the full ASFR-based fertility schedule for every
# province and year, and visualize the age schedule, TFR/MAC trends, and
# the GRR-vs-NRR mortality-adjustment gap for a comparison set of
# provinces over the whole period available.

library(inedemogR)
library(dplyr)
library(ggplot2)

# 1. Retrieve age-of-mother births, population, and deaths (built-in
#    package functions). Age-of-mother births is the one retrieval this
#    fertility schedule needs beyond what general_fertility_rate()/
#    crude_birth_rate() already used - see get_ine_births_by_age().
births_age <- get_ine_births_by_age()
pop <- get_ine_population()
deaths <- get_ine_deaths()

# 2. ASFR schedule, then the indicators built on it.
asfr <- age_specific_fertility_rate(births_age$data, pop$data)
tfr <- total_fertility_rate(asfr)
mac <- mean_age_at_childbearing(asfr)
grr <- gross_reproduction_rate(asfr)

# 3. NRR additionally needs the female life table (Lx), so run the
#    mortality pipeline (same steps as life_expectancy_comparison.R).
exposure <- compute_exposure(pop$data, deaths$data_provinces)
rates <- compute_death_rates(deaths$data_provinces, exposure$data)
lt <- build_life_tables(rates$mx_1x1)
nrr <- net_reproduction_rate(asfr, lt$fltper)

# 4. Compare a set of provinces (edit this vector for any other set).
provinces_to_compare <- c("Madrid", "Barcelona", "Sevilla", "A Coruna")

# 5. Chart 1: the ASFR age schedule itself, latest year available - the
#    classic single-peaked fertility curve, shifted right (peak in the
#    30s, not the 20s) as expected for Spain's late childbearing pattern.
latest_year <- max(asfr$year)
ggplot(
  asfr |> filter(province_name %in% provinces_to_compare, year == latest_year),
  aes(x = age, y = asfr, color = province_name)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  labs(
    x = "Age of mother", y = "ASFR (births per 1,000 women of that age)",
    color = "Province",
    title = paste("Age-specific fertility rate schedule,", latest_year)
  ) +
  theme_minimal()

# 6. Chart 2: TFR over time - Spain's well-documented below-replacement
#    fertility (TFR well under 2.1 throughout), with A Coruna
#    consistently lower, matching its already-established aging profile.
ggplot(
  tfr |> filter(province_name %in% provinces_to_compare),
  aes(x = year, y = tfr, color = province_name)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 2.1, linetype = "dashed", color = "grey40") +
  labs(
    x = "Year", y = "Total fertility rate (children per woman)",
    color = "Province", title = "Total fertility rate over time",
    subtitle = "Dashed line: replacement-level TFR (2.1)"
  ) +
  theme_minimal()

# 7. Chart 3: mean age at childbearing over time - the secular rise in
#    childbearing age, a well-documented feature of Spanish fertility.
ggplot(
  mac |> filter(province_name %in% provinces_to_compare),
  aes(x = year, y = mac, color = province_name)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    x = "Year", y = "Mean age at childbearing",
    color = "Province", title = "Mean age at childbearing over time"
  ) +
  theme_minimal()

# 8. Chart 4: GRR vs NRR, latest year - the mortality-adjustment gap.
#    NRR is always <= GRR; the gap is the (now very small, in a
#    low-mortality country like Spain) share of women who would not
#    survive to complete their reproductive years.
comparison <- bind_rows(
  grr |> filter(province_name %in% provinces_to_compare, year == latest_year) |>
    transmute(province_name, rate = grr, measure = "GRR"),
  nrr |> filter(province_name %in% provinces_to_compare, year == latest_year) |>
    transmute(province_name, rate = nrr, measure = "NRR")
)

ggplot(comparison, aes(x = province_name, y = rate, fill = measure)) +
  geom_col(position = "dodge") +
  labs(
    x = "Province", y = "Daughters per woman",
    fill = NULL,
    title = paste("Gross vs. net reproduction rate,", latest_year),
    subtitle = "NRR accounts for mothers' own survival through reproductive age"
  ) +
  theme_minimal()
