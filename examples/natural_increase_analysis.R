# Test crude_death_rate() and rate_of_natural_increase(): retrieve data
# with the package's built-in functions, compute CBR/CDR/RNI for every
# province and year, and visualize the "demographic crossover" - where a
# province's death rate overtakes its birth rate, meaning it's losing
# population to natural change alone, before migration is even
# considered - plus the resulting rate of natural increase over time.

library(inedemogR)
library(dplyr)
library(ggplot2)

# 1. Retrieve births, deaths, population (built-in package functions).
births <- get_ine_births()
deaths <- get_ine_deaths()
pop <- get_ine_population()

# 2. Compute crude rates and RNI for every province and year, the whole
#    period the data covers.
cbr <- crude_birth_rate(births$data, pop$data)
cdr <- crude_death_rate(deaths$data_provinces, pop$data)
rni <- rate_of_natural_increase(cbr, cdr)

# 3. Compare a set of provinces (edit this vector for any other set).
provinces_to_compare <- c("Madrid", "Barcelona", "Sevilla", "A Coruna")

# 4. Chart 1: the "demographic crossover" plot - birth rate and death
#    rate over time, one panel per province. Where the death-rate line
#    rises above the birth-rate line, the gap between them is exactly
#    what rate_of_natural_increase() reports as negative.
rates_long <- bind_rows(
  cbr |>
    filter(province_name %in% provinces_to_compare) |>
    transmute(province_name, year, rate = cbr, type = "Birth rate (CBR)"),
  cdr |>
    filter(province_name %in% provinces_to_compare) |>
    transmute(province_name, year, rate = cdr, type = "Death rate (CDR)")
)

ggplot(rates_long, aes(x = year, y = rate, color = type)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~province_name, nrow = 2, ncol = 2) +
  scale_color_manual(values = c(
    "Birth rate (CBR)" = "#4477AA", "Death rate (CDR)" = "#CC6677"
  )) +
  labs(
    x = "Year", y = "Rate per 1,000 population", color = NULL,
    title = "Demographic crossover: birth vs. death rate by province"
  ) +
  theme_minimal()

# 5. Chart 2: rate of natural increase, all four provinces on one chart -
#    negative means the province is shrinking from natural change alone
#    (the dashed line at 0 marks the crossover point from chart 1).
ggplot(
  rni |> filter(province_name %in% provinces_to_compare),
  aes(x = year, y = rni, color = province_name)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    x = "Year", y = "Rate of natural increase (per 1,000)", color = "Province",
    title = "Rate of natural increase over time"
  ) +
  theme_minimal()
