# Population pyramids for the 8 Andalusian provinces, latest available
# year, laid out in a 2x4 grid.
#
# Data is retrieved with get_ine_population() (System B: province-level,
# age/sex-disaggregated population). Andalusia's 8 provinces (Almeria,
# Cadiz, Cordoba, Granada, Huelva, Jaen, Malaga, Sevilla) all share
# nuts2_code "ES61", so that's used to filter rather than matching
# province names by hand. The grid itself is built with plain ggplot2
# syntax (geom_col() + coord_flip() + facet_wrap()) rather than
# combining 8 separate plot_population_pyramid() calls, since
# facet_wrap() is the idiomatic ggplot2 way to lay out one pyramid per
# province in a single plot object.

library(inedemogR)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Retrieve province-level, age/sex-disaggregated population.
pop <- get_ine_population()

# 2. Filter to Andalusia's 8 provinces and the latest available year.
latest_year <- max(pop$data$year)

andalusia <- pop$data |>
  filter(nuts2_code == "ES61", year == latest_year)

# 3. Reshape to one row per province x age x sex, with male counts
#    negated for the mirrored pyramid layout.
pyramid_data <- andalusia |>
  select(province_name, age, female, male) |>
  pivot_longer(c(female, male), names_to = "sex", values_to = "count") |>
  mutate(count = if_else(sex == "male", -count, count))

# 4. Plot: one pyramid per province, 2 rows x 4 columns. free_x scales so
#    each province's pyramid fills its own panel (its shape is equally
#    readable regardless of population size), at the cost of not being
#    able to compare bar lengths directly across provinces.
# width = 1: geom_col()'s default (0.9x the age resolution) leaves a
# systematic 10% gap between each of the 101 age bars, which shows up as
# thin white stripes across the pyramid at typical render sizes.
ggplot(pyramid_data, aes(x = age, y = count, fill = sex)) +
  geom_col(width = 1) +
  coord_flip() +
  facet_wrap(~province_name, nrow = 2, ncol = 4, scales = "free_x") +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c(female = "#D4667A", male = "#4477AA")) +
  labs(
    x = "Age", y = "Population", fill = "Sex",
    title = paste("Population pyramids, Andalusian provinces,", latest_year)
  ) +
  theme_minimal()
