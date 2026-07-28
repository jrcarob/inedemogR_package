# Lexis diagrams (age x year mortality surface, with birth-cohort
# diagonals) for Madrid and Barcelona, side by side.
#
# plot_lexis_diagram() itself only handles one province at a time (it
# errors if `province` matches more than one, since combining provinces
# into a single call would silently average/mix their surfaces). To show
# two provinces together, this script instead retrieves the data with
# the package's built-in pipeline functions and builds the two-panel
# figure directly with plain ggplot2 syntax (facet_wrap()), following
# the same pattern as examples/andalusia_pyramids.R.
#
# Two things about the rendered plot that are easy to mistake for bugs:
#   - The white diagonal lines are intentional birth-cohort lines (age =
#     year - cohort) - the defining feature of a Lexis diagram, tracing
#     one birth cohort's mortality experience across its lifetime. Not
#     an artifact; remove the geom_abline() layer below to turn them off.
#   - The grey block in the upper-left (oldest ages, earliest years) is
#     missing data, not a rendering glitch: the source population data
#     has no single-year age breakdown for the oldest ages that far back
#     in the series, so exposure (the rate's denominator) is missing
#     there and no rate can be computed. It clears up once the source
#     data's age detail becomes complete (confirmed for Madrid: NA for
#     ages 85-100 in 1996-2001, resolved from 2002 onward).

library(inedemogR)
library(dplyr)
library(ggplot2)

# 1. Retrieve and compute the full mortality pipeline with the package's
#    built-in functions: province-level population and deaths, then
#    exposure-to-risk, then central death rates (mx).
pop <- get_ine_population()
deaths <- get_ine_deaths()
exposure <- compute_exposure(pop$data, deaths$data_provinces)
rates <- compute_death_rates(deaths$data_provinces, exposure$data)

# 2. Filter mx_1x1 to Madrid and Barcelona.
provinces <- c("Madrid", "Barcelona")
mx <- rates$mx_1x1 |>
  filter(province_name %in% provinces)

# 3. Cohort diagonals (age = year - cohort), shared across both panels
#    since both provinces cover the same year/age range.
cohort_step <- 10
cohorts <- seq(
  from = floor((min(mx$year) - max(mx$age)) / cohort_step) * cohort_step,
  to = ceiling((max(mx$year) - min(mx$age)) / cohort_step) * cohort_step,
  by = cohort_step
)

# 4. Plot: one Lexis surface per province, side by side. Rates are
#    expressed per 1,000 (the standard demographic convention) with
#    plain-number legend labels, rather than the raw per-person rate in
#    scientific notation (e.g. "1e-02"), which is harder to read.
mx <- mx |> mutate(mx_per_1000 = mx_total * 1000)

ggplot(mx, aes(x = year, y = age, fill = mx_per_1000)) +
  geom_tile() +
  geom_abline(
    intercept = -cohorts, slope = 1,
    color = "white", linewidth = 0.3, alpha = 0.6
  ) +
  scale_fill_viridis_c(
    name = "Mortality rate\n(deaths per 1,000, log scale)",
    trans = "log10", na.value = "grey50",
    labels = scales::label_number(accuracy = 0.1)
  ) +
  # A continuous fill scale can't show an na.value swatch in its legend on
  # its own, so add one via a dummy, invisible point layer with its own
  # manual (single-entry) discrete scale - a standard ggplot2 pattern.
  geom_point(
    data = data.frame(year = NA, age = NA, na_label = "Missing data"),
    aes(x = year, y = age, shape = na_label), inherit.aes = FALSE, na.rm = TRUE
  ) +
  scale_shape_manual(
    name = NULL, values = c("Missing data" = 15),
    guide = guide_legend(override.aes = list(size = 5, colour = "grey50"))
  ) +
  coord_cartesian(xlim = range(mx$year), ylim = range(mx$age), expand = FALSE) +
  facet_wrap(~province_name, nrow = 1, ncol = 2) +
  labs(x = "Year", y = "Age", title = "Lexis diagrams: Madrid and Barcelona") +
  theme_minimal()
