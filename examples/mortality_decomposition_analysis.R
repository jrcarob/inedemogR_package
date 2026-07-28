# Test decompose_life_expectancy(): retrieve data with the package's
# built-in functions, build the full mortality pipeline once, then
# decompose two kinds of e0 gap into age-specific contributions -
#   (a) a cross-sectional gap: two provinces, the same year;
#   (b) a temporal gap: one province, two different years -
# comparing Arriaga's (1984, exact) and Pollard's (1988, approximate)
# methods, and charting each decomposition as an age-contribution bar
# chart (the standard way to visualize these results: which ages helped,
# which hurt, and by how much).

library(inedemogR)
library(dplyr)
library(ggplot2)

# 1. Retrieve and compute the full mortality pipeline (built-in package
#    functions), once, for every province and year.
pop <- get_ine_population()
deaths <- get_ine_deaths()
exposure <- compute_exposure(pop$data, deaths$data_provinces)
rates <- compute_death_rates(deaths$data_provinces, exposure$data)
lt <- build_life_tables(rates$mx_1x1)

# 2. Cross-sectional comparison: two provinces, the latest year
#    available - edit these two names for any other pair.
sex <- "female"
latest_year <- max(lt$fltper$year)

province_a <- "A Coruna"
province_b <- "Madrid"

lt_a <- lt$fltper |> filter(province_name == province_a, year == latest_year)
lt_b <- lt$fltper |> filter(province_name == province_b, year == latest_year)

decomp_cross_arriaga <- decompose_life_expectancy(lt_a, lt_b, method = "arriaga")
decomp_cross_pollard <- decompose_life_expectancy(lt_a, lt_b, method = "pollard")

cat(
  province_a, latest_year, "e0:", lt_a$ex[lt_a$age == 0], "\n",
  province_b, latest_year, "e0:", lt_b$ex[lt_b$age == 0], "\n",
  "Gap (Arriaga, sums exactly):", sum(decomp_cross_arriaga$contribution), "\n",
  "Gap (Pollard, close approximation):", sum(decomp_cross_pollard$contribution), "\n"
)

# 3. Temporal comparison: one province, first vs. latest year available.
earliest_year <- min(lt$fltper$year)

lt_early <- lt$fltper |> filter(province_name == province_a, year == earliest_year)
lt_late <- lt$fltper |> filter(province_name == province_a, year == latest_year)

decomp_time_arriaga <- decompose_life_expectancy(lt_early, lt_late, method = "arriaga")

cat(
  province_a, earliest_year, "e0:", lt_early$ex[lt_early$age == 0], "\n",
  province_a, latest_year, "e0:", lt_late$ex[lt_late$age == 0], "\n",
  "Gap (Arriaga, sums exactly):", sum(decomp_time_arriaga$contribution), "\n"
)

# 4. Chart 1: cross-sectional age-contribution bars (Arriaga) - which
#    ages explain the e0 gap between the two provinces, and in which
#    direction (positive: that age favors province_b; negative: favors
#    province_a).
ggplot(decomp_cross_arriaga, aes(x = age, y = contribution, fill = contribution > 0)) +
  geom_col(width = 1) +
  scale_fill_manual(
    values = c(`TRUE` = "steelblue", `FALSE` = "firebrick"), guide = "none"
  ) +
  labs(
    x = "Age", y = "Contribution to e0 gap (years)",
    title = paste0(
      province_b, " vs. ", province_a, " life expectancy gap by age, ",
      latest_year, " (", sex, ", Arriaga decomposition)"
    ),
    subtitle = paste0(
      "Blue: favors ", province_b, ". Red: favors ", province_a,
      ". Bars sum to the total gap (",
      round(sum(decomp_cross_arriaga$contribution), 2), " years)."
    )
  ) +
  theme_minimal()

# 5. Chart 2: the same cross-sectional gap, Arriaga vs. Pollard side by
#    side - the two methods' age patterns should track closely (per
#    Ponnapalli 2005) without being identical, since Pollard's is only
#    exact in the continuous limit.
comparison <- bind_rows(
  decomp_cross_arriaga |> mutate(method = "Arriaga"),
  decomp_cross_pollard |> mutate(method = "Pollard")
)

ggplot(comparison, aes(x = age, y = contribution, color = method)) +
  geom_line(linewidth = 0.7) +
  labs(
    x = "Age", y = "Contribution to e0 gap (years)", color = "Method",
    title = paste0(
      "Arriaga vs. Pollard decomposition, ", province_b, " vs. ", province_a, ", ", latest_year
    )
  ) +
  theme_minimal()

# 6. Chart 3: the temporal age-contribution bars (Arriaga) - which ages
#    drove province_a's e0 change between the two years.
ggplot(decomp_time_arriaga, aes(x = age, y = contribution, fill = contribution > 0)) +
  geom_col(width = 1) +
  scale_fill_manual(
    values = c(`TRUE` = "steelblue", `FALSE` = "firebrick"), guide = "none"
  ) +
  labs(
    x = "Age", y = "Contribution to e0 gap (years)",
    title = paste0(
      province_a, " life expectancy change by age, ", earliest_year, " to ", latest_year,
      " (", sex, ", Arriaga decomposition)"
    ),
    subtitle = paste0(
      "Blue: gain. Red: loss. Bars sum to the total change (",
      round(sum(decomp_time_arriaga$contribution), 2), " years)."
    )
  ) +
  theme_minimal()
