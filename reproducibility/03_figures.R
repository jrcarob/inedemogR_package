# Figures of the SoftwareX article, regenerated from the frozen inputs in
# reproducibility/data with explicit years (no max(year)), so they do not
# change when INE publishes new data.
#
#   Rscript reproducibility/03_figures.R
#
# Writes reproducibility/figs/{fig_maps,fig_decomp,fig_lexis,fig_fertility,
# fig_pyramids}.png at 600 dpi.

devtools::load_all(quiet = TRUE)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(patchwork)
})
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
figs <- "reproducibility/figs"
dir.create(figs, showWarnings = FALSE)
sv <- function(p, name, w, h) ggsave(file.path(figs, name), p, width = w, height = h, dpi = 600, bg = "white")
d <- "reproducibility/data"

YEAR <- 2024
comp <- c("Madrid", "Barcelona", "Sevilla", "A Coruna")
pal <- c(Madrid = "#EE6677", Barcelona = "#4477AA", Sevilla = "#228833", `A Coruna` = "#CCBB44")
small_legend <- theme(legend.key.size = unit(3.2, "mm"), legend.text = element_text(size = 7),
                      legend.title = element_text(size = 7.5), plot.title = element_text(size = 10))

pop <- readRDS(file.path(d, "population_56945_100.rds"))$data
deaths <- readRDS(file.path(d, "deaths_6545_6547_100.rds"))$data_provinces
births_age <- readRDS(file.path(d, "births_by_age_6508_100.rds"))$data
vitals <- readRDS(file.path(d, "demog_totals_2023.rds"))

exposure <- quiet(compute_exposure(pop, deaths))
rates <- quiet(compute_death_rates(deaths, exposure$data))
lt <- quiet(build_life_tables(rates$mx_1x1))

# Fig. 1: (a) births per death 2023, (b) female e0 2024.
p_bdr <- map_indicator(birth_death_ratio(vitals), value_col = "birth_death_ratio",
  geo_level = "province", breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2, 3), palette = "PiYG",
  legend_title = "Births / death", title = "(a) Births per death, 2023") + small_legend
le_f <- life_expectancy_summary(lt$fltper, sex = "female")
p_le <- map_life_expectancy(le_f, year = YEAR, sex = "female",
  title = paste0("(b) Female e0, ", YEAR)) + small_legend
sv(p_bdr | p_le, "fig_maps.png", w = 8.6, h = 4.0)

# Fig. 3: Arriaga decomposition of the female e0 gap, A Coruna vs Madrid.
female <- lt$fltper # female tables only
lt_a <- filter(female, province_name == "A Coruna", year == YEAR)
lt_b <- filter(female, province_name == "Madrid", year == YEAR)
d_arr <- decompose_life_expectancy(lt_a, lt_b, method = "arriaga")
p_dec <- ggplot(d_arr, aes(age, contribution, fill = contribution > 0)) +
  geom_col(width = 1) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  scale_fill_manual(values = c(`TRUE` = "#4477AA", `FALSE` = "#CC3311"), guide = "none") +
  labs(x = "Age", y = "Contribution to female e0 gap (years)",
       title = paste0("Age decomposition of the female e0 gap: A Coruna vs. Madrid, ", YEAR),
       subtitle = "Arriaga's method; bars > 0: ages where Madrid's lower mortality widens its advantage") +
  theme_minimal(base_size = 11)
sv(p_dec, "fig_decomp.png", w = 7, h = 4.6)

# Fig. 4: Lexis surface, Madrid, both sexes, 2002-2024.
sv(plot_lexis_diagram(filter(rates$mx_1x1, year >= 2002), province = "Madrid", sex = "total",
                      title = "Lexis mortality surface, Madrid"),
   "fig_lexis.png", w = 7.2, h = 5.6)

# Fig. 5: fertility, (a) ASFR 2024 per 1,000 women, (b) TFR 2002-2024.
asfr <- quiet(age_specific_fertility_rate(births_age, pop))
tfr <- total_fertility_rate(asfr)
fscale <- scale_colour_manual(values = pal, name = NULL, limits = comp, breaks = comp)
p_asfr <- asfr |>
  filter(province_name %in% comp, year == YEAR) |>
  ggplot(aes(age, asfr_per_1000, colour = province_name)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.3) + fscale +
  labs(x = "Age of mother", y = "ASFR (births per 1,000 women)",
       title = paste0("(a) Age-specific fertility schedule, ", YEAR)) +
  theme_minimal(base_size = 10)
p_tfr <- tfr |>
  filter(province_name %in% comp, year >= 2002, year <= YEAR) |>
  ggplot(aes(year, tfr, colour = province_name)) +
  geom_hline(yintercept = 2.1, linetype = "dashed", colour = "grey45") +
  geom_line(linewidth = 0.8) + geom_point(size = 1.3) + fscale +
  labs(x = "Year", y = "Total fertility rate (children per woman)",
       title = "(b) TFR (dashed line: replacement, 2.1)") +
  theme_minimal(base_size = 10)
sv((p_asfr | p_tfr) + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
   "fig_fertility.png", w = 8.4, h = 4.2)

# Fig. 6: population pyramids, Andalusian provinces, 1 January 2024.
and <- pop |>
  filter(nuts2_code == "ES61", year == YEAR) |>
  select(province_name, age, female, male) |>
  pivot_longer(c(female, male), names_to = "sex", values_to = "n") |>
  mutate(n = if_else(sex == "male", -n, n))
p_pyr <- ggplot(and, aes(age, n, fill = sex)) +
  geom_col(width = 1) +
  coord_flip() +
  facet_wrap(~province_name, nrow = 2, scales = "free_x") +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  scale_fill_manual(values = c(female = "#EE6677", male = "#4477AA"), name = NULL) +
  labs(x = "Age", y = "Population",
       title = paste0("Population pyramids, Andalusian provinces, 1 January ", YEAR)) +
  theme_minimal(base_size = 9) + theme(legend.position = "bottom")
sv(p_pyr, "fig_pyramids.png", w = 8.4, h = 5.4)

# Numbers quoted in the prose about natural increase.
births <- readRDS(file.path(d, "births_6506_100.rds"))$data
rni <- rate_of_natural_increase(crude_birth_rate(births, pop), crude_death_rate(deaths, pop))
crossover <- rni |>
  filter(province_name %in% comp, year >= 2002, year <= YEAR) |>
  group_by(province_name) |>
  summarise(
    negative_every_year = all(rni < 0),
    last_year_positive = if (any(rni >= 0)) max(year[rni >= 0]) else NA,
    rni_2019 = rni[year == 2019], rni_2020 = rni[year == 2020], rni_2024 = rni[year == YEAR]
  )
print(as.data.frame(crossover))
cat("wrote:", paste(list.files(figs), collapse = ", "), "\n")
