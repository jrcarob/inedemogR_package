#' @include helpers.R
NULL

#' Calculate and plot a Lexis diagram for one province
#'
#' Builds the classic demographic Lexis surface — age (y-axis) by
#' calendar year (x-axis), shaded by mortality rate, with birth-cohort
#' diagonals overlaid — for one province, from the age x year central
#' death rates already computed by [compute_death_rates()]. Operates on
#' data already retrieved/computed via the mortality pipeline
#' (`get_ine_deaths()` -> `compute_exposure()` -> `compute_death_rates()`);
#' it does not fetch anything itself.
#'
#' "Calculate" here means deriving the tidy age x year surface for the
#' requested province/sex and the cohort-diagonal positions to overlay
#' (age = year - cohort, for cohorts every `cohort_step` years across the
#' data's range) — not fitting a new mortality model; the rates
#' themselves come straight from [compute_death_rates()].
#'
#' @details
#' **The white diagonal lines are intentional, not an artifact.** They
#' are birth-cohort lines (age = year - cohort): every point along one
#' line represents the same group of people born in the same year, aging
#' one year for every calendar year that passes — the defining feature of
#' a Lexis diagram, letting you trace one cohort's mortality experience
#' across its lifetime instead of only reading across a fixed year or up
#' a fixed age. Set `cohort_lines = FALSE` to turn them off.
#'
#' **Grey cells mean no rate could be computed for that age x year
#' cell**, almost always because the source population data has no
#' single-year age breakdown for the oldest ages in the earliest years of
#' the series (so exposure, the rate's denominator, is missing there) —
#' not a bug in the calculation. This typically shows up as a rectangular
#' block in the upper-left (oldest ages, earliest years), clearing up
#' once the source data's age detail becomes complete. When present,
#' grey gets its own "Missing data" legend key (a continuous color scale
#' can't show this on its own, so it's added via a small dummy layer).
#'
#' @param mx_df Output of the `mx_1x1` element of [compute_death_rates()]
#'   (columns `nuts3_code`, `province_name`, `year`, `age`, `mx_female`,
#'   `mx_male`, `mx_total`).
#' @param province Character, a province name (regular expressions
#'   allowed, matched against `province_name` — same convention as
#'   `region` in [plot_population_pyramid()]). Must match exactly one
#'   province.
#' @param sex Character, one of `"total"`, `"female"`, `"male"`.
#' @param log_scale Logical (default `TRUE`), color-scale mortality rates
#'   on a log10 scale — the standard convention for Lexis surfaces, since
#'   mx spans several orders of magnitude across ages.
#' @param rate_per Numeric, express the mortality rate as deaths per this
#'   many people (default `1000`, the standard demographic convention —
#'   e.g. a legend value of `10` reads as "10 deaths per 1,000
#'   population"). Use `1` for the raw per-person rate.
#' @param cohort_lines Logical (default `TRUE`), overlay birth-cohort
#'   diagonals (age = year - cohort).
#' @param cohort_step Numeric, spacing in years between overlaid cohort
#'   diagonals (default `10`).
#' @param title Optional plot title (defaults to the matched province
#'   name).
#' @return A `ggplot` object.
#' @examples
#' mx <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna",
#'   year = rep(2020:2023, each = 3), age = rep(c(0, 50, 90), 4),
#'   mx_total = c(
#'     0.004, 0.003, 0.15, 0.0038, 0.0031, 0.14,
#'     0.0035, 0.0029, 0.16, 0.0033, 0.0028, 0.15
#'   )
#' )
#' plot_lexis_diagram(mx, province = "A Coruna")
#' @export
plot_lexis_diagram <- function(mx_df, province, sex = c("total", "female", "male"),
                                log_scale = TRUE, rate_per = 1000, cohort_lines = TRUE,
                                cohort_step = 10, title = NULL) {
  sex <- match.arg(sex)
  value_col <- paste0("mx_", sex)

  df <- dplyr::filter(mx_df, stringr::str_detect(.data$province_name, province))
  matched <- unique(df$province_name)
  if (length(matched) == 0) stop("`province` did not match any province name.")
  if (length(matched) > 1) {
    stop(
      "`province` matched more than one province (", paste(matched, collapse = ", "),
      "); narrow the pattern to match exactly one."
    )
  }

  df$plotted_rate <- df[[value_col]] * rate_per
  legend_name <- if (rate_per == 1) {
    "Mortality rate\n(deaths per person)"
  } else {
    sprintf("Mortality rate\n(deaths per %s)", format(rate_per, big.mark = ",", scientific = FALSE))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$age, fill = .data$plotted_rate)) +
    ggplot2::geom_tile()

  p <- if (log_scale) {
    p + ggplot2::scale_fill_viridis_c(
      name = paste0(legend_name, "\n(log scale)"), trans = "log10", na.value = "grey50",
      labels = scales::label_number(accuracy = 0.1)
    )
  } else {
    p + ggplot2::scale_fill_viridis_c(name = legend_name, na.value = "grey50")
  }

  # Continuous fill scales don't show a legend swatch for na.value on their
  # own, so add one via a dummy, invisible point layer with its own manual
  # (single-entry) discrete scale — a standard ggplot2 pattern for adding a
  # legend key without a second real dependency-requiring scale.
  if (anyNA(df$plotted_rate)) {
    p <- p +
      ggplot2::geom_point(
        data = data.frame(year = NA, age = NA, na_label = "Missing data"),
        ggplot2::aes(x = .data$year, y = .data$age, shape = .data$na_label),
        inherit.aes = FALSE, na.rm = TRUE
      ) +
      ggplot2::scale_shape_manual(
        name = NULL, values = c("Missing data" = 15),
        guide = ggplot2::guide_legend(override.aes = list(size = 5, colour = "grey50"))
      )
  }

  if (cohort_lines) {
    year_range <- range(df$year)
    age_range <- range(df$age)
    cohorts <- seq(
      from = floor((year_range[1] - age_range[2]) / cohort_step) * cohort_step,
      to = ceiling((year_range[2] - age_range[1]) / cohort_step) * cohort_step,
      by = cohort_step
    )
    p <- p + ggplot2::geom_abline(
      intercept = -cohorts, slope = 1, color = "white", linewidth = 0.3, alpha = 0.6
    )
  }

  p <- p +
    ggplot2::coord_cartesian(xlim = range(df$year), ylim = range(df$age), expand = FALSE) +
    ggplot2::labs(x = "Year", y = "Age", title = title %||% matched) +
    ggplot2::theme_minimal()

  p
}
