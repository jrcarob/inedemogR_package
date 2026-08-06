#' @include get_ine_geo.R helpers.R
NULL

#' Plot a population pyramid
#'
#' Draws a population pyramid for one province and year from
#' age-disaggregated population data. Requires an age/sex breakdown, so it
#' operates on [get_ine_population()]'s output, not [get_ine_demog()]'s
#' `population_total` indicator.
#'
#' @param pop_df Population tibble as returned by `get_ine_population()$data`
#'   (columns `province_name`, `year`, `age`, `female`, `male`).
#' @param year Numeric, the year to plot.
#' @param region Optional character vector to subset by province name
#'   (regular expressions allowed, matched against `province_name`). If
#'   more than one province matches, counts are summed across them. If
#'   `NULL` (default), all provinces are summed (national pyramid).
#' @param title Optional plot title.
#' @return A `ggplot` object.
#' @examples
#' pop <- tibble::tibble(
#'   province_name = "A Coruna", year = 2023,
#'   age = c(0, 1), female = c(100, 90), male = c(105, 95)
#' )
#' plot_population_pyramid(pop, year = 2023)
#' @export
plot_population_pyramid <- function(pop_df, year, region = NULL, title = NULL) {
  df <- dplyr::filter(pop_df, .data$year == !!year)

  if (!is.null(region)) {
    pattern <- paste(region, collapse = "|")
    df <- dplyr::filter(df, stringr::str_detect(.data$province_name, pattern))
    if (nrow(df) == 0) warning("`region` did not match any province name.")
  }

  df <- df |>
    dplyr::group_by(.data$age) |>
    dplyr::summarise(female = sum(.data$female), male = sum(.data$male), .groups = "drop") |>
    tidyr::pivot_longer(c("female", "male"), names_to = "sex", values_to = "count") |>
    dplyr::mutate(count = dplyr::if_else(.data$sex == "male", -.data$count, .data$count))

  # width = 1: geom_col()'s default (0.9x the age resolution) leaves a
  # systematic 10% gap between each single-year-of-age bar, which shows
  # up as thin white stripes across the pyramid at typical render sizes.
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$age, y = .data$count, fill = .data$sex)) +
    ggplot2::geom_col(width = 1) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = abs) +
    ggplot2::scale_fill_manual(values = c(female = "#D4667A", male = "#4477AA")) +
    ggplot2::labs(x = "Age", y = "Population", fill = "Sex") +
    ggplot2::theme_minimal()

  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)

  p
}

#' Plot a demographic indicator trend over time
#'
#' Generic time-series line chart for any indicator tibble keyed by
#' `nuts3_code`/`province_name`/`year`, such as the output of
#' [age_dependency_ratio()], [aging_index()], [sex_ratio()],
#' [crude_birth_rate()], or [life_expectancy_summary()].
#'
#' @param df A tibble with `province_name`, `year`, and `value_col`.
#' @param value_col Character, the name of the column to plot.
#' @param region Optional character vector to subset by province name
#'   (regular expressions allowed, matched against `province_name`); one
#'   line is drawn per matched province. If `NULL` (default), every
#'   province present in `df` is plotted.
#' @param title Optional plot title.
#' @param ylab Optional y-axis label (defaults to `value_col`).
#' @return A `ggplot` object.
#' @examples
#' trend <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna",
#'   year = c(2022, 2023), aging_index = c(150, 160)
#' )
#' plot_demog_trend(trend, "aging_index")
#' @export
plot_demog_trend <- function(df, value_col, region = NULL, title = NULL, ylab = NULL) {
  if (!is.null(region)) {
    pattern <- paste(region, collapse = "|")
    df <- dplyr::filter(df, stringr::str_detect(.data$province_name, pattern))
    if (nrow(df) == 0) warning("`region` did not match any province name.")
  }

  p <- ggplot2::ggplot(
    df, ggplot2::aes(x = .data$year, y = .data[[value_col]], color = .data$province_name)
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Year", y = ylab %||% value_col, color = "Province") +
    ggplot2::theme_minimal()

  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)

  p
}

#' Map any indicator by province or municipality
#'
#' General-purpose choropleth: joins a tidy indicator tibble (keyed by
#' `GEOID`, matching [get_ine_geo()]'s output — e.g. [get_ine_demog()]'s
#' output already has it) onto province or municipality geometry and
#' renders it, either as a binned/discrete-legend map (the default,
#' matching the classic "N provinces above/below a threshold" style of
#' choropleth) or a continuous gradient. [map_life_expectancy()] is a
#' thin wrapper around this function for System B's life-table output.
#'
#' @param df A tibble with a `GEOID` column (matching [get_ine_geo()]'s
#'   `GEOID` for the requested `geo_level`) and `value_col`.
#' @param value_col Character, the name of the numeric column to map.
#' @param geo_level Character, one of `"province"` or `"municipality"`.
#' @param region Optional character vector to subset the geometry by
#'   region name (regular expressions allowed). See [get_ine_geo()].
#' @param binned Logical (default `TRUE`). If `TRUE`, uses a discrete,
#'   binned color scale (`ggplot2::scale_fill_fermenter()`) — a fixed
#'   number of color bands with a legend showing each band's range,
#'   matching the classic choropleth style. If `FALSE`, uses a continuous
#'   gradient (`ggplot2::scale_fill_viridis_c()` by default).
#' @param breaks Numeric vector of bin edges when `binned = TRUE`. If
#'   `NULL` (default), computed automatically via `pretty()`.
#' @param palette Character, a `RColorBrewer` palette name when
#'   `binned = TRUE` (default `"PiYG"`, a pink-to-green diverging
#'   palette well suited to ratios centered on 1) or a `viridis` option
#'   letter when `binned = FALSE` (default `"D"`).
#' @param moveCAN Logical, shift the Canary Islands next to the mainland
#'   (default `TRUE`). See [get_ine_geo()].
#' @param can_gap_km Numeric, gap in km for the shifted Canary Islands
#'   (default `60`). See [get_ine_geo()].
#' @param legend_title Optional legend title (defaults to `value_col`).
#' @param title Optional plot title.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' # Not run: get_ine_geo() requires live network access to fetch boundaries.
#' pop_data <- get_ine_demog(indicator = "population_total", year = 2023)
#' map_indicator(pop_data, "population_total", geo_level = "province")
#' }
#' @export
map_indicator <- function(df, value_col, geo_level = c("province", "municipality"),
                           region = NULL, binned = TRUE, breaks = NULL,
                           palette = if (binned) "PiYG" else "D",
                           moveCAN = TRUE, can_gap_km = 60,
                           legend_title = NULL, title = NULL) {
  geo_level <- match.arg(geo_level)
  legend_title <- legend_title %||% value_col

  geo <- get_ine_geo(geo_level = geo_level, region = region, moveCAN = moveCAN, can_gap_km = can_gap_km)
  geo <- dplyr::left_join(geo, df[c("GEOID", value_col)], by = "GEOID")

  if (any(is.na(geo[[value_col]]))) {
    warning(sum(is.na(geo[[value_col]])), " region(s) had no matching value for '", value_col, "'.")
  }

  p <- ggplot2::ggplot(geo) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[value_col]]), color = "white", linewidth = 0.1)

  p <- if (binned) {
    brks <- breaks %||% pretty(geo[[value_col]], n = 6)
    # direction = 1 maps low values to the first (pink) end and high values
    # to the last (green) end of PiYG-style palettes; scale_fill_fermenter()'s
    # own default is the reverse, which is misleading for ratios (would show
    # low-ratio/depopulating regions as green, not pink).
    p + ggplot2::scale_fill_fermenter(name = legend_title, palette = palette, breaks = brks, direction = 1)
  } else {
    p + ggplot2::scale_fill_viridis_c(name = legend_title, option = palette)
  }

  # theme_void()'s plot.background is transparent as of ggplot2 >= 4.0
  # (fill = NULL); set it explicitly so saved images have a solid white
  # background regardless of ggplot2 version or viewer.
  p <- p + ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  box <- attr(geo, "can_box")
  if (!is.null(box)) {
    p <- p + ggplot2::geom_sf(data = box, fill = NA, color = "grey60", linewidth = 0.3)
  }

  if (!is.null(title)) p <- p + ggplot2::ggtitle(title)

  p
}

#' Map life expectancy by province
#'
#' Bridges System B's mortality pipeline ([build_life_tables()] via
#' [life_expectancy_summary()]) with System A's spatial layer
#' ([get_ine_geo()]) to render a province-level choropleth of life
#' expectancy at birth. A thin wrapper around [map_indicator()] (a
#' continuous gradient, since life expectancy has no natural discrete
#' bins) that handles the `nuts3_code` -> `GEOID` bridge via
#' `province_lookup`. See [plot_ine_map()] for the analogous
#' region-highlighting map built purely on [get_ine_geo()].
#'
#' @param le_df Output of [life_expectancy_summary()] (columns
#'   `nuts3_code`, `year`, `e0`, `sex`).
#' @param year Numeric, the year to plot.
#' @param sex Character, one of `"female"`, `"male"`, `"both"`, matching
#'   the `sex` value in `le_df` to plot.
#' @param moveCAN Logical, shift the Canary Islands next to the mainland
#'   (default `TRUE`). See [get_ine_geo()].
#' @param can_gap_km Numeric, gap in km for the shifted Canary Islands
#'   (default `60`). See [get_ine_geo()].
#' @param title Optional plot title.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' # Not run: get_ine_geo() requires live network access to fetch province
#' # boundaries.
#' lt <- build_life_tables(rates$mx_1x1)
#' le <- life_expectancy_summary(lt$fltper, sex = "female")
#' map_life_expectancy(le, year = max(le$year), sex = "female")
#' }
#' @export
map_life_expectancy <- function(le_df, year, sex = "both",
                                 moveCAN = TRUE, can_gap_km = 60, title = NULL) {
  le <- le_df |>
    dplyr::filter(.data$year == !!year, .data$sex == !!sex) |>
    dplyr::left_join(province_lookup, by = "nuts3_code") |>
    dplyr::select(GEOID = "ine_code", "e0")

  map_indicator(
    le, "e0", geo_level = "province", binned = FALSE, palette = "D",
    moveCAN = moveCAN, can_gap_km = can_gap_km, legend_title = "e0 (years)", title = title
  )
}
