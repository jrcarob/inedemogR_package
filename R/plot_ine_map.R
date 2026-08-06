#' Plot a Map of Spain with Regions Highlighted
#'
#' Plots the full map of Spain at the requested geographic level (provinces
#' or municipalities), with one or more regions highlighted in a different
#' color. Useful for confirming *which* regions a `region` filter passed to
#' [get_ine_demog()] or [get_ine_geo()] actually matches, since
#' [get_ine_geo()] uses regex substring matching by default (see
#' `vignette` examples in `test_cordoba.R`).
#'
#' @param geo_level Character, one of `"province"` or `"municipality"`.
#' @param highlight Character vector of region names to highlight (regular
#'   expressions allowed, matched against the geography name — same
#'   matching rules as `region` in [get_ine_demog()]/[get_ine_geo()]).
#'   If `NULL` (default), the whole map is plotted in `base_color` with no
#'   highlighting.
#' @param base_color Fill color for non-highlighted regions. Default
#'   `"grey90"`.
#' @param highlight_color Fill color for highlighted regions. Default
#'   `"firebrick"`.
#' @param title Optional plot title.
#' @param can_box Logical, draw an inset separator box around the Canary
#'   Islands, which are shifted next to the mainland for a compact map
#'   (default `TRUE`).
#' @param can_gap_km Numeric, how close (in km) the shifted Canary Islands
#'   sit next to the mainland (default `60`). Smaller values move them
#'   closer. See [get_ine_geo()].
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' # Not run: get_ine_geo() requires live network access to fetch boundaries.
#' plot_ine_map(geo_level = "province", highlight = "^Córdoba$")
#' plot_ine_map(geo_level = "municipality", highlight = "Córdoba")
#' }
#' @export
plot_ine_map <- function(geo_level = c("province", "municipality"),
                          highlight = NULL,
                          base_color = "grey90",
                          highlight_color = "firebrick",
                          title = NULL,
                          can_box = TRUE,
                          can_gap_km = 60) {
  geo_level <- match.arg(geo_level)

  geo <- get_ine_geo(geo_level = geo_level, moveCAN = TRUE, can_gap_km = can_gap_km)

  if (!is.null(highlight)) {
    pattern <- paste(highlight, collapse = "|")
    geo$highlighted <- stringr::str_detect(geo$NAME, pattern)
    if (!any(geo$highlighted)) {
      warning("`highlight` did not match any region name.")
    }
  } else {
    geo$highlighted <- FALSE
  }

  p <- ggplot2::ggplot(geo) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data$highlighted),
      color = "white", linewidth = 0.1
    ) +
    ggplot2::scale_fill_manual(
      values = c(`FALSE` = base_color, `TRUE` = highlight_color),
      guide = "none"
    ) +
    ggplot2::theme_void() +
    # theme_void()'s plot.background is transparent as of ggplot2 >= 4.0
    # (fill = NULL); set it explicitly so saved images have a solid
    # white background regardless of ggplot2 version or viewer.
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  box <- attr(geo, "can_box")
  if (isTRUE(can_box) && !is.null(box)) {
    p <- p + ggplot2::geom_sf(
      data = box, fill = NA, color = "grey60", linewidth = 0.3
    )
  }

  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  p
}
