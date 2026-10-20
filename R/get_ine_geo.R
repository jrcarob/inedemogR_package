#' Get INE Geographic Data
#'
#' Retrieves spatial geometries for INE geographic levels via the
#' [mapSpain](https://ropenspain.github.io/mapSpain/) package, which sources
#' boundaries from Instituto Geografico Nacional / GISCO. Geometries are
#' returned with a `GEOID` column that matches the geographic codes returned
#' by [get_ine_demog()], so the two can be joined directly.
#'
#' @param geo_level Character, one of `"municipality"` or `"province"`.
#' @param region Optional character vector to subset by region name
#'   (regular expressions allowed).
#' @param moveCAN Logical, shift the Canary Islands next to the mainland for
#'   compact national maps (default `TRUE`). Pass `FALSE` to keep their real
#'   geographic coordinates.
#' @param can_gap_km Numeric, the gap in kilometers left between the shifted
#'   Canary Islands and the closest point on the mainland coast (default
#'   `60`). Only used when `moveCAN = TRUE`. Smaller values move the
#'   islands closer.
#' @return An `sf` object with columns `GEOID`, `NAME`, and `geometry`,
#'   projected to ETRS89 / UTM zone 30N (EPSG:25830). When `moveCAN = TRUE`
#'   and the shift was applied, an sfc rectangle marking the shifted
#'   islands' extent is attached as the `"can_box"` attribute (used by
#'   [plot_ine_map()] to draw an inset frame).
#' @examples
#' \dontrun{
#' # Not run: requires live network access to fetch boundaries via mapSpain.
#' prov_geo <- get_ine_geo(geo_level = "province")
#' mun_geo <- get_ine_geo(geo_level = "municipality", region = "Sevilla")
#' }
#' @export
get_ine_geo <- function(geo_level = c("municipality", "province"), region = NULL,
                         moveCAN = TRUE, can_gap_km = 60) {
  geo_level <- match.arg(geo_level)

  geo <- switch(
    geo_level,
    municipality = mapSpain::esp_get_munic(moveCAN = FALSE) |>
      dplyr::transmute(GEOID = paste0(.data$cpro, .data$cmun), NAME = .data$name),
    province = mapSpain::esp_get_prov(moveCAN = FALSE) |>
      dplyr::transmute(GEOID = .data$cpro, NAME = .data$ine.prov.name)
  )

  geo <- sf::st_transform(geo, 25830) # ETRS89 / UTM zone 30N for Spain

  if (isTRUE(moveCAN)) {
    geo <- shift_canary_islands(geo, gap_km = can_gap_km)
  }

  if (!is.null(region)) {
    pattern <- paste(region, collapse = "|")
    geo <- dplyr::filter(geo, stringr::str_detect(.data$NAME, pattern))
  }

  geo
}

# Canary Islands province codes (Las Palmas, Santa Cruz de Tenerife) are the
# first two characters of GEOID at both province level (GEOID == cpro) and
# municipality level (GEOID == cpro + cmun).
is_canary <- function(geoid) substr(geoid, 1, 2) %in% c("35", "38")

# Translates the Canary Islands so they sit `gap_km` from the closest point
# on the mainland coast (near Cadiz/Huelva), rather than mapSpain's default
# offset or a bounding-box corner (which, because the mainland's bounding
# box is dominated by Galicia's longitude and the peninsula's southern tip,
# sits far from any actual coastline and still looks distant). Attaches the
# shifted islands' bounding box as a `"can_box"` attribute for
# plot_ine_map() to draw as an inset frame.
shift_canary_islands <- function(geo, gap_km = 60) {
  can <- is_canary(geo$GEOID)
  if (!any(can) || all(can)) {
    return(geo)
  }

  mainland <- sf::st_union(sf::st_geometry(geo[!can, ]))
  canary <- sf::st_union(sf::st_geometry(geo[can, ]))

  nearest <- sf::st_coordinates(sf::st_nearest_points(canary, mainland))
  from <- nearest[1, c("X", "Y")]
  to <- nearest[2, c("X", "Y")]

  direction <- to - from
  unit <- direction / sqrt(sum(direction^2))
  target <- to - unit * (gap_km * 1000)

  offset <- target - from
  shifted <- sf::st_geometry(geo[can, ]) + offset
  sf::st_geometry(geo[can, ]) <- sf::st_set_crs(shifted, sf::st_crs(geo))

  can_box <- sf::st_as_sfc(sf::st_bbox(geo[can, ]))
  attr(geo, "can_box") <- sf::st_set_crs(can_box, sf::st_crs(geo))

  geo
}
