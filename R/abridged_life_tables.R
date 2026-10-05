#' @include helpers.R life_tables.R death_rates.R
NULL

# Abridged (5-year age group) period life tables, built directly from
# compute_death_rates()'s mx_5x1 output - a standalone capability distinct
# from build_life_tables()'s single-year (1x1) tables, useful when
# comparing against other agencies' published abridged tables or working
# with grouped-only data. Standard discrete abridged life-table method
# (Chiang's formula for nqx from nMx; Preston, Heuveline & Guillot (2001),
# "Demography: Measuring and Modeling Population Processes," Ch. 3),
# except for the youngest group: mx_5x1's own age grouping merges age 0
# with ages 1-4 into a single "00-04" group, which would otherwise need
# an external, less-precise nax(0-4) regression (e.g. Coale-Demeny) to
# handle the sharp infant-mortality gradient within that interval. Since
# compute_death_rates() already computes single-year mx (mx_1x1) with an
# exact Andreev-Kingkade a0 for age 0, this instead builds an exact
# single-year sub-table for ages 0-4 from mx_1x1 and aggregates it -
# avoiding that approximation entirely for the one interval where it
# matters most.

#' @noRd
parse_age_group_labels <- function(age_group) {
  is_open <- stringr::str_detect(age_group, "\\+$")

  # Computed unconditionally on every label (dplyr::if_else()'s branches
  # are both evaluated), so extracting a trailing "\\d+$" from an
  # open-interval label like "100+" (which has none) is expected to
  # yield NA and is harmless - suppress the resulting coercion warning
  # rather than let it leak to callers.
  age_start <- as.integer(ifelse(
    is_open, stringr::str_remove(age_group, "\\+$"), stringr::str_extract(age_group, "^\\d+")
  ))
  age_end <- suppressWarnings(as.integer(stringr::str_extract(age_group, "\\d+$")))
  age_end[is_open] <- NA_integer_
  n <- age_end - age_start + 1L

  tibble::tibble(age_group = age_group, age_start = age_start, n = n, is_open = is_open)
}

#' @noRd
build_infant_segment <- function(mx0_4, a0) {
  # Single-year recursion for ages 0-4 with the same conventions as
  # build_life_table() (Andreev-Kingkade a0, ax = 0.5 at ages 1-4), then
  # aggregated into the 0-4 group.
  ax <- c(a0, 0.5, 0.5, 0.5, 0.5)
  qx <- mx0_4 / (1 + (1 - ax) * mx0_4)
  lx <- 100000 * cumprod(c(1, 1 - qx))
  dx <- lx[1:5] * qx
  L0_4 <- sum(lx[1:5] - (1 - ax) * dx)
  l5 <- lx[6]
  d0_4 <- lx[1] - l5
  # nLx = n*l(x+n) + nax*ndx  =>  nax = (nLx - n*l(x+n)) / ndx
  a0_4 <- if (d0_4 > 0) (L0_4 - 5 * l5) / d0_4 else 2.5
  list(l0 = lx[1], L0_4 = L0_4, a0_4 = a0_4, q0_4 = d0_4 / lx[1])
}

# Closed groups 5-9, ..., 95-99 use Greville's nax (Preston, Heuveline &
# Guillot 2001, Box 3.1), nax = n/2 - n^2/12 (nmx - k) with
# k = log(m[x+n] / m[x-n]) / (2n), and the matching Chiang conversion
# nqx = n nmx / (1 + (n - nax) nmx), nLx = n l(x+n) + nax ndx. Any nax gives
# nmx = ndx / nLx exactly with this pair. Where k is undefined (a zero rate in
# a neighbouring group) or nax would push nqx above 1, the group falls back to
# a constant hazard: nqx = 1 - exp(-n nmx), nLx = ndx / nmx. The open interval
# has qx = 1 and Lx = lx / mx.
#' @noRd
build_abridged_life_table_engine <- function(mx_5x1, mx0_4, a0) {
  groups <- parse_age_group_labels(mx_5x1$age_group) |>
    dplyr::mutate(mx = mx_5x1$mx) |>
    dplyr::arrange(.data$age_start)
  if (length(mx0_4) != 5) stop("`mx_1x1` must have single-year rates for ages 0-4.")
  if (any(!is.finite(groups$mx)) || any(!is.finite(mx0_4))) {
    stop("All grouped and age 0-4 death rates must be finite.", call. = FALSE)
  }
  k <- nrow(groups)
  n <- groups$n
  mx <- groups$mx
  infant <- build_infant_segment(mx0_4, a0)

  closed <- seq_len(k)[-c(1, k)]
  slope <- log(mx[closed + 1] / mx[closed - 1]) / (2 * n[closed])
  ax <- rep(NA_real_, k)
  ax[closed] <- n[closed] / 2 - n[closed]^2 / 12 * (mx[closed] - slope)
  greville <- is.finite(ax) & ax > 0 & ax < n & ax * mx < 1
  qx <- ifelse(greville, n * mx / (1 + (n - ax) * mx), 1 - exp(-n * mx))
  qx[1] <- infant$q0_4
  qx[k] <- 1
  lx <- infant$l0 * cumprod(c(1, 1 - qx[-k]))
  dx <- lx * qx
  l_next <- c(lx[-1], 0)
  Lx <- ifelse(greville, n * l_next + ax * dx, ifelse(mx > 0, dx / mx, n * lx))
  Lx[1] <- infant$L0_4
  Lx[k] <- lx[k] / mx[k]
  ax <- ifelse(greville, ax, ifelse(dx > 0, (Lx - n * l_next) / dx, n / 2))
  ax[1] <- infant$a0_4
  ax[k] <- 1 / mx[k]
  Tx <- rev(cumsum(rev(Lx)))

  tibble::tibble(
    age_group = groups$age_group, age_start = groups$age_start, n = n,
    mx = mx, ax = ax, qx = qx, lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = Tx / lx
  )
}

#' Build an abridged (5-year age group) period life table
#'
#' Constructs an abridged period life table (age groups `"00-04"`,
#' `"05-09"`, ..., `"95-99"`, `"100+"`) independently from observed grouped
#' central death rates. It is a separate estimate, not an abridgement of the
#' single-year table from [build_life_tables()]: it uses no Kannisto
#' smoothing, and its open interval is 100+ rather than 110+, so the two
#' tables' life expectancies differ slightly by construction.
#'
#' Closed groups from age 5 use Greville's `nax` with the Chiang
#' conversion `nqx = n nmx / (1 + (n - nax) nmx)` and
#' `nLx = n l(x+n) + nax ndx` (Preston, Heuveline & Guillot 2001, Box 3.1),
#' falling back to a constant hazard (`nqx = 1 - exp(-n nmx)`,
#' `nLx = ndx / nmx`) where Greville's slope term is undefined. Both keep
#' `nmx = ndx / nLx` exactly. The open interval has `qx = 1`,
#' `Lx = lx / mx`.
#' Complements [build_life_table()]'s single-year (1x1) tables - use
#' this when comparing against other agencies' published abridged
#' tables, or when only grouped-age data is available. The youngest
#' group (`"00-04"`) is the one exception to using `mx_5x1` alone: it is
#' built from an exact single-year sub-table using `mx_1x1`'s
#' Andreev-Kingkade `a0` (see [andreev_kingkade_a0()]), avoiding the
#' external nax(0-4) regression a from-mx_5x1-alone method would
#' otherwise need for that interval's sharp infant-mortality gradient.
#'
#' @param mx_1x1 tibble with columns `age` (single years, at least
#'   `0:4`), `mx`, for one year and one sex - see [compute_death_rates()]'s
#'   `mx_1x1` output, filtered to one province/year/sex.
#' @param mx_5x1 tibble with columns `age_group` (`"00-04"`, `"05-09"`,
#'   ..., `"100+"`), `mx`, for the same year and sex - see
#'   [compute_death_rates()]'s `mx_5x1` output, filtered the same way.
#' @param sex `"female"` or `"male"` - selects the Andreev-Kingkade a0
#'   branch for the youngest group, same as [build_life_table()].
#' @return tibble: `age_group`, `age_start`, `n` (interval width, `NA`
#'   for the open interval), `mx`, `ax`, `qx`, `lx`, `dx`, `Lx`, `Tx`, `ex`.
#' @examples
#' mx1 <- data.frame(age = 0:4, mx = c(0.004, 0.0003, 0.0002, 0.0002, 0.0002))
#' mx5 <- data.frame(
#'   age_group = c("00-04", "05-09", "100+"), mx = c(0.0006, 0.0001, 0.35)
#' )
#' build_abridged_life_table(mx1, mx5, "female")
#' @export
build_abridged_life_table <- function(mx_1x1, mx_5x1, sex = c("female", "male")) {
  sex <- match.arg(stringr::str_to_lower(sex), c("female", "male"))

  mx0_4 <- mx_1x1 |> dplyr::filter(.data$age %in% 0:4) |> dplyr::arrange(.data$age)
  build_abridged_life_table_engine(mx_5x1, mx0_4$mx, andreev_kingkade_a0(mx0_4$mx[1], sex))
}

#' Validate a constructed abridged life table
#'
#' Fails when any column is missing or non-finite, `lx` increases, `Lx`
#' or `ex` is not positive, `qx` lies outside \[0, 1\], or the identity
#' `mx = dx / Lx` fails in a closed interval from age 5 upward (the 0-4
#' group is built from single-year rates, so its grouped `mx` need not
#' satisfy it exactly).
#'
#' @param alt Output of [build_abridged_life_table()] for one year.
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @examples
#' mx1 <- data.frame(age = 0:4, mx = c(0.004, 0.0003, 0.0002, 0.0002, 0.0002))
#' mx5 <- data.frame(
#'   age_group = c("00-04", "05-09", "100+"), mx = c(0.0006, 0.0001, 0.35)
#' )
#' alt <- build_abridged_life_table(mx1, mx5, "female")
#' validate_abridged_life_table(alt)
#' @export
validate_abridged_life_table <- function(alt) {
  issues <- list()

  alt <- dplyr::arrange(alt, .data$age_start)

  lx_check <- alt |>
    dplyr::mutate(lx_increase = .data$lx > dplyr::lag(.data$lx, default = dplyr::first(.data$lx) + 1)) |>
    dplyr::filter(.data$lx_increase)
  if (nrow(lx_check) > 0) issues$lx_not_monotonic <- lx_check |> dplyr::select("age_group", "lx")

  cols <- c("mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")
  nonfinite <- !apply(is.finite(as.matrix(alt[cols])), 1, all)
  if (any(nonfinite)) issues$non_finite_values <- alt[nonfinite, c("age_group", cols)]

  closed <- alt[-c(1, nrow(alt)), ]
  gap <- abs(closed$dx / closed$Lx - closed$mx) > 1e-8 * pmax(1, closed$mx)
  if (any(gap, na.rm = TRUE)) {
    issues$mx_not_dx_over_Lx <- closed[which(gap), c("age_group", "mx", "dx", "Lx")]
  }

  Lx_check <- alt |> dplyr::filter(.data$Lx <= 0 | is.na(.data$Lx))
  if (nrow(Lx_check) > 0) issues$non_positive_Lx <- Lx_check |> dplyr::select("age_group", "Lx")

  ex_check <- alt |> dplyr::filter(.data$ex <= 0)
  if (nrow(ex_check) > 0) issues$non_positive_ex <- ex_check |> dplyr::select("age_group", "ex")

  qx_check <- alt |> dplyr::filter(.data$qx < 0 | .data$qx > 1)
  if (nrow(qx_check) > 0) issues$qx_out_of_bounds <- qx_check |> dplyr::select("age_group", "qx")

  passed <- length(issues) == 0
  if (!passed) {
    warning("Abridged life table validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
  }

  list(passed = passed, issues = issues)
}

# Combined-sex a0: death-weighted average of the sex-specific values (HMD V6
# Eq. 77); simple average when age-0 death counts are not supplied.
#' @noRd
total_a0 <- function(mx_1x1_year) {
  r <- mx_1x1_year[mx_1x1_year$age == 0, ]
  a0 <- c(andreev_kingkade_a0(r$mx_female, "female"), andreev_kingkade_a0(r$mx_male, "male"))
  d0 <- if (all(c("d_female", "d_male") %in% names(r))) c(r$d_female, r$d_male) else c(1, 1)
  if (sum(d0) > 0) sum(a0 * d0) / sum(d0) else mean(a0)
}

#' @noRd
build_abridged_life_tables_one <- function(mx_1x1_all, mx_5x1_all, nuts3, province_name) {
  years <- sort(unique(mx_5x1_all$year))

  build_one_year <- function(yr, sex) {
    col <- switch(sex, female = "mx_female", male = "mx_male", total = "mx_total")
    mx1 <- mx_1x1_all |>
      dplyr::filter(.data$year == yr) |>
      dplyr::select(age = "age", mx = dplyr::all_of(col))
    mx5 <- mx_5x1_all |>
      dplyr::filter(.data$year == yr) |>
      dplyr::select(age_group = "age_group", mx = dplyr::all_of(col))

    alt <- tryCatch(
      if (sex == "total") {
        mx0_4 <- dplyr::arrange(dplyr::filter(mx1, .data$age %in% 0:4), .data$age)$mx
        build_abridged_life_table_engine(mx5, mx0_4, total_a0(mx_1x1_all[mx_1x1_all$year == yr, ]))
      } else {
        build_abridged_life_table(mx1, mx5, sex = sex)
      },
      error = function(e) {
        warning(province_name, " ", yr, " ", sex, ": abridged table withheld (",
                conditionMessage(e), ")", call. = FALSE)
        NULL
      }
    )
    if (!is.null(alt)) alt$year <- yr
    alt
  }

  fltper <- purrr::map_dfr(years, build_one_year, sex = "female")
  mltper <- purrr::map_dfr(years, build_one_year, sex = "male")
  bltper <- purrr::map_dfr(years, build_one_year, sex = "total")

  add_geo <- function(df) dplyr::mutate(df, nuts3_code = nuts3, province_name = province_name, .before = 1)

  list(fltper = add_geo(fltper), mltper = add_geo(mltper), bltper = add_geo(bltper))
}

#' Build abridged period life tables for every province
#'
#' Constructs female, male, and both-sex abridged (5-year age group)
#' period life tables for every province and year present in `mx_5x1`,
#' from [compute_death_rates()]'s `mx_1x1`/`mx_5x1` output. See
#' [build_abridged_life_table()] for the method; [build_life_tables()]
#' for the single-year (1x1) equivalent this complements.
#'
#' @param mx_1x1 Output of the `mx_1x1` element of [compute_death_rates()].
#' @param mx_5x1 Output of the `mx_5x1` element of [compute_death_rates()].
#' @return `list(fltper, mltper, bltper, qc)`: each a tibble with
#'   `nuts3_code`, `province_name`, `year`, `age_group`, `age_start`,
#'   `n`, `mx`, `ax`, `qx`, `lx`, `dx`, `Lx`, `Tx`, `ex`; `qc` is a named
#'   list of [validate_abridged_life_table()] results.
#' @examples
#' mx_1x1 <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = 0:4, mx_female = c(0.004, 0.0003, 0.0002, 0.0002, 0.0002),
#'   mx_male = c(0.005, 0.0004, 0.0003, 0.0003, 0.0003),
#'   mx_total = c(0.0045, 0.00035, 0.00025, 0.00025, 0.00025)
#' )
#' mx_5x1 <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age_group = c("00-04", "100+"),
#'   mx_female = c(0.0006, 0.35), mx_male = c(0.0008, 0.4), mx_total = c(0.0007, 0.38)
#' )
#' alt <- build_abridged_life_tables(mx_1x1, mx_5x1)
#' alt$fltper
#' @export
build_abridged_life_tables <- function(mx_1x1, mx_5x1) {
  provinces <- unique(mx_5x1$nuts3_code)

  per_province <- purrr::map(provinces, function(nuts3) {
    mx1_df <- mx_1x1 |> dplyr::filter(.data$nuts3_code == nuts3)
    mx5_df <- mx_5x1 |> dplyr::filter(.data$nuts3_code == nuts3)
    province_name <- mx5_df$province_name[1]
    build_abridged_life_tables_one(mx1_df, mx5_df, nuts3, province_name)
  })
  names(per_province) <- provinces

  fltper <- purrr::map_dfr(per_province, "fltper")
  mltper <- purrr::map_dfr(per_province, "mltper")
  bltper <- purrr::map_dfr(per_province, "bltper")

  validate_all <- function(df) {
    df |>
      dplyr::group_by(.data$nuts3_code, .data$year) |>
      dplyr::group_map(~ validate_abridged_life_table(.x))
  }
  qc <- list(female = validate_all(fltper), male = validate_all(mltper), both = validate_all(bltper))
  n_failed <- sum(!purrr::map_lgl(c(qc$female, qc$male, qc$both), "passed"))
  if (n_failed > 0) {
    message(n_failed, " year/province/sex abridged life tables flagged validation issues (see returned $qc).")
  } else {
    message("All abridged life tables passed validation.")
  }

  list(fltper = fltper, mltper = mltper, bltper = bltper, qc = qc)
}
