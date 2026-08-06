#' @include helpers.R
NULL

# Period life table construction for the SHMD pipeline, per SHMD Protocol
# Section 12, Step 6 / HMD Methods Protocol V6: Andreev-Kingkade a0,
# Kannisto old-age smoothing, standard life-table recursion.

#' Andreev-Kingkade a0 (mean age at death in the first year of life)
#'
#' Implements the Andreev-Kingkade (2015) piecewise regression specified in
#' HMD Methods Protocol V6 (replacing the older Coale-Demeny approximation).
#'
#' @param m0 Central death rate at age 0.
#' @param sex `"female"` or `"male"` (case-insensitive).
#' @return Numeric a0 value.
#' @examples
#' andreev_kingkade_a0(0.004, "female")
#' andreev_kingkade_a0(0.15, "male")
#' @export
andreev_kingkade_a0 <- function(m0, sex = c("female", "male")) {
  sex <- match.arg(stringr::str_to_lower(sex), c("female", "male"))
  if (is.na(m0)) return(NA_real_)
  if (m0 >= 0.107) return(0.330)
  if (sex == "female") 0.053 + 2.800 * m0 else 0.045 + 2.684 * m0
}

#' @noRd
fit_kannisto <- function(age_vec, mx_vec) {
  valid <- is.finite(mx_vec) & mx_vec > 0 & mx_vec < 1 &
    age_vec >= KANNISTO_FIT_MIN & age_vec <= KANNISTO_FIT_MAX

  if (sum(valid) < 3) {
    warning("Kannisto fit skipped: fewer than 3 valid data points in ages ",
            KANNISTO_FIT_MIN, "-", KANNISTO_FIT_MAX, ".")
    return(NULL)
  }

  x <- age_vec[valid]
  m <- mx_vec[valid]
  logit_m <- log(m / (1 - m))

  fit <- tryCatch(stats::lm(logit_m ~ x), error = function(e) NULL)
  if (is.null(fit)) {
    warning("Kannisto model fitting failed.")
    return(NULL)
  }

  alpha <- unname(stats::coef(fit)[1])
  beta <- unname(stats::coef(fit)[2])

  ext_age <- KANNISTO_FIT_MIN:LIFE_TABLE_MAX
  linpred <- alpha + beta * ext_age
  fitted_mx <- exp(linpred) / (1 + exp(linpred))

  tibble::tibble(age = ext_age, mx_kannisto = fitted_mx)
}

#' @noRd
apply_kannisto_smoothing <- function(mx_df) {
  kannisto <- fit_kannisto(mx_df$age, mx_df$mx)
  below_fit <- mx_df |> dplyr::filter(.data$age < KANNISTO_FIT_MIN)

  if (is.null(kannisto)) return(dplyr::arrange(mx_df, .data$age))

  dplyr::bind_rows(below_fit, dplyr::rename(kannisto, mx = "mx_kannisto")) |>
    dplyr::arrange(.data$age)
}

#' Build a period life table from a mortality schedule
#'
#' Constructs a full 1x1 period life table from a smoothed mx series
#' spanning ages `0:110`, per HMD Methods Protocol V6: Andreev-Kingkade a0
#' at age 0, a(1) = 0.4, a(x) = 0.5 elsewhere; the open age interval closes
#' with `qx = 1` and `Lx = lx/mx`.
#'
#' @param mx_df tibble with columns `age`, `mx` for one year and one sex
#'   (typically Kannisto-smoothed at older ages, see [build_life_tables()]).
#' @param sex `"female"` or `"male"` — selects the Andreev-Kingkade a0 branch.
#' @return tibble: `age`, `mx`, `qx`, `ax`, `lx`, `dx`, `Lx`, `Tx`, `ex`.
#' @examples
#' mx <- data.frame(age = 0:5, mx = c(0.004, 0.0003, 0.0002, 0.0002, 0.0002, 0.0002))
#' build_life_table(mx, "female")
#' @export
build_life_table <- function(mx_df, sex = c("female", "male")) {
  sex <- match.arg(stringr::str_to_lower(sex), c("female", "male"))
  mx_df <- dplyr::arrange(mx_df, .data$age)
  n <- nrow(mx_df)

  ax <- numeric(n)
  ax[1] <- andreev_kingkade_a0(mx_df$mx[1], sex)
  if (n >= 2) ax[2] <- 0.4
  if (n >= 3) ax[3:n] <- 0.5

  mx <- mx_df$mx
  qx <- mx / (1 + (1 - ax) * mx)
  qx[n] <- 1

  lx <- numeric(n)
  dx <- numeric(n)
  lx[1] <- 100000
  for (i in seq_len(n)) {
    dx[i] <- lx[i] * qx[i]
    if (i < n) lx[i + 1] <- lx[i] - dx[i]
  }

  Lx <- numeric(n)
  for (i in seq_len(n)) {
    Lx[i] <- if (i < n) {
      lx[i + 1] + ax[i] * dx[i]
    } else if (is.finite(mx[i]) && mx[i] > 0) {
      lx[i] / mx[i]
    } else {
      lx[i] * ax[i]
    }
  }

  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx

  tibble::tibble(age = mx_df$age, mx = mx, qx = qx, ax = ax, lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
}

#' @noRd
build_life_table_both <- function(mx_total_df) {
  mx_df <- dplyr::arrange(mx_total_df, .data$age)
  n <- nrow(mx_df)

  a0_f <- andreev_kingkade_a0(mx_df$mx[1], "female")
  a0_m <- andreev_kingkade_a0(mx_df$mx[1], "male")

  ax <- numeric(n)
  ax[1] <- mean(c(a0_f, a0_m), na.rm = TRUE)
  if (n >= 2) ax[2] <- 0.4
  if (n >= 3) ax[3:n] <- 0.5

  mx <- mx_df$mx
  qx <- mx / (1 + (1 - ax) * mx)
  qx[n] <- 1

  lx <- numeric(n)
  dx <- numeric(n)
  lx[1] <- 100000
  for (i in seq_len(n)) {
    dx[i] <- lx[i] * qx[i]
    if (i < n) lx[i + 1] <- lx[i] - dx[i]
  }

  Lx <- numeric(n)
  for (i in seq_len(n)) {
    Lx[i] <- if (i < n) {
      lx[i + 1] + ax[i] * dx[i]
    } else if (is.finite(mx[i]) && mx[i] > 0) {
      lx[i] / mx[i]
    } else {
      lx[i] * ax[i]
    }
  }

  Tx <- rev(cumsum(rev(Lx)))
  ex <- Tx / lx

  tibble::tibble(age = mx_df$age, mx = mx, qx = qx, ax = ax, lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = ex)
}

#' Validate a constructed life table
#'
#' Checks: lx strictly non-increasing in age; Lx > 0; ex > 0; qx in \[0, 1\].
#'
#' @param lt Output of [build_life_table()] for one year.
#' @return `list(passed = logical, issues = named list of flagged tibbles)`.
#' @export
validate_life_table <- function(lt) {
  issues <- list()

  lx_check <- lt |>
    dplyr::arrange(.data$age) |>
    dplyr::mutate(lx_increase = .data$lx > dplyr::lag(.data$lx, default = dplyr::first(.data$lx) + 1)) |>
    dplyr::filter(.data$lx_increase)
  if (nrow(lx_check) > 0) issues$lx_not_monotonic <- lx_check |> dplyr::select("age", "lx")

  Lx_check <- lt |> dplyr::filter(.data$Lx <= 0)
  if (nrow(Lx_check) > 0) issues$non_positive_Lx <- Lx_check |> dplyr::select("age", "Lx")

  ex_check <- lt |> dplyr::filter(.data$ex <= 0)
  if (nrow(ex_check) > 0) issues$non_positive_ex <- ex_check |> dplyr::select("age", "ex")

  qx_check <- lt |> dplyr::filter(.data$qx < 0 | .data$qx > 1)
  if (nrow(qx_check) > 0) issues$qx_out_of_bounds <- qx_check |> dplyr::select("age", "qx")

  passed <- length(issues) == 0
  if (!passed) {
    warning("Life table validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ".")
  }

  list(passed = passed, issues = issues)
}

#' @noRd
build_life_tables_one <- function(mx_all, nuts3, province_name) {
  years <- sort(unique(mx_all$year))

  build_one_year <- function(yr, sex) {
    col <- switch(sex, female = "mx_female", male = "mx_male", total = "mx_total")
    yr_df <- mx_all |>
      dplyr::filter(.data$year == yr) |>
      dplyr::select(age = "age", mx = dplyr::all_of(col)) |>
      dplyr::arrange(.data$age)

    smoothed <- apply_kannisto_smoothing(yr_df)
    lt <- if (sex == "total") build_life_table_both(smoothed) else build_life_table(smoothed, sex)
    lt$year <- yr
    lt
  }

  fltper <- purrr::map_dfr(years, build_one_year, sex = "female")
  mltper <- purrr::map_dfr(years, build_one_year, sex = "male")
  bltper <- purrr::map_dfr(years, build_one_year, sex = "total")

  add_geo <- function(df) dplyr::mutate(df, nuts3_code = nuts3, province_name = province_name, .before = 1)

  list(fltper = add_geo(fltper), mltper = add_geo(mltper), bltper = add_geo(bltper))
}

#' Build period life tables for every province
#'
#' Constructs female, male, and both-sex 1x1 period life tables for every
#' province and year present in `mx_1x1`, per SHMD Protocol Section 12,
#' Step 6: Kannisto smoothing/extrapolation of old-age mortality (fitted on
#' ages 80-95, extended to 110+), then standard life-table recursion with
#' the Andreev-Kingkade a0.
#'
#' @param mx_1x1 Output of the `mx_1x1` element of [compute_death_rates()].
#' @return `list(fltper, mltper, bltper, qc)`: each a tibble with
#'   `nuts3_code`, `province_name`, `year`, `age`, `mx`, `qx`, `ax`, `lx`,
#'   `dx`, `Lx`, `Tx`, `ex`; `qc` is a named list of
#'   [validate_life_table()] results, one per province/year/sex table
#'   that failed validation.
#' @examples
#' mx_1x1 <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023,
#'   age = 0:5,
#'   mx_female = c(0.004, 0.0003, 0.0002, 0.0002, 0.0002, 0.0002),
#'   mx_male = c(0.005, 0.0004, 0.0003, 0.0003, 0.0003, 0.0003),
#'   mx_total = c(0.0045, 0.00035, 0.00025, 0.00025, 0.00025, 0.00025)
#' )
#' lt <- build_life_tables(mx_1x1)
#' lt$fltper
#' @export
build_life_tables <- function(mx_1x1) {
  provinces <- unique(mx_1x1$nuts3_code)

  per_province <- purrr::map(provinces, function(nuts3) {
    mx_df <- mx_1x1 |> dplyr::filter(.data$nuts3_code == nuts3)
    province_name <- mx_df$province_name[1]
    build_life_tables_one(mx_df, nuts3, province_name)
  })
  names(per_province) <- provinces

  fltper <- purrr::map_dfr(per_province, "fltper")
  mltper <- purrr::map_dfr(per_province, "mltper")
  bltper <- purrr::map_dfr(per_province, "bltper")

  validate_all <- function(df) {
    df |>
      dplyr::group_by(.data$nuts3_code, .data$year) |>
      dplyr::group_map(~ validate_life_table(.x))
  }
  qc <- list(female = validate_all(fltper), male = validate_all(mltper), both = validate_all(bltper))
  n_failed <- sum(!purrr::map_lgl(c(qc$female, qc$male, qc$both), "passed"))
  if (n_failed > 0) {
    message(n_failed, " year/province/sex life tables flagged validation issues (see returned $qc).")
  } else {
    message("All life tables passed validation.")
  }

  list(fltper = fltper, mltper = mltper, bltper = bltper, qc = qc)
}
