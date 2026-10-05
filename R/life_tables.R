#' @include helpers.R
NULL

# Period life tables by single year of age, following HMD Methods Protocol V6
# Section 7.1: Andreev-Kingkade a0 (Table 3), ax = 1/2 at all other ages,
# Kannisto old-age smoothing fitted by Poisson maximum likelihood (Eqs.
# 67-68) with the data-dependent replacement age Y, combined-sex rates from
# smoothed female-exposure weights (Eqs. 69-73), and a death-weighted
# combined-sex a0 (Eq. 77). Deviations from V6, all forced by INE's data, are
# listed in build_life_tables()'s documentation.

#' Andreev-Kingkade a0 (mean age at death in the first year of life)
#'
#' Period (m0-based) formulas of Andreev and Kingkade (2015) as tabulated in
#' HMD Methods Protocol V6, Table 3. Each interval is closed at its lower
#' bound.
#'
#' | Sex | m0 range | a0 |
#' |---|---|---|
#' | Male | \[0, 0.02300) | 0.14929 - 1.99545 m0 |
#' | Male | \[0.02300, 0.08307) | 0.02832 + 3.26021 m0 |
#' | Male | \[0.08307, Inf) | 0.29915 |
#' | Female | \[0, 0.01724) | 0.14903 - 2.05527 m0 |
#' | Female | \[0.01724, 0.06891) | 0.04667 + 3.88089 m0 |
#' | Female | \[0.06891, Inf) | 0.31411 |
#'
#' @param m0 Central death rate at age 0 (a single number).
#' @param sex `"female"` or `"male"` (case-insensitive).
#' @return Numeric a0 value, `NA` if `m0` is `NA`.
#' @references Andreev, E. M. and Kingkade, W. W. (2015). Average age at
#'   death in infancy and infant mortality level: Reconsidering the
#'   Coale-Demeny formulas at current levels of low mortality. *Demographic
#'   Research* 33(13): 363-390. \doi{10.4054/DemRes.2015.33.13}
#' @examples
#' andreev_kingkade_a0(0.004, "female")
#' andreev_kingkade_a0(0.15, "male")
#' @export
andreev_kingkade_a0 <- function(m0, sex = c("female", "male")) {
  sex <- match.arg(stringr::str_to_lower(sex), c("female", "male"))
  if (is.na(m0)) return(NA_real_)
  if (sex == "male") {
    if (m0 < 0.02300) 0.14929 - 1.99545 * m0
    else if (m0 < 0.08307) 0.02832 + 3.26021 * m0
    else 0.29915
  } else {
    if (m0 < 0.01724) 0.14903 - 2.05527 * m0
    else if (m0 < 0.06891) 0.04667 + 3.88089 * m0
    else 0.31411
  }
}

# Kannisto hazard (HMD V6 Eq. 67), evaluated at exact age x.
#' @noRd
kannisto_mu <- function(a, b, x) {
  z <- a * exp(b * (x - KANNISTO_FIT_MIN))
  z / (1 + z)
}

#' Fit the Kannisto model by Poisson maximum likelihood (HMD V6 Eq. 68):
#' D(x) ~ Poisson(E(x) mu(x + 1/2)), a, b >= 0, so fitted rates cannot fall
#' with age. L-BFGS-B with an analytic gradient and a grid-search start, as in
#' HMD's reference implementation (footnote 21).
#' @noRd
fit_kannisto <- function(age, deaths, exposure) {
  ok <- is.finite(deaths) & is.finite(exposure) & exposure > 0
  x <- age[ok] + 0.5
  d <- deaths[ok]
  e <- exposure[ok]
  if (length(x) < 2 || sum(d) <= 0) {
    stop("Kannisto fit needs deaths at two or more ages >= ", KANNISTO_FIT_MIN, ".", call. = FALSE)
  }
  scale <- sum(d)

  negll <- function(p) {
    mu <- kannisto_mu(p[1], p[2], x)
    -sum(d * log(mu) - e * mu) / scale
  }
  gradient <- function(p) {
    mu <- kannisto_mu(p[1], p[2], x)
    w <- (d / mu - e) * mu * (1 - mu)
    -c(sum(w) / p[1], sum(w * (x - KANNISTO_FIT_MIN))) / scale
  }

  grid <- expand.grid(a = c(0.01, 0.03, 0.05, 0.1, 0.2, 0.5), b = c(0.01, 0.05, 0.1, 0.15, 0.2))
  start <- unlist(grid[which.min(apply(grid, 1, negll)), ])

  fit <- stats::optim(
    start, negll, gradient, method = "L-BFGS-B",
    lower = c(1e-10, 0), upper = c(5, 5), control = list(factr = 1e3)
  )
  if (fit$convergence != 0) {
    fit <- stats::optim(start, negll, method = "BFGS")
    fit$par <- pmax(fit$par, c(1e-10, 0))
  }
  list(a = unname(fit$par[1]), b = unname(fit$par[2]), convergence = fit$convergence)
}

#' Replacement age Y (HMD V6 Section 7.1 and footnote 22): the lowest age from
#' 80 upward with at most 100 female or 100 male deaths, capped at 95. The same
#' Y is used for both sexes.
#' @noRd
kannisto_replacement_age <- function(age, deaths_female, deaths_male) {
  few <- age >= KANNISTO_FIT_MIN & (deaths_female <= 100 | deaths_male <= 100)
  y <- if (any(few, na.rm = TRUE)) min(age[which(few)]) else KANNISTO_Y_MAX
  as.integer(min(max(y, KANNISTO_FIT_MIN), KANNISTO_Y_MAX))
}

#' @noRd
check_life_table_input <- function(age, mx) {
  if (anyDuplicated(age) || !identical(as.numeric(age), as.numeric(seq(0, length(age) - 1)))) {
    stop("Ages must be unique and contiguous single years starting at 0.", call. = FALSE)
  }
  if (any(!is.finite(mx)) || any(mx < 0)) {
    stop("All death rates must be finite and non-negative; missing ages: ",
         paste(age[!is.finite(mx)], collapse = ", "), ".", call. = FALSE)
  }
  if (mx[length(mx)] <= 0) {
    stop("The open-interval death rate must be positive (Lx = lx / mx).", call. = FALSE)
  }
}

# Life-table recursion (HMD V6 Eqs. 74-82): ax = 1/2 except at age 0; the
# last row is the open interval with qx = 1, ax = 1/mx, Lx = lx/mx.
#' @noRd
life_table_core <- function(age, mx, a0) {
  check_life_table_input(age, mx)
  n <- length(mx)
  ax <- rep(0.5, n)
  ax[1] <- a0
  ax[n] <- 1 / mx[n]

  qx <- mx / (1 + (1 - ax) * mx)
  qx[n] <- 1
  lx <- 100000 * cumprod(c(1, 1 - qx[-n]))
  dx <- lx * qx
  Lx <- lx - (1 - ax) * dx
  Lx[n] <- lx[n] / mx[n]
  Tx <- rev(cumsum(rev(Lx)))

  tibble::tibble(
    age = age, mx = mx, qx = qx, ax = ax, lx = lx, dx = dx, Lx = Lx, Tx = Tx, ex = Tx / lx
  )
}

#' Build a period life table from a mortality schedule
#'
#' Standard life-table recursion from a complete schedule of central death
#' rates, per HMD Methods Protocol V6: Andreev-Kingkade a0 at age 0,
#' `ax = 0.5` at every other closed age, and an open final interval with
#' `qx = 1`, `ax = 1/mx`, `Lx = lx/mx`. No smoothing is applied; use
#' [build_life_tables()] for Kannisto old-age smoothing of observed data.
#'
#' @param mx_df Data frame with columns `age` and `mx` for one year and one
#'   sex. Ages must be unique, contiguous single years starting at 0; the last
#'   age is treated as the open interval. Rates must be finite and
#'   non-negative, and the open-interval rate positive.
#' @param sex `"female"` or `"male"`, selecting the Andreev-Kingkade branch.
#' @param a0 Optional override for a0 (used for combined-sex tables).
#' @return tibble: `age`, `mx`, `qx`, `ax`, `lx`, `dx`, `Lx`, `Tx`, `ex`
#'   (radix `l0 = 100000`).
#' @examples
#' mx <- data.frame(age = 0:100, mx = 0.0003 * exp(0.07 * (0:100)))
#' build_life_table(mx, "female")
#' @export
build_life_table <- function(mx_df, sex = c("female", "male"), a0 = NULL) {
  sex <- match.arg(stringr::str_to_lower(sex), c("female", "male"))
  mx_df <- dplyr::arrange(mx_df, .data$age)
  a0 <- a0 %||% andreev_kingkade_a0(mx_df$mx[1], sex)
  life_table_core(mx_df$age, mx_df$mx, a0)
}

#' Validate a constructed life table
#'
#' Fails when any of the following holds: ages are not unique and contiguous
#' from 0; any of `mx`, `qx`, `ax`, `lx`, `dx`, `Lx`, `Tx`, `ex` is missing or
#' non-finite; `lx` increases with age; `Lx` or `ex` is not positive; `qx` lies
#' outside \[0, 1\]; `mx = dx/Lx` does not hold; or life expectancy at birth is
#' outside the plausibility band (15, 100\] years.
#'
#' @param lt Output of [build_life_table()] for one year and sex.
#' @return `list(passed = logical, issues = named list)`.
#' @examples
#' mx <- data.frame(age = 0:100, mx = 0.0003 * exp(0.07 * (0:100)))
#' validate_life_table(build_life_table(mx, "female"))
#' @export
validate_life_table <- function(lt) {
  issues <- list()
  lt <- dplyr::arrange(lt, .data$age)
  cols <- c("mx", "qx", "ax", "lx", "dx", "Lx", "Tx", "ex")

  if (anyDuplicated(lt$age) || !identical(as.numeric(lt$age), as.numeric(seq(0, nrow(lt) - 1)))) {
    issues$ages_not_contiguous <- lt$age
  }
  nonfinite <- !apply(is.finite(as.matrix(lt[cols])), 1, all)
  if (any(nonfinite)) issues$non_finite_values <- lt[nonfinite, c("age", cols)]

  ok <- lt[!nonfinite, ]
  if (any(diff(ok$lx) > 0)) issues$lx_not_monotonic <- ok[c(FALSE, diff(ok$lx) > 0), c("age", "lx")]
  if (any(ok$Lx <= 0)) issues$non_positive_Lx <- ok[ok$Lx <= 0, c("age", "Lx")]
  if (any(ok$ex <= 0)) issues$non_positive_ex <- ok[ok$ex <= 0, c("age", "ex")]
  if (any(ok$qx < 0 | ok$qx > 1)) issues$qx_out_of_bounds <- ok[ok$qx < 0 | ok$qx > 1, c("age", "qx")]

  identity_gap <- abs(ok$dx / ok$Lx - ok$mx) > 1e-8 * pmax(1, ok$mx)
  if (any(identity_gap)) issues$mx_not_dx_over_Lx <- ok[identity_gap, c("age", "mx", "dx", "Lx")]

  e0 <- lt$ex[lt$age == 0]
  if (length(e0) == 1 && is.finite(e0) && (e0 <= 15 || e0 > 100)) issues$implausible_e0 <- e0

  passed <- length(issues) == 0
  if (!passed) {
    warning("Life table validation flagged ", length(issues), " issue categor",
            if (length(issues) == 1) "y" else "ies", ": ",
            paste(names(issues), collapse = ", "), ".")
  }
  list(passed = passed, issues = issues)
}

#' Smoothed female share of exposure at old ages (HMD V6 Eqs. 71-72): weighted
#' least squares of logit(pi) on a quadratic in age, weights E_total.
#' @noRd
smooth_female_share <- function(age, e_female, e_male, new_age) {
  ok <- age >= KANNISTO_FIT_MIN & e_female > 0 & e_male > 0
  if (sum(ok) < 3) return(rep(sum(e_female[ok]) / sum(e_female[ok] + e_male[ok]), length(new_age)))
  pi_f <- e_female[ok] / (e_female[ok] + e_male[ok])
  x <- age[ok]
  fit <- stats::lm(stats::qlogis(pi_f) ~ x + I(x^2), weights = (e_female + e_male)[ok])
  stats::plogis(stats::predict(fit, newdata = data.frame(x = new_age)))
}

#' Build the female, male, and combined-sex life tables for one province-year.
#' @noRd
build_life_tables_year <- function(df, kannisto_age = NULL, fit_min = KANNISTO_FIT_MIN) {
  df <- dplyr::arrange(df, .data$age)
  needed <- c("mx_female", "mx_male", "mx_total", "d_female", "d_male", "e_female", "e_male")
  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns ", paste(missing_cols, collapse = ", "),
         ": pass the `mx_1x1` output of compute_death_rates(), which carries the ",
         "deaths and exposures needed for Kannisto smoothing.", call. = FALSE)
  }
  if (!identical(as.integer(df$age), 0:MAX_AGE)) {
    stop("Incomplete age coverage: need contiguous single ages 0-", MAX_AGE,
         " (got ", length(df$age), " ages).", call. = FALSE)
  }
  y <- kannisto_age %||% kannisto_replacement_age(df$age, df$d_female, df$d_male)
  fit_rows <- df$age >= fit_min & df$age < MAX_AGE
  for (s in c("female", "male")) {
    e <- df[[paste0("e_", s)]]
    if (any(!is.finite(e) | e < 0)) {
      stop("Incomplete age coverage: missing ", s, " exposure at ages ",
           paste(df$age[!is.finite(e) | e < 0], collapse = ", "),
           " (e.g. population top-coded below age ", MAX_AGE, ").", call. = FALSE)
    }
    bad <- df$age < y & !is.finite(df[[paste0("mx_", s)]])
    if (any(bad)) {
      stop("Incomplete age coverage: non-finite ", s, " rates below the Kannisto age ", y,
           " at ages ", paste(df$age[bad], collapse = ", "), ".", call. = FALSE)
    }
    if (sum(fit_rows & e > 0) < 5) {
      stop("Too few ages with positive ", s, " exposure in ", fit_min, "-",
           MAX_AGE - 1, " to fit the Kannisto model.", call. = FALSE)
    }
  }
  out_age <- 0:LIFE_TABLE_MAX
  smoothed_age <- out_age[out_age >= y]

  smoothed <- list()
  for (s in c("female", "male")) {
    fit <- fit_kannisto(df$age[fit_rows], df[[paste0("d_", s)]][fit_rows], df[[paste0("e_", s)]][fit_rows])
    smoothed[[s]] <- c(df[[paste0("mx_", s)]][df$age < y], kannisto_mu(fit$a, fit$b, smoothed_age + 0.5))
  }
  pi_f <- smooth_female_share(df$age, df$e_female, df$e_male, smoothed_age)
  smoothed$total <- c(
    df$mx_total[df$age < y],
    pi_f * smoothed$female[out_age >= y] + (1 - pi_f) * smoothed$male[out_age >= y]
  )

  a0f <- andreev_kingkade_a0(smoothed$female[1], "female")
  a0m <- andreev_kingkade_a0(smoothed$male[1], "male")
  d0 <- c(df$d_female[1], df$d_male[1])
  a0t <- if (sum(d0) > 0) sum(c(a0f, a0m) * d0) / sum(d0) else mean(c(a0f, a0m))

  list(
    female = life_table_core(out_age, smoothed$female, a0f),
    male = life_table_core(out_age, smoothed$male, a0m),
    total = life_table_core(out_age, smoothed$total, a0t),
    y = y
  )
}

#' Build period life tables for every province
#'
#' Constructs female, male, and combined-sex single-year period life tables
#' (ages 0 to 110+) for every province and year in `mx_1x1`.
#'
#' @section Method:
#' Follows HMD Methods Protocol V6, Section 7.1:
#' * Observed rates are used below the replacement age `Y`, the lowest age
#'   from 80 with at most 100 female or 100 male deaths, capped at 95 (the
#'   same `Y` for both sexes).
#' * From `Y` upward, rates come from a Kannisto model fitted to deaths and
#'   exposures at ages 80-99 by Poisson maximum likelihood, with both
#'   parameters constrained to be non-negative so smoothed rates cannot fall
#'   with age.
#' * Combined-sex rates at ages `Y`+ weight the smoothed sex-specific rates by
#'   a smoothed female share of exposure; combined-sex a0 is the
#'   death-weighted average of the sex-specific Andreev-Kingkade values.
#'
#' Deviations from HMD V6, all forced by the INE inputs: (i) exposure uses an
#' even Lexis-triangle split because INE deaths are not classified by cohort
#' (see [compute_exposure()]); (ii) the Kannisto fit uses ages 80-99 because
#' INE's single-year detail ends at an open interval 100+, whereas HMD fits to
#' 80-110+; (iii) no birth-by-month exposure correction.
#'
#' @section Failure behaviour:
#' A province-year whose input lacks finite rates and exposures at every
#' single age 0-100 is not tabulated: its three tables are withheld and the
#' reason is reported in `failed`. This covers INE releases whose population
#' or deaths are top-coded below age 100 (for example, some historical years
#' for Ceuta and Melilla).
#'
#' @param mx_1x1 The `mx_1x1` element of [compute_death_rates()] (needs the
#'   `mx_*`, `d_*`, and `e_*` columns).
#' @param kannisto_age Optional fixed replacement age `Y` (an integer in
#'   80-100) overriding the HMD 100-deaths rule; for sensitivity analysis.
#' @param fit_min First age of the Kannisto fitting window (default 80, as in
#'   HMD; the window ends at 99). For sensitivity analysis.
#' @return A list:
#'   * `fltper`, `mltper`, `bltper`: tibbles with `nuts3_code`,
#'     `province_name`, `year`, `age`, `mx`, `qx`, `ax`, `lx`, `dx`, `Lx`,
#'     `Tx`, `ex`.
#'   * `qc`: tibble with one row per table built (`nuts3_code`, `year`,
#'     `sex`, `kannisto_age` = Y, `passed`, `issues`).
#'   * `failed`: tibble of withheld tables (`nuts3_code`, `province_name`,
#'     `year`, `sex`, `reason`).
#' @examples
#' age <- 0:100
#' e <- 1e5
#' mx <- 0.0003 * exp(0.07 * age)
#' mx_1x1 <- tibble::tibble(
#'   nuts3_code = "ES111", province_name = "A Coruna", year = 2023, age = age,
#'   mx_female = mx, mx_male = 1.2 * mx, mx_total = 1.1 * mx,
#'   d_female = mx * e, d_male = 1.2 * mx * e, d_total = 2.2 * mx * e,
#'   e_female = e, e_male = e, e_total = 2 * e
#' )
#' lt <- build_life_tables(mx_1x1)
#' lt$fltper
#' @export
build_life_tables <- function(mx_1x1, kannisto_age = NULL, fit_min = KANNISTO_FIT_MIN) {
  if (!is.null(kannisto_age) && !(kannisto_age %in% KANNISTO_FIT_MIN:MAX_AGE)) {
    stop("`kannisto_age` must be an integer between ", KANNISTO_FIT_MIN, " and ", MAX_AGE, ".")
  }
  keys <- dplyr::distinct(mx_1x1, .data$nuts3_code, .data$province_name, .data$year)
  tables <- list(female = list(), male = list(), total = list())
  qc <- list()
  failed <- list()

  for (i in seq_len(nrow(keys))) {
    k <- keys[i, ]
    df <- mx_1x1[mx_1x1$nuts3_code == k$nuts3_code & mx_1x1$year == k$year, ]
    res <- tryCatch(build_life_tables_year(df, kannisto_age, fit_min), error = function(e) e)
    if (inherits(res, "error")) {
      failed[[length(failed) + 1]] <- tibble::tibble(
        k, sex = c("female", "male", "total"), reason = conditionMessage(res)
      )
      next
    }
    for (s in c("female", "male", "total")) {
      v <- suppressWarnings(validate_life_table(res[[s]]))
      qc[[length(qc) + 1]] <- tibble::tibble(
        nuts3_code = k$nuts3_code, year = k$year, sex = s, kannisto_age = res$y,
        passed = v$passed, issues = list(v$issues)
      )
      tables[[s]][[length(tables[[s]]) + 1]] <- tibble::tibble(k, res[[s]])
    }
  }

  bind <- function(x) {
    if (length(x) == 0) {
      return(tibble::tibble(
        nuts3_code = character(), province_name = character(), year = numeric(),
        age = numeric(), mx = numeric(), qx = numeric(), ax = numeric(), lx = numeric(),
        dx = numeric(), Lx = numeric(), Tx = numeric(), ex = numeric()
      ))
    }
    dplyr::bind_rows(x)
  }
  qc <- dplyr::bind_rows(qc)
  failed <- if (length(failed) > 0) {
    dplyr::bind_rows(failed)
  } else {
    tibble::tibble(nuts3_code = character(), province_name = character(),
                   year = numeric(), sex = character(), reason = character())
  }

  if (nrow(failed) > 0) {
    warning(nrow(failed) / 3, " province-year(s) withheld (incomplete or invalid input); see $failed.")
  }
  n_flagged <- if (nrow(qc) > 0) sum(!qc$passed) else 0
  if (n_flagged > 0) {
    message(n_flagged, " life table(s) flagged by validate_life_table(); see $qc.")
  } else if (nrow(qc) > 0) {
    message("All ", nrow(qc), " life tables passed validation.")
  }

  list(
    fltper = bind(tables$female), mltper = bind(tables$male), bltper = bind(tables$total),
    qc = qc, failed = failed
  )
}
