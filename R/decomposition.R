#' @include life_tables.R
NULL

# Mortality decomposition: attributing a difference in life expectancy at
# birth between two life tables (two provinces in the same year, or the
# same province in two different years) to age-specific contributions.
# Two independently-derived methods are implemented so results can be
# cross-checked against each other (Ponnapalli 2005 found the two methods
# are not sensitive to which one is used - a useful consistency check,
# not a redundancy):
#
# - Arriaga (1984): an exact discrete-time decomposition using life table
#   quantities directly (lx, Lx, Tx) - the standard, most widely
#   implemented method. Reference: Arriaga, E. (1984). "Measuring and
#   Explaining the Change in Life Expectancies." Demography 21(1):83-96.
#   Formulas as reproduced in Rodriguez, G. (2017), "Life Expectancy
#   Decompositions" (Princeton ECO 572/SOC 532 course notes).
#
# - Pollard (1988): an exact continuous-time decomposition,
#   e2(0) - e1(0) = integral of (mu1(x) - mu2(x)) * l2(x)/l0_2 * e1(x) dx
#   (equivalently, with the "1"/"2" life tables swapped - both forms are
#   exact). This package uses the symmetric average of the two exact
#   forms, discretized over single-year age intervals using mx as the
#   piecewise-constant hazard (the same approximation build_life_table()
#   already makes for qx). Reference: Pollard, J.H. (1988). "On the
#   Decomposition of Changes in Expectation of Life and Differentials in
#   Life Expectancy." Demography 25(2):265-276.

#' @noRd
decompose_arriaga_one <- function(lt1, lt2) {
  n <- nrow(lt1)
  l0_1 <- lt1$lx[1]

  contribution <- numeric(n)
  for (i in seq_len(n)) {
    if (i < n) {
      direct <- (lt1$lx[i] / l0_1) * (lt2$Lx[i] / lt2$lx[i] - lt1$Lx[i] / lt1$lx[i])
      indirect_interaction <- (lt2$Tx[i + 1] / l0_1) *
        (lt1$lx[i] / lt2$lx[i] - lt1$lx[i + 1] / lt2$lx[i + 1])
      contribution[i] <- direct + indirect_interaction
    } else {
      # Terminal open age interval: only a direct effect (nLx == Tx here).
      contribution[i] <- (lt1$lx[i] / l0_1) * (lt2$Tx[i] / lt2$lx[i] - lt1$Tx[i] / lt1$lx[i])
    }
  }

  tibble::tibble(age = lt1$age, contribution = contribution)
}

#' @noRd
decompose_pollard_one <- function(lt1, lt2) {
  l0_1 <- lt1$lx[1]
  l0_2 <- lt2$lx[1]

  contribution <- 0.5 * (lt1$mx - lt2$mx) *
    ((lt2$lx / l0_2) * lt1$ex + (lt1$lx / l0_1) * lt2$ex)

  tibble::tibble(age = lt1$age, contribution = contribution)
}

#' Decompose a difference in life expectancy at birth by age
#'
#' Attributes the difference in life expectancy at birth (`e0`) between
#' two life tables to age-specific contributions, using either Arriaga's
#' (1984) or Pollard's (1988) method. Arriaga's is an exact discrete
#' decomposition: `sum(contribution)` recovers `e2(0) - e1(0)` exactly.
#' Pollard's is exact only in the continuous limit; applied to a
#' single-year life table, `sum(contribution)` is a close but not exact
#' approximation of `e2(0) - e1(0)` - the approximation error grows with
#' how steeply mortality changes with age (a few percent of the total
#' gap under a fast-rising hazard is possible), so treat Arriaga's
#' (the default) as the primary result and Pollard's as a cross-check,
#' per Ponnapalli (2005)'s finding that the two methods' age patterns
#' agree closely without being identical. Use this to answer "how much
#' of the life-expectancy gap
#' between province A and B (or between year Y1 and Y2) comes from
#' mortality at each age?" - complementing [life_expectancy_summary()]/
#' [life_expectancy()], which report `e0`/`e65` but don't explain *why*
#' two values differ.
#'
#' @param lt1 A single life table (the "before"/reference table) - one
#'   province-year-sex slice of [build_life_tables()]'s `fltper`,
#'   `mltper`, or `bltper` (or [build_life_table()]'s direct output).
#'   Must have one row per age, sorted or unsorted, with columns `age`,
#'   `mx`, `lx`, `Lx`, `Tx`, `ex`.
#' @param lt2 The "after"/comparison life table, same shape as `lt1`,
#'   over the *same set of ages* as `lt1`.
#' @param method `"arriaga"` (default) or `"pollard"`.
#' @return A tibble with `age`, `contribution` (years of the `e2(0) -
#'   e1(0)` gap attributable to mortality differences at that age;
#'   positive means that age group contributed to a *gain* from `lt1` to
#'   `lt2`).
#' @examples
#' age <- 0:100
#' lt1 <- build_life_table(
#'   data.frame(age = age, mx = 0.0004 * exp(0.075 * age)), "female"
#' )
#' lt2 <- build_life_table(
#'   data.frame(age = age, mx = 0.0003 * exp(0.070 * age)), "female"
#' )
#' decomp <- decompose_life_expectancy(lt1, lt2)
#' sum(decomp$contribution)
#' lt2$ex[1] - lt1$ex[1]
#' @export
decompose_life_expectancy <- function(lt1, lt2, method = c("arriaga", "pollard")) {
  method <- match.arg(method)

  lt1 <- dplyr::arrange(lt1, .data$age)
  lt2 <- dplyr::arrange(lt2, .data$age)

  if (!identical(lt1$age, lt2$age)) {
    stop("`lt1` and `lt2` must cover exactly the same set of ages.")
  }

  if (method == "arriaga") decompose_arriaga_one(lt1, lt2) else decompose_pollard_one(lt1, lt2)
}
