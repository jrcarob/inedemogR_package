#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
#' @importFrom rlang .env
#' @importFrom rlang %||%
## usethis namespace: end
NULL

# ine_variables is a lazy-loaded package dataset (see data-raw/ine_variables.R
# and R/data.R); referencing it by name inside functions is not NSE and does
# not need a variable binding, but R CMD check cannot tell the difference.
utils::globalVariables("ine_variables")
