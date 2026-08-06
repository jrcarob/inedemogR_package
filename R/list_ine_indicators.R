#' List Available INE Indicators
#'
#' Returns a data frame of available demographic indicators.
#'
#' @return A data frame with indicator codes, names, and descriptions.
#' @examples
#' list_ine_indicators()
#' @export
list_ine_indicators <- function() {
  ine_variables
}
