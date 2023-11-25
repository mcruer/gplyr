#' Sum values with removal of NA values
#'
#' This function sums all the provided values, automatically removing any NA values.
#' It is a simple wrapper around the base R `sum` function with `na.rm = TRUE` set by default.
#'
#' @param ... Numeric values or numeric vectors to be summed.
#'
#' @return The sum of all provided values with NA values removed.
#'
#' @examples
#' \dontrun{
#' sum_rna(1, 2, 3, NA, 5)  # Returns 11
#' sum_rna(c(1, 2, NA, 4), c(NA, 6))  # Returns 13
#' }
#'
#' @export
sum_rna <- function (...){
  sum(..., na.rm = TRUE)
}
