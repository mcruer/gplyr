#' Check if a column exists in a dataframe or tibble
#'
#' This function determines if a specified column (passed as a symbol) exists in a given dataframe or tibble.
#'
#' @param df A dataframe or tibble.
#' @param column A symbol representing the column name to check.
#'
#' @return A logical value: `TRUE` if the column exists in the dataframe or tibble, and `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' library(tibble)
#'
#' df <- tibble(a = 1:5, b = 6:10)
#' is_in_df(df, a) # returns TRUE
#' is_in_df(df, z) # returns FALSE
#' }
#'
#' @export
is_in_df <- function(df, column) {

  # Check if df is a data frame or tibble
  if (!is.data.frame(df)) {
    stop("The first argument must be a data frame or tibble.")
  }

  column_string <- rlang::as_name(rlang::enquo(column))
  column_string %in% names(df)
}
