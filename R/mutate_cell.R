#' Modify a specific cell in a data frame
#'
#' This function allows for the modification of a specific cell in a data frame
#' by specifying the column name, row number, and new value.
#'
#' @param df A data frame that contains the cell to be modified.
#' @param column The name of the column (unquoted) that contains the cell to be modified.
#' @param row_number The row number of the cell to be modified.
#' @param new_value The new value to assign to the specified cell.
#'
#' @return A data frame with the specified cell modified to the new value.
#' The original data frame remains unchanged.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(A = c(1, 2, 3), B = c("a", "b", "c"))
#' mutate_cell(data, A, 2, 100)
#' }
#'
#' @export
mutate_cell <- function (df, column, row_number, new_value) {
  df[[deparse(substitute(column))]][[row_number]] <- new_value
  df

}
