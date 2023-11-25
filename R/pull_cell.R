#' Extract a Specific Cell from a Dataframe
#'
#' This function extracts the value of a specific cell from a dataframe based on
#' the given column and row.
#'
#' @param df A dataframe from which the cell value will be extracted.
#' @param column The column name or index where the desired cell is located.
#' @param row The row number of the desired cell.
#'
#' @return The value of the cell at the specified column and row.
#'
#' @examples
#' \dontrun{
#' df_example <- tibble(a = 1:5, b = letters[1:5])
#' pull_cell(df_example, "a", 3) # Returns 3
#' pull_cell(df_example, "b", 4) # Returns "d"
#' }
#'
#' @export
#'
pull_cell <- function (df, column, row = 1){
  df %>%
    dplyr::ungroup() %>%
    dplyr::pull({{column}}) %>%
    magrittr::extract2(row)
}
