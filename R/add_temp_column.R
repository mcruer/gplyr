#' Add a Temporary Column Name
#'
#' Generates a unique temporary column name for a given dataframe `df`.
#' The function starts with a base name of "temp" and appends a number to it
#' if "temp" already exists as a column name. The number is incremented until a unique
#' column name is found.
#'
#' @param df A dataframe to which the temporary column name will be added.
#'
#' @return A character string representing the unique temporary column name.
#'
#' @examples
#' \dontrun{
#' df_example <- tibble(a = 1:3, temp = 4:6, temp1 = 7:9)
#' add_temp_column(df_example)
#' }
#'
#' @export
#'
add_temp_column <- function (df) {
  temp_column <- "temp"
  num <- 1
  while(temp_column %in% names(df)){
    temp_column <- stringr::str_c(temp_column, num)
  }

  temp_column

}
