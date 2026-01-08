#' Internal Function to Select Columns, with Default to All
#'
#' This internal function returns a character vector of selected column names.
#'
#' @param .df A dataframe or tibble
#' @param ... Columns to select, defaulting to all columns if none are specified
#' @param include_grouped Logical, should grouped columns be included? Defaults to FALSE.
#' @importFrom magrittr %>%
#'
#' @return A character vector of column names
select_cols_default_all <- function(.df, ..., include_grouped = FALSE) {
  if (missing(...)) {
    cols <- names(.df)
  } else {
    cols <- rlang::ensyms(...)
    cols <- purrr::map_chr(cols, rlang::as_string)
  }

  if (include_grouped) {
    grouped_vars <- dplyr::group_vars(.df)
    # Reorder to keep grouped variables at their original positions
    cols <- unique(c(cols, grouped_vars))
    cols <- cols[order(match(cols, names(.df)))]
  }

  return(cols)
}


#' Filter Out Rows Containing NA in Specified Columns
#'
#' This function removes rows from the data where specified columns contain NA values.
#'
#' @param .df A dataframe or tibble
#' @param ... Columns to check for NA values
#' @param if_any_or_all Should the row be removed if any or all selected columns contain NA? Defaults to "if_all".
#'
#' @return A tibble with rows containing NA in specified columns removed
#' @export
filter_out_na <- function (.df, ..., if_any_or_all = "if_all") {
  col.names <- .df %>% dplyr::ungroup() %>% dplyr::select(...) %>% names()
  groups <- dplyr::group_vars(.df)
  if (if_any_or_all == "if_any") {
    .df %>%
      dplyr::ungroup () %>%
      dplyr::filter (dplyr::if_all (dplyr::all_of (col.names), ~ !is.na(.x))) %>%
      dplyr::group_by(dplyr::across (dplyr::all_of(groups)))
  } else if (if_any_or_all == "if_all") {
    .df %>%
      dplyr::ungroup () %>%
      dplyr::filter (dplyr::if_any (dplyr::all_of (col.names), ~ !is.na(.x))) %>%
      dplyr::group_by(dplyr::across (dplyr::all_of(groups)))
  }
}

#' Filter In Rows Containing NA in Specified Columns
#'
#' This function keeps rows from the data where specified columns contain NA values.
#'
#' @param .df A dataframe or tibble
#' @param ... Columns to check for NA values
#' @param if_any_or_all Should the row be kept if any or all selected columns contain NA? Defaults to "if_all".
#'
#' @return A tibble with only rows containing NA in specified columns
#' @export
filter_in_na <- function (.df, ..., if_any_or_all = "if_all") {
  col.names <- .df %>% dplyr::ungroup() %>% dplyr::select(...) %>% names()
  #col.names <- select_cols_default_all(.df, ..., include_grouped = FALSE)
  groups <- dplyr::group_vars(.df)
  if (if_any_or_all == "if_any") {
    .df %>%
      dplyr::ungroup() %>%
      dplyr::filter (dplyr::if_any (dplyr::all_of (col.names), ~ is.na(.x))) %>%
      dplyr::group_by(dplyr::across (dplyr::all_of(groups)))
  } else if (if_any_or_all == "if_all") {
    .df %>%
      dplyr::ungroup() %>%
      dplyr::filter (dplyr::if_all (dplyr::all_of (col.names), ~ is.na(.x))) %>%
      dplyr::group_by(dplyr::across (dplyr::all_of(groups)))
  }
}

#' Filter rows in a data frame based on string matching in a column
#'
#' @param df A data frame or tibble.
#' @param col The column to filter on.
#' @param string The string to look for.
#' @param ignore_case Whether to ignore case (default is TRUE).
#' @param drop.col Whether to remove the column that was filtered on (default is FALSE).
#' @param negate Whether to keep or remove rows that match the string (default is FALSE).
#' @param na.rm Whether to remove NA values (default is FALSE).
#' @return A filtered data frame.
#' @export
filter_str <- function(df, col, string, ignore_case = TRUE, drop.col = FALSE, negate = FALSE, na.rm = FALSE) {

  if (na.rm) {
    df <- df %>% dplyr::filter(stringr::str_detect({{col}}, stringr::regex(string, ignore_case = ignore_case), negate = negate))
  } else {
    df <- df %>% dplyr::filter(stringr::str_detect({{col}}, stringr::regex(string, ignore_case = ignore_case), negate = negate) | is.na({{col}}))
  }

  if (drop.col) {
    return(df %>% dplyr::select(-{{col}}))
  }

  return(df)
}

#' Filter rows containing a specific string in a given column
#'
#' This function filters rows where the specified column contains the given string.
#'
#' @inheritParams filter_str
#' @return A filtered data frame.
#' @examples \dontrun{
#' library(dplyr)
#' tibble(x = c("apple", "banana", "cherry")) %>%
#'   filter_in(col = x, string = "app")
#' }
#' @export
filter_in <- function(df, col, string, ignore_case = TRUE, drop.col = FALSE, na.rm = FALSE) {
  filter_str(df = df, col = {{col}}, string = string, ignore_case = ignore_case, drop.col = drop.col, negate = FALSE, na.rm = na.rm)
}

#' Filter out rows containing a specific string in a given column
#'
#' This function filters out rows where the specified column contains the given string.
#'
#' @inheritParams filter_str
#' @return A filtered data frame.
#' @examples \dontrun{
#' library(dplyr)
#' tibble(x = c("apple", "banana", "cherry")) %>%
#'   filter_out(col = x, string = "app")
#' }
#' @export
filter_out <- function(df, col, string, ignore_case = TRUE, drop.col = FALSE, na.rm = FALSE) {
  filter_str(df = df, col = {{col}}, string = string, ignore_case = ignore_case, drop.col = drop.col, negate = TRUE, na.rm = na.rm)
}

#' Filter Out Numeric Values from Selected Columns
#'
#' This function filters a dataframe to retain rows where the selected columns contain non-numeric values.
#' It can optionally remove rows where the selected columns are NA.
#'
#' @param df A dataframe to be filtered.
#' @param .cols Columns to check for non-numeric values; defaults to all columns.
#' @param na.rm Logical; if TRUE, rows where the selected columns are NA are excluded.
#'
#' @return A dataframe with rows containing non-numeric values in the specified columns.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom dplyr everything
#' @export
#'
#' @examples
#' # Example dataframe
#' df <- data.frame(
#'   a = c("1", "2", "x", NA),
#'   b = c("y", "2", "3", "4")
#' )
#'
#' # Filter out rows with numeric values in all columns
#' filter_out_numeric(df)
#'
#' # Filter out rows with numeric values in column 'a', ignoring NAs
#' filter_out_numeric(df, .cols = a, na.rm = TRUE)
filter_out_numeric <- function (df, .cols = dplyr::everything(), na.rm = FALSE) {
  is_non_numeric <- function(x) {
    suppressWarnings(is.na(as.numeric(x)))
  }

  if (na.rm) {
    df <- df %>%
      dplyr::filter(dplyr::if_any({{.cols}}, ~ is_non_numeric(.x) & !is.na(.x)))
  } else {
    df <- df %>%
      dplyr::filter(dplyr::if_any({{.cols}}, is_non_numeric))
  }

  df
}
