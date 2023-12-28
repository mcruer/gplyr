#' Reparses everything in a data frame
#'
#' @param df data frame
#'
#' @return a data frame
#' @export
#'
#' @examples
#' parse_guess_all (tibble::tibble (a = "1", b = "a", c = 5))
parse_guess_all <- function (df) {
  df %>% purrr::modify (~.x %>% as.character %>% readr::parse_guess())
}

#' Review NA Values in a Data Frame
#'
#' This function provides a summary review of NA values in a data frame. It returns a data frame
#' that includes the class of each column, the total row count, the count and percentage of NA
#' values for each column. It utilizes non-standard evaluation to allow for the direct
#' use of unquoted column names.
#'
#' @param df A data frame object to be reviewed.
#' @param ... use tidyselect to select columns. Defaults to all columns.
#'
#' @return A data frame with each input column's class, row count, count of NA values, and
#' percentage of NA values. If a column has multiple classes, only the first is listed.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Assuming 'my_data' is a data frame with columns 'a', 'b', and 'c'.
#'   na_review_result <- na_review(my_data, a, b, c)
#'   print(na_review_result)
#' }
na_review <- function (df, ...) {
  col.names <- select_cols_default_all(df, ...)
  df <- dplyr::select(df, dplyr::all_of(col.names))
  output <- dplyr::bind_rows(purrr::map_df(df, ~.x %>% class() %>% magrittr::extract(1)),
                             purrr::map_df(df, ~.x %>% length() %>% as.character()),
                             purrr::map_df(df, ~.x %>% is.na() %>% sum() %>% as.character()),
                             purrr::map_df(df, ~{
                               scales::percent(mean(is.na(.x)))
                             }),
  )
  output <- dplyr::bind_cols(`NA Review` = c("Class", "Row Count",
                                             "NAs (Count)", "NAs (%)"), output)
  output
}

add_rows <- function (df, new.rows, total.rows = NULL) {
  if (is.null(total.rows)){
    rows.to.add <- new.rows
  }else{
    rows.to.add <- total.rows - nrow(df)
  }
  first.col <- names(df)[1]
  df %>%
    dplyr::bind_rows(tibble::tibble(.col.to.drop.x4eajkl44902 = rep(NA, rows.to.add))) %>%
    dplyr::select(-.col.to.drop.x4eajkl44902)

}


#' Count Occurrences of Each Unique Value in All Columns
#'
#' This function applies a count operation to every column in the provided data frame. It
#' counts the occurrences of each unique value in each column, including `NA` values.
#' The result is a tibble with each column's unique values and their corresponding counts.
#'
#' @param df A data frame for which to count occurrences of unique values in all columns.
#'
#' @return A tibble where each pair of columns represents the unique values and their counts
#' from a column in the input data frame. The first of each pair is the unique values,
#' and the second is the counts. Each pair of columns is named after the original column,
#' with the count columns having '_counts' appended.
#'
#' @export
#' @examples
#' \dontrun{
#'   data(mtcars)
#'   # Assuming 'mtcars' is a data frame loaded in the environment.
#'   count_results <- count_all(mtcars)
#'   print(count_results)
#' }
count_all <- function(df) {
  count_vector <- function(v, v_name) {
    # Get unique values excluding NA
    unique_values <- unique(v[!is.na(v)])

    # Count occurrences of non-NA values
    counts <- purrr::map_int(unique_values, ~ sum(v == .x, na.rm = TRUE))

    # Count NA values
    na_count <- sum(is.na(v))

    # Combine NA count and other counts
    counts <- c(na_count, counts)

    # Combine NA with other unique values
    unique_values <- c(NA, unique_values)

    tib <- tibble::tibble(
      values = unique_values,
      counts = counts
    ) %>%
      dplyr::arrange (!is.na(unique_values), dplyr::desc(counts))

    list(tib$values, tib$counts) %>%
      purrr::set_names(c(v_name, stringr::str_c(v_name, "_", "counts")))
  }


  set_length_to_the_max <- function(list) {
    max_length <- list %>%
      purrr::map_int(length) %>%
      max()

    list %>%
      purrr::map(`length<-`, max_length)
  }


  suppressWarnings(
    df %>%
      purrr::imap(count_vector) %>%
      purrr::flatten() %>%
      set_length_to_the_max() %>%
      tibble::as_tibble()
  )
}


#' Rename Columns to x1, x2, x3, etc.
#'
#' This function renames all columns of a given dataframe to a sequence starting with "x"
#' followed by the column index. It uses `stringr::str_c` to create the new column names
#' and `rlang::set_names` for setting these names.
#'
#' @param df A dataframe whose columns need to be renamed.
#'
#' @return A dataframe with renamed columns. The names of the columns will be "x1", "x2", "x3",
#'         etc., corresponding to their original position in the dataframe.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Example dataframe
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#'
#' # Rename columns
#' df_renamed <- rename_x(df)
#' print(df_renamed)
#'
#' # Output:
#' #   x1 x2 x3
#' # 1  1  4  7
#' # 2  2  5  8
#' # 3  3  6  9
#' }
#'
#' @export
rename_x <- function (df) {
  names <- stringr::str_c("x", 1:ncol(df))
  df %>%
    rlang::set_names(names)
}
