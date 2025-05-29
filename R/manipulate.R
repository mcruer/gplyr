utils::globalVariables("index")

#' Quickly apply a function to multiple columns in a data frame
#'
#' @param df A data frame or tibble.
#' @param .cols A selection of columns where the function will be applied.
#' @param .f Function to be applied across the selected columns. Default is the identity function `~.x`.
#' @param ... Additional arguments to be passed to the function `.f`.
#'
#' @return A modified data frame with the function applied to the specified columns.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gplyr)
#' library(dplyr)
#'
#' df <- tibble(a = 1:3, b = 4:6, c = 7:9)
#' df_new <- quickm(df, .cols = c("a", "b"), .f = ~ .x * 2)
#' }
quickm <- function (df, .cols, .f = ~.x ,  ...) {
  .f <- as_mapper(.f)

  df %>%
    dplyr::mutate(dplyr::across({{.cols}}, .fns = .f, .args = ...))
}

#' Convert specified columns to numeric type
#'
#' This function uses `quickm` to convert the specified columns to numeric type.
#'
#' @param df A data frame or tibble.
#' @param .cols A selection of columns where the conversion will be applied. Defaults to all columns.
#'
#' @return A modified data frame with the specified columns converted to numeric type.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gplyr)
#' library(dplyr)
#'
#' df <- tibble(a = c("1", "2", "3"), b = c("4", "5", "6"))
#' df_new <- to_number(df)
#' }
to_number <- function (df, .cols=dplyr::everything()){
  df %>%
    quickm({{.cols}}, as.numeric)
}

#' Convert specified columns to character type
#'
#' This function uses `quickm` to convert the specified columns to character type.
#'
#' @param df A data frame or tibble.
#' @param .cols A selection of columns where the conversion will be applied. Defaults to all columns.
#'
#' @return A modified data frame with the specified columns converted to character type.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gplyr)
#' library(dplyr)
#'
#' df <- tibble(a = 1:3, b = 4:6)
#' df_new <- to_character(df)
#' }
to_character <- function (df, .cols = dplyr::everything()){
  df %>%
    quickm({{.cols}}, as.character)
}


#' Quickly summarize multiple columns in a data frame
#'
#' @param df A data frame or tibble.
#' @param .cols A selection of columns to which the function will be applied.
#' @param .f Function to be applied across the selected columns. Default is the identity function `~.x`.
#' @param ... Additional arguments to be passed to the function `.f`.
#'
#' @return A summarized data frame with the function applied to the specified columns.
#' @export
#'
#' @examples
#' \dontrun{
#' library(gplyr)
#' library(dplyr)
#'
#' df <- tibble(a = 1:3, b = 4:6, c = 7:9)
#' summary_df <- quicks(df, .cols = c("a", "b"), .f = mean)
#' }
quicks <- function (df, .cols, .f = ~.x, ...) {

  args <- list(...)

  anon_f <- function(x, ...) {
    do.call(.f, c(list(x), args))
  }

  df %>%
    dplyr::summarise(dplyr::across({{.cols}}, .fns = anon_f))
}


#' Merge NA values from a target column with another column
#'
#' This function replaces NA values in a target column with values from another specified column.
#'
#' @param df Data frame or tibble to manipulate.
#' @param target The name of the target column with NA values.
#' @param use_if_na The name of the column to use for replacing NA values.
#' @param drop_col Logical indicating whether to drop the 'use_if_na' column. Defaults to TRUE.
#'
#' @importFrom rlang `:=`
#' @return A data frame or tibble with NA values in the target column replaced.
#' @export
merge_if_na <- function(df, target, use_if_na, drop_col = TRUE) {
  df <- df %>%
    dplyr::mutate({{target}} := dplyr::if_else(is.na({{target}}), {{use_if_na}}, {{target}}))

  if (drop_col) {
    df <- df %>%
      dplyr::select(-{{use_if_na}})
  }

  df
}


#' Add an index column to a data frame or tibble
#'
#' This function adds an index column to a data frame or tibble.
#' The index starts from 1 and increments by 1 for each row.
#' If the data is grouped, the index will be relative to the group.
#'
#' @param df A data frame or tibble to which the index will be added.
#'
#' @param col_name The name to assign to the new index column. Defaults to 'index'.
#'
#' @return A data frame or tibble with the added index column.
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' # Adding index to an ungrouped tibble
#' tibble(a = c(3, 3, 3, 2, 2, 3)) %>%
#'   add_index()
#'
#' # Adding index to a grouped tibble
#' tibble(a = c(3, 3, 3, 2, 2, 3)) %>%
#'   group_by(a) %>%
#'   add_index(col_name = "group.index")
#'
#' # Changing the name of the index column
#' tibble(a = c(3, 3, 3, 2, 2, 3)) %>%
#'   add_index(col_name = "row_id")
#'   }
add_index <- function (df, col_name = index) {
  df <- dplyr::mutate(df, {{col_name}} := dplyr::row_number(), .before = 1)
  df
}

#' Left Join Without Column Name Collisions
#'
#' Performs a left join between two data frames while automatically removing overlapping columns
#' from the second data frame (excluding those used for joining), to avoid `.x` / `.y` suffixes.
#'
#' @param df1 A data frame or tibble. The left-hand side of the join.
#' @param df2 A data frame or tibble. The right-hand side of the join. Any columns that also
#'   exist in \code{df1} (but are not part of the join keys) will be removed before the join.
#' @param by A character vector specifying the column(s) to join by. Passed directly to \code{left_join()}.
#' @param quiet Logical. If \code{FALSE} (default), prints informative messages about the join and dropped columns.
#'
#' @return A tibble resulting from the join, without conflicting column names.
#' @importFrom dplyr left_join select all_of
#' @importFrom rlang inform
#' @export
#'
#' @examples
#' \dontrun{
#' df1 <- tibble(id = 1:3, value = c("A", "B", "C"))
#' df2 <- tibble(id = 2:4, value = c("X", "Y", "Z"), extra = c(10, 20, 30))
#'
#' left_join_unique(df1, df2, by = "id")
#' }
left_join_unique <- function(df1, df2, by, quiet = FALSE) {
  cols_to_remove <- setdiff(intersect(names(df1), names(df2)), by)

  if (!quiet && length(cols_to_remove) > 0) {
    inform(paste0("Removing overlapping columns from df2: ", toString(cols_to_remove)))
  }

  df2_trimmed <- df2 %>% select(-all_of(cols_to_remove))

  if (!quiet) {
    inform(paste0("Joining with `by = ", toString(by), "`"))
  }

  left_join(df1, df2_trimmed, by = by)
}
