mutate_case_when_step <- function(df, target_content, new_column_content, target_column, new_column_name) {
  df %>%
    dplyr::mutate({{new_column_name}} := dplyr::case_when(
      stringr::str_detect({{target_column}}, target_content) & is.na(new_column_content) ~ {{target_column}},
      stringr::str_detect({{target_column}}, target_content) ~ new_column_content,
      TRUE ~ {{new_column_name}}
    ))
}

filter_out_target <- function(df, target, column) {
  df %>%
    filter_out({{column}}, target)
}

#' Add a New Category Column to a Dataframe Based on Target Content
#'
#' This function adds a new column to a dataframe, populating it based on matching
#' content in a target column. It can also drop rows that match specified content.
#'
#' @param df A dataframe to which the new category column will be added.
#' @param target_column The name of the column in the dataframe where the content
#' matching will occur. This column is used as the basis for determining the new
#' category column's content.
#' @param target_content A character vector specifying the content to match in the
#' `target_column`. Each element represents a pattern to match.
#' @param new_column_content (Optional) A character vector specifying the new
#' content to add to the `new_column_name` based on matching the `target_content`.
#' If not provided, the default behavior is to insert NAs.
#' @param new_column_name (Optional) The name of the new category column to be added
#' to the dataframe. Default is 'category'.
#' @param drop_rows (Optional) Logical indicating whether rows that match the
#' `target_content` should be dropped. Default is FALSE.
#'
#' @return A dataframe with the added (or modified) category column.
#'
#' @examples
#' \dontrun{
#' tibble(a = c("hi", "here", "hi", "bye", "target2", "other")) %>%
#'   add_category(a, c("here", "target2"),
#'                c("One", "Two"),
#'                drop_rows = TRUE)
#' }
#'
#' @export
add_category <- function(df, target_column, target_content, new_column_content = NULL, new_column_name = category, drop_rows = FALSE) {

  if(is.null(new_column_content)) {
    new_column_content <- NA_character_
    length(new_column_content) <- length(target_content)
  }

  # Error handling for target_column
  if(!deparse(substitute(target_column)) %in% names(df)) {
    stop(paste("The target_column", deparse(substitute(target_column)), "does not exist in the dataframe."))
  }

  if(length(target_content) != length(new_column_content)) {
    stop("The length of target_content and new_column_content must be the same.")
  }

  if(deparse(substitute(new_column_name)) %in% names(df)) {
    warning(paste("The column", deparse(substitute(new_column_name)), "already exists in the dataframe. It will be overwritten."))
  }


  df <- df %>%
    dplyr::mutate({{new_column_name}} := NA_character_, .before = 1) %>%
    listful::build2(target_content, new_column_content, mutate_case_when_step, {{target_column}}, {{new_column_name}}) %>%
    tidyr::fill({{new_column_name}}, .direction = "down")

  if(drop_rows){
    df <- df %>%
      listful::build(target_content, filter_out_target, {{target_column}})
  }

  df
}

