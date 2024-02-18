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
#'   tibble(a = c("hi", "here", "hi", "bye", "target2", "other")) %>%
#'     add_category(a, c("here", "target2"),
#'                  c("One", "Two"),
#'                  drop_rows = TRUE)
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


#' Split a dataframe into a list of dataframes based on unique values in a specified column.
#'
#' @param df A dataframe to split.
#' @param col Column name based on which to split the dataframe.
#' @param drop_col Indicates whether to drop the splitting column in the resultant dataframes.
#' @return A named list of dataframes, each corresponding to a unique value in `col`.
#' @export
#' @examples
#' \dontrun{
#'   data <- data.frame(group = c("A", "B", "A"), value = 1:3)
#'   list_of_dfs <- listify_df(data, group)
#' }
#'
listify_df <- function(df, col, drop_col = TRUE) {
  na_values_count <- df %>%
    dplyr::ungroup() %>%
    dplyr::select({{col}}) %>%
    filter_in_na({{col}}) %>%
    nrow()

  if(na_values_count > 0) {
    stop("The target column cannot contain NA values.")
  }

  keys <- df %>%
    dplyr::pull({{col}}) %>%
    unique ()

  list_out <- purrr::map(keys, ~{
    filtered_df <- df %>%
      dplyr::filter({{col}} == .x)

    if (drop_col) {
      filtered_df <- filtered_df %>%
        dplyr::select(-{{col}})
    }

    return(filtered_df)
  })
  names(list_out) <- keys
  list_out
}


#' Apply a Function to Corresponding Columns of Two Dataframes
#'
#' This function applies a user-defined function (.f) to corresponding columns
#' of two dataframes (df1 and df2) after selecting columns based on dplyr-style
#' selection arguments (cols).
#'
#' @param df1 A dataframe to which the function will be applied.
#' @param df2 Another dataframe to which the function will be applied.
#' @param .f A function or formula to apply to corresponding columns of df1 and df2.
#' @param cols A selection specification of columns to which the function will be applied.
#'             This can be standard column names, helper functions like starts_with(),
#'             or conditional checks like where(is.numeric). Defaults to everything().
#' @param ... Additional arguments to pass to .f.
#' @return A dataframe with the same columns as specified by cols,
#'         each column being the result of applying .f to the corresponding
#'         columns of df1 and df2.
#' @examples
#' \dontrun{
#' df1 <- data.frame(a = 1:3, b = 4:6)
#' df2 <- data.frame(a = 7:9, b = 10:12)
#' df_map(df1, df2, ~.x + .y)
#' df_map(df1, df2, ~.x * .y, cols = starts_with("b"))
#' }
#' @export
#'
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom purrr map2_dfc
df_map <- function(df1, df2, .f, cols = dplyr::everything(), ...) {
  if (!is.data.frame(df1) | !is.data.frame(df2)) {
    stop("Input 'df' must be a dataframe or tibble.")
  }

  # Applying dplyr-style selection to both dataframes
  df1_selected <- df1 %>% dplyr::select({{ cols }})
  df2_selected <- df2 %>% dplyr::select({{ cols }})

  # Aligning columns of df2_selected to those of df1_selected
  df2_selected <- df2_selected %>% dplyr::relocate(dplyr::any_of(names(df1_selected)))

  if (!identical(names(df1_selected), names(df2_selected))) {
    stop("The selected columns in the dataframes do not match.")
  }

  if (!inherits(.f, "function") && !inherits(.f, "formula")) {
    stop("Input '.f' must be a function or a formula.")
  }

  additional_args <- list(...)

  # Applying the function column-wise
  results <- purrr::map2_dfc(df1_selected, df2_selected, .f, ...)

  results
}

