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

#' Row-wise Mutation of Data Frame
#'
#' This function performs a row-wise operation on selected columns of a data
#' frame and creates a new column with the results.
#'
#' @param df A data frame or tibble to be manipulated.
#' @param new_col_name A symbol indicating the name of the new column to be created.
#' @param cols A tidy-select expression to select columns for row-wise operation.
#' @param fun A function or formula to apply to each row on selected columns.
#' @param ... Additional arguments to be passed to the function `fun`.
#'
#' @return A data frame with the new column added.
#' @export
#'
#' @examples
#' \dontrun{
#'   tibble(a1 = 1:5, a2 = 6:10) %>%
#'     mutate_rowwise(new_column, starts_with("a"), sum)
#' }
mutate_rowwise <- function(df, new_col_name, cols, fun, ...) {

  # Ensure df is a data frame
  if (!is.data.frame(df)) {
    stop("The first argument must be a data frame.")
  }

  # Convert new_col_name to a symbol (ensures it works with both strings and symbols)
  new_col_name_sym <- rlang::ensym(new_col_name)

  # Check if the new column name already exists in the dataframe
  if (as.character(new_col_name_sym) %in% names(df)) {
    stop("new_col_name already exists in the data frame.")
  }

  # Capture existing groups
  group_vars <- dplyr::group_vars(df)

  # Convert function if it's a formula
  if (inherits(fun, "formula")) {
    fun <- rlang::as_function(fun)
  } else if (!is.function(fun)) {
    stop("fun must be a function or a formula.")
  }

  # Select columns based on tidy_select
  cols_selected <- rlang::enquos(cols)

  # Check if cols results in a non-empty selection
  if (length(cols_selected) == 0) {
    stop("No columns selected. Please select at least one column.")
  }

  # Perform row-wise mutation and then restore original grouping
  df %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(!!new_col_name_sym := fun(dplyr::c_across(!!!cols_selected), ...)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!rlang::syms(group_vars), .add = TRUE)
}
