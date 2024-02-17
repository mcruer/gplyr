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


#' Apply a Function Columnwise to Two List-Columns in a Data Frame
#'
#' This function takes a data frame and two list-columns containing tibbles
#' with identical structures. It applies a specified function to corresponding
#' columns in these tibbles and returns a new list-column containing the results.
#'
#' @param df A data frame containing the list-columns to be processed.
#' @param col1 The name of the first list-column (as a symbol or string).
#' @param col2 The name of the second list-column (as a symbol or string).
#' @param .f The function to apply to corresponding columns of the list-columns.
#'           This function should take at least two arguments.
#' @param output_col The name of the output list-column (default is "result").
#' @param ... Additional arguments to pass to the function .f.
#'
#' @return A data frame with an additional list-column containing the results
#'         of applying .f to corresponding columns of col1 and col2.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' example_df <- tibble(
#'   ID = 1:3,
#'   events = list(tibble(col1 = 1:3, col2 = 4:6)),
#'   pt = list(tibble(col1 = 2:4, col2 = 5:7))
#' )
#' result <- apply_columnwise(example_df, events, pt, function(x, y) x - y)
#' }
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map2
#' @importFrom rlang ensym
#' @importFrom tibble as_tibble
apply_columnwise <- function(df, col1, col2, .f, output_col = "result", ...) {
  if (!is.data.frame(df)) {
    stop("Input 'df' must be a dataframe or tibble.")
  }

  col1_sym <- rlang::ensym(col1)
  col2_sym <- rlang::ensym(col2)

  if (!all(c(rlang::as_string(col1_sym), rlang::as_string(col2_sym)) %in% names(df))) {
    stop("Specified columns are not in the dataframe.")
  }

  # Check if the two columns contain identical variable names
  col1_names <- names(dplyr::pull(df, !!col1_sym)[[1]])
  col2_names <- names(dplyr::pull(df, !!col2_sym)[[1]])
  if (!identical(col1_names, col2_names)) {
    stop("The specified columns do not contain identical variable names.")
  }

  if (!is.function(.f)) {
    stop("Input '.f' must be a function.")
  }

  additional_args <- list(...)

  dplyr::mutate(df, {{ output_col }} := purrr::map2(!!col1_sym, !!col2_sym, ~ {
    cols <- names(.x)
    results <- vector("list", length(cols))

    for (i in seq_along(cols)) {
      results[[i]] <- do.call(.f, c(list(.x[[cols[i]]], .y[[cols[i]]]), additional_args))
    }
    suppressMessages(
      tibble::as_tibble(results, .name_repair = "universal") %>%
        set_names(cols)
    )
  }))
}


