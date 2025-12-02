#' Mutate Columns Inside a Nested Tibble (List-Column)
#'
#' @description
#' `nested_mutate()` applies a column-wise transformation inside each tibble
#' stored in a list-column. This is a convenient wrapper around `purrr::map()`
#' and `dplyr::across()` that allows you to mutate selected columns *within*
#' nested tibbles using tidyselect syntax.
#'
#' This is useful in workflows where list-columns created by `tidyr::nest()`
#' contain structured data requiring internal mutation.
#'
#' @param df A data frame containing at least one list-column of tibbles.
#' @param list_col The list-column containing nested tibbles. Must be supplied
#'   as an unquoted column name.
#' @param .cols A tidyselect expression identifying which columns *inside each
#'   nested tibble* should be mutated. Defaults to `dplyr::everything()`.
#' @param .fn A function or purrr-style formula to apply to the selected columns.
#'   May be supplied as a bare function (e.g., `as.character`) or a formula
#'   (e.g., `~ stringr::str_trim(.x)`).
#' @param ... Additional arguments passed to `.fn` if applicable.
#'
#' @return
#' A data frame identical to `df`, except that the nested tibbles in `list_col`
#' have been updated by applying `.fn` to the selected `.cols`.
#'
#' @details
#' Inside each nested tibble, `nested_mutate()` calls `quickm()` with the provided
#' column selection and transformation function. Column selection and mutation
#' occur independently within each nested tibble.
#'
#' The function is designed for internal data engineering workflows that make
#' extensive use of list-columns and nested tibbles.
#'
#' @importFrom dplyr mutate everything
#' @importFrom purrr map
#' @importFrom rlang ensym as_function
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(tibble)
#' library(tidyselect)
#'
#' df <- tibble(
#'   id = 1:2,
#'   data = list(
#'     tibble(a = 1, b = 2),
#'     tibble(a = NA, b = 5)
#'   )
#' )
#'
#' # Convert column `a` inside each nested tibble to character
#' df %>%
#'   nested_mutate(data, a, as.character)
#'
#' # Mutate multiple nested columns using tidyselect
#' df %>%
#'   nested_mutate(data, tidyselect::matches("a|b"), ~ .x * 10)
#' }
#'
#' @export
nested_mutate <- function(df, list_col, .cols = everything(), .fn = ~ .x, ...) {
  .fn <- as_function(.fn)
  list_col <- ensym(list_col)

  df %>%
    mutate(
      {{ list_col }} := map(
        {{ list_col }},
        ~ {
          quickm(.x, {{ .cols }}, .f = .fn)
        }
      )
    )
}
