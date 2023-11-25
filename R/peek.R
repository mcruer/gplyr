#' Peek at Transformed Object
#'
#' This function takes an object, applies a function to it, and prints the result, all while returning the original object.
#'
#' @param obj An object to be transformed.
#' @param .f A function to apply to the object. Default is identity function.
#' @param ... Additional arguments to pass to `.f`.
#'
#' @return The original object, returned invisibly.
#'
#' @importFrom purrr as_mapper
#' @importFrom utils head
#' @examples
#' \dontrun{
#' library(magrittr)
#' mtcars %>% peek(~ .x %>% head())
#' }
#' @export
peek <- function (obj, .f = ~.x, ...) {
  .f <- purrr::as_mapper(.f)
  transformed_obj <- .f(obj, ...)

  print(transformed_obj, n = 3)

  invisible(obj)
}

#' Peek at Transformed Data Frame with View
#'
#' This function takes a data frame, applies a function to it, and opens the result in the RStudio viewer, all while returning the original data frame.
#'
#' @param df A data frame to be transformed.
#' @param .f A function to apply to the data frame. Default is identity function.
#' @param ... Additional arguments to pass to `.f`.
#'
#' @return The original data frame, returned invisibly.
#'
#' @importFrom purrr as_mapper
#' @importFrom utils head
#' @importFrom tibble view
#' @examples
#' \dontrun{
#' library(magrittr)
#' mtcars %>% peek_v(~ .x %>% head())
#' }
#' @export
peek_v <- function (df, .f = ~.x, ...) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }

  .f <- purrr::as_mapper(.f)
  transformed_df <- .f(df, ...)

  tibble::view(transformed_df)

  invisible(df)
}


#' Peek at Transformed Object with Row Limit
#'
#' This function takes an object, applies a function to it, and prints the result up to a specified number of rows, all while returning the original object.
#'
#' @param obj An object to be transformed.
#' @param .f A function to apply to the object. Default is identity function.
#' @param n The maximum number of rows to print. Default is 20.
#' @param ... Additional arguments to pass to `.f`.
#'
#' @return The original object, returned invisibly.
#'
#' @importFrom purrr as_mapper
#' @importFrom utils head
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' mtcars %>% peek_n(~ .x %>% head(), n = 10)
#' }
#' @export
peek_n <- function (obj, .f = ~.x, ..., n = 40) {
  .f <- purrr::as_mapper(.f)
  transformed_obj <- .f(obj, ...)

  print(transformed_obj, n = n)

  invisible(obj)
}
