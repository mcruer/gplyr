#' Pipe and Assign
#'
#' This function takes an object, applies a function to it, and assigns the result to a new variable, all within a pipe.
#'
#' @param obj An object to be transformed.
#' @param name The name of the new variable to which the result will be assigned.
#' @param .f A function to apply to the object. Default is identity function.
#' @param ... Additional arguments to pass to `.f`.
#' @param envir The environment to which the new variable will be assigned. Default is the environment where the function was called.
#'
#'
#' @return The original object.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' library(magrittr)
#' mtcars %>% pipe_assign("mtcars_modified", ~ .x %>% mutate(mpg = mpg * 2))
#' }
#' @export
pipe_assign <- function (obj, name, .f = ~.x, ..., envir = parent.frame(n=2)) {

  if (!is.character(name) || length(name) != 1) {
    stop("Name must be a single character string.", call. = FALSE)
  }

  .f <- purrr::as_mapper(.f)
  assign(name, .f(obj, ...), envir = envir)

  return(obj)
}

