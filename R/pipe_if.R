#' Conditionally apply a transformation to a pipeline
#'
#' This function conditionally applies a transformation to a data frame or tibble.
#' The transformation is only applied if the provided condition evaluates to `TRUE`.
#' The right-hand side of the formula (`condition ~ transformation`) can be either:
#' - a full expression (e.g., `mutate(., new_col = x + 1)`)
#' - a bare function (e.g., `mutate`) with additional arguments supplied via `...`
#'
#' This function supports both styles. For use in pipelines, a corresponding infix
#' operator `%pif%` is provided, but due to R's parsing limitations, it cannot be
#' used with trailing `...` arguments. In those cases, use the `pif()` function form.
#'
#' @param lhs A data frame or tibble. Typically the result of the left-hand side of a pipe.
#' @param rhs A formula of the form `condition ~ transformation`, where:
#'   - the LHS is a logical expression evaluated in the calling environment,
#'   - the RHS is either a function name or a full expression using `.` as a placeholder.
#' @param ... Additional arguments to be passed to the transformation function
#'   when the RHS is a bare function.
#'
#' @return The transformed `lhs` if the condition is `TRUE`, otherwise `lhs` unchanged.
#'
#' @examples
#' \dontrun{
#'   library(dplyr)
#'
#'   condition <- TRUE
#'
#'   tibble(x = 1:3) %>%
#'     pif(condition ~ mutate(., y = x * 2))
#'
#'   tibble(x = 1:3) %>%
#'     pif(condition ~ mutate, y = x * 2)
#' }
#'
#' @importFrom rlang f_lhs f_rhs eval_tidy is_symbol env
#' @export
pif <- function(lhs, rhs, ...) {
  condition <- f_lhs(rhs)
  expr <- f_rhs(rhs)

  if (eval_tidy(condition, env = parent.frame())) {
    dot_exprs <- rlang::exprs(...)  # capture unevaluated expressions
    eval_env <- env(. = lhs, .x = lhs)

    if (is_symbol(expr)) {
      fn <- eval_tidy(expr, env = eval_env)
      # evaluate each dot in the context of the data
      args <- lapply(dot_exprs, eval_tidy, data = lhs, env = parent.frame())
      do.call(fn, c(list(lhs), args))
    } else {
      eval_tidy(expr, data = lhs, env = eval_env)
    }
  } else {
    lhs
  }
}

#' Infix version of `pif()` for use in pipelines
#'
#' This infix operator applies a transformation to a data frame conditionally, using
#' a formula of the form `condition ~ transformation`. It is designed for use inside
#' `%>%` pipelines.
#'
#' Note: Due to R's parsing rules, this operator **cannot be used** with additional
#' arguments (`...`) after the formula. If additional arguments are needed, use
#' the regular function form `pif()` instead.
#'
#' @param lhs A data frame or tibble (typically from a pipeline).
#' @param rhs A formula of the form `condition ~ transformation`.
#'
#' @return The transformed `lhs` if the condition is `TRUE`, otherwise `lhs` unchanged.
#'
#' @seealso [pif()] for a function version that supports additional arguments.
#'
#' @export
#' @rdname pif
`%pif%` <- function(lhs, rhs) {
  pif(lhs, rhs)
}
