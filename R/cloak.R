#' Cloak an object with hidden metadata
#'
#' This function wraps any R object and attaches hidden metadata
#' (referred to as "secrets") via attributes. The object remains usable
#' as its original class but can later be inspected using `uncloak()`.
#'
#' @param value An object to wrap (e.g., a character, tibble, list, etc.).
#' @param secrets A named list of metadata to store invisibly.
#'
#' @return An object of class `cloak` that behaves like `value`
#' but carries hidden metadata accessible via `uncloak()`.
#' @export
cloak <- function(value, secrets = list()) {
  stopifnot(is.list(secrets))
  structure(
    value,
    secrets = secrets,
    class = c("cloak", class(value))
  )
}

#' Reveal hidden metadata from a cloaked object
#'
#' This function extracts the secret metadata stored via `cloak()`.
#'
#' @param x An object created with `cloak()`.
#'
#' @return A list of hidden metadata, or `NULL` if no metadata is found.
#' @export
uncloak <- function(x) {
  attr(x, "secrets")
}
