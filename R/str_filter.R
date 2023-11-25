#' Filter strings based on a regex pattern
#'
#' This function filters a character vector, returning strings that match (or do not match if negate is TRUE) a specified regular expression pattern.
#'
#' @param string A character vector to be filtered.
#' @param regex_string The regular expression pattern to match against.
#' @param ignore_case Logical; should case be ignored in the match? Defaults to TRUE.
#' @param negate Logical; should the sense of the match be reversed? Defaults to FALSE.
#'
#' @return A character vector containing the filtered strings.
#'
#' @importFrom stringr str_detect
#' @importFrom stringr regex
#' @export
#'
#' @examples
#' str_filter(c("apple", "banana", "cherry"), "a")
#' str_filter(c("apple", "banana", "cherry"), "^a", negate = TRUE)
str_filter <- function (string, regex_string, ignore_case = TRUE, negate = FALSE) {
  index <- stringr::str_detect(string, stringr::regex(regex_string, ignore_case = ignore_case), negate = negate)
  string[index]
}
