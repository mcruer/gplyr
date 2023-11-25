#' Replace matched strings with NA
#'
#' This function takes a character vector and replaces all elements that match a specified pattern with NA.
#'
#' @param string A character vector where matches to 'pattern' will be replaced with NA.
#' @param pattern A pattern to match against the elements of 'string'.
#' @param ignore_case Logical; should case be ignored when matching the pattern? Defaults to TRUE.
#'
#' @return A character vector with elements matching 'pattern' replaced with NA.
#'
#' @importFrom stringr str_detect
#' @importFrom stringr regex
#' @export
#'
#' @examples
#' replace_with_na(c("apple", "APPLE", "banana"), "apple")
#' replace_with_na(c("apple", "APPLE", "banana"), "apple", ignore_case = FALSE)
replace_with_na <- function(string, pattern, ignore_case = TRUE){
  replace(string, stringr::str_detect(string, stringr::regex(pattern, ignore_case = ignore_case)), NA_character_)
}
