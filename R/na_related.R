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

#' Convert NA values to TRUE
#'
#' @description
#' `na_to_T` takes a vector and replaces all `NA` values with `TRUE`,
#' leaving all other values unchanged. This function is useful for
#' handling missing data in logical vectors or conditions.
#'
#' @param x A vector (numeric, logical, character, etc.) where `NA`
#' values need to be replaced with `TRUE`.
#'
#' @return A vector of the same type as `x` where all `NA` values
#' have been replaced with `TRUE`.
#'
#' @examples
#' \dontrun{
#' vec <- c(NA, 1, 0, NA, TRUE, FALSE)
#' na_to_T(vec)
#' }
#'
#' @export
na_to_T <- function(x) {
  dplyr::if_else(is.na(x), TRUE, x)
}


#' Convert NA values to FALSE
#'
#' @description
#' `na_to_F` takes a vector and replaces all `NA` values with `FALSE`,
#' leaving all other values unchanged. This function is useful for
#' handling missing data in logical vectors or conditions.
#'
#' @param x A vector (numeric, logical, character, etc.) where `NA`
#' values need to be replaced with `FALSE`.
#'
#' @return A vector of the same type as `x` where all `NA` values
#' have been replaced with `FALSE`.
#'
#' @examples
#' \dontrun{
#' vec <- c(NA, 1, 0, NA, TRUE, FALSE)
#' na_to_F(vec)
#' }
#'
#' @export
na_to_F <- function(x) {
  dplyr::if_else(is.na(x), FALSE, x)
}


#' Sum values with removal of NA values
#'
#' This function sums all the provided values, automatically removing any NA values.
#' It is a simple wrapper around the base R `sum` function with `na.rm = TRUE` set by default.
#'
#' @param ... Numeric values or numeric vectors to be summed.
#'
#' @return The sum of all provided values with NA values removed.
#'
#' @examples
#' \dontrun{
#' sum_rna(1, 2, 3, NA, 5)  # Returns 11
#' sum_rna(c(1, 2, NA, 4), c(NA, 6))  # Returns 13
#' }
#'
#' @export
sum_rna <- function (...){
  sum(..., na.rm = TRUE)
}
