% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na_related.R
\name{replace_with_na}
\alias{replace_with_na}
\title{Replace matched strings with NA}
\usage{
replace_with_na(string, pattern, ignore_case = TRUE)
}
\arguments{
\item{string}{A character vector where matches to 'pattern' will be replaced with NA.}

\item{pattern}{A pattern to match against the elements of 'string'.}

\item{ignore_case}{Logical; should case be ignored when matching the pattern? Defaults to TRUE.}
}
\value{
A character vector with elements matching 'pattern' replaced with NA.
}
\description{
This function takes a character vector and replaces all elements that match a specified pattern with NA.
}
\examples{
replace_with_na(c("apple", "APPLE", "banana"), "apple")
replace_with_na(c("apple", "APPLE", "banana"), "apple", ignore_case = FALSE)
}
