% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter_out_numeric}
\alias{filter_out_numeric}
\title{Filter Out Numeric Values from Selected Columns}
\usage{
filter_out_numeric(df, .cols = dplyr::everything(), na.rm = FALSE)
}
\arguments{
\item{df}{A dataframe to be filtered.}

\item{.cols}{Columns to check for non-numeric values; defaults to all columns.}

\item{na.rm}{Logical; if TRUE, rows where the selected columns are NA are excluded.}
}
\value{
A dataframe with rows containing non-numeric values in the specified columns.
}
\description{
This function filters a dataframe to retain rows where the selected columns contain non-numeric values.
It can optionally remove rows where the selected columns are NA.
}
\examples{
# Example dataframe
df <- data.frame(
  a = c("1", "2", "x", NA),
  b = c("y", "2", "3", "4")
)

# Filter out rows with numeric values in all columns
filter_out_numeric(df)

# Filter out rows with numeric values in column 'a', ignoring NAs
filter_out_numeric(df, .cols = a, na.rm = TRUE)
}
