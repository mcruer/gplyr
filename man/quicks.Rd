% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manipulate.R
\name{quicks}
\alias{quicks}
\title{Quickly summarize multiple columns in a data frame}
\usage{
quicks(df, .cols, .f = ~.x, ...)
}
\arguments{
\item{df}{A data frame or tibble.}

\item{.cols}{A selection of columns to which the function will be applied.}

\item{.f}{Function to be applied across the selected columns. Default is the identity function \code{~.x}.}

\item{...}{Additional arguments to be passed to the function \code{.f}.}
}
\value{
A summarized data frame with the function applied to the specified columns.
}
\description{
Quickly summarize multiple columns in a data frame
}
\examples{
\dontrun{
library(gplyr)
library(dplyr)

df <- tibble(a = 1:3, b = 4:6, c = 7:9)
summary_df <- quicks(df, .cols = c("a", "b"), .f = mean)
}
}
