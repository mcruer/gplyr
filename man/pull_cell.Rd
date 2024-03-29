% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_cell.R
\name{pull_cell}
\alias{pull_cell}
\title{Extract a Specific Cell from a Dataframe}
\usage{
pull_cell(df, column, row = 1)
}
\arguments{
\item{df}{A dataframe from which the cell value will be extracted.}

\item{column}{The column name or index where the desired cell is located.}

\item{row}{The row number of the desired cell.}
}
\value{
The value of the cell at the specified column and row.
}
\description{
This function extracts the value of a specific cell from a dataframe based on
the given column and row.
}
\examples{
\dontrun{
df_example <- tibble(a = 1:5, b = letters[1:5])
pull_cell(df_example, "a", 3) # Returns 3
pull_cell(df_example, "b", 4) # Returns "d"
}

}
