% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_manipulation.R
\name{merge_supersede}
\alias{merge_supersede}
\title{Merge and Supersede Column Values Between Two Data Frames}
\usage{
merge_supersede(df1, df2, unique_identifier, column)
}
\arguments{
\item{df1}{The primary data frame.}

\item{df2}{The secondary data frame whose non-NA values will supersede those in \code{df1}.}

\item{unique_identifier}{Unquoted name of the column that uniquely identifies each row.
This column must exist in both \code{df1} and \code{df2}.}

\item{column}{Unquoted name of the column whose values are to be superseded.
This column must exist in both \code{df1} and \code{df2}.}
}
\value{
A data frame resulting from merging the specified column of \code{df2} into \code{df1}.
This merge is based on the unique identifier, with \code{df2} values taking precedence
over \code{df1} values, except when \code{df2} has NA values.
}
\description{
\code{merge_supersede} merges a specified column from a second data frame (\code{df2})
into a first data frame (\code{df1}) based on a unique identifier shared by both
data frames. For each record, if the second data frame (\code{df2}) contains non-NA
values in the specified column, these values supersede those in the first data
frame (\code{df1}). The values from \code{df1} are preserved only when the corresponding
record in \code{df2} contains NA values in that column.
}
\examples{
\dontrun{
  # Assuming df1 and df2 are existing data frames with a common unique identifier
  # column 'id' and a common column 'value'.
  # The value from df2 supersedes that of df1 except where df2's value is NA.
  result <- merge_supersede(df1, df2, id, value)
}

}
