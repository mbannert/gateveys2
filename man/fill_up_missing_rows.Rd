\name{fill_up_missing_rows}
\alias{fill_up_missing_rows}
\title{Find Missing Ranks in a data.table based on keys}
\usage{
fill_up_missing_rows(dt, extend_cols = NULL)
}
\arguments{
  \item{dt}{a data.table with specified keys}

  \item{extend_cols}{a named vector containing colums that
  should be bound to the result table.}
}
\description{
This function creates a full ranked matrix based on the
data.table key and gives back all rows of the full ranked
matrix that are missing in the given restricted data.table.
}
\author{
Matthias Bannert
}

