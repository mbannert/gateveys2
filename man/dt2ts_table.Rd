\name{dt2ts_table}
\alias{dt2ts_table}
\title{Convert data.table to a time series table}
\usage{
  dt2ts_table(dt, ..., tindex = "time")
}
\arguments{
  \item{dt}{a data.table created by
  \code{\link{calc_shares}}.}

  \item{...}{varying arguments needed to index over.}

  \item{tindex}{name of the time index column.}
}
\description{
  This function extract one particular item over time from
  a data.table that contains multiple items over time. It
  uses the data.table key to identify relevant columns.
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{calc_shares}}
}

