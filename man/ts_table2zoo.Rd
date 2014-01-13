\name{ts_table2zoo}
\alias{ts_table2zoo}
\title{Turn time series data.table into zoo object}
\usage{
  ts_table2zoo(dtable, env, cname = "ch", pname = "kof",
    sname = "iq02", freq = 4, item_chunk = "item",
    ans_chunk = "an")
}
\arguments{
  \item{dtable}{the data.table object}

  \item{env}{environment to store the results}

  \item{cname}{name of the country, typically 2
  characters.}

  \item{pname}{name of the data.provider.}

  \item{sname}{name of the source.}

  \item{freq}{integer frequency of the resulting time
  series, defaults to 4. currently only quarterly time
  series are supported.}

  \item{item_chunk}{character chunk denoting items within
  the time series key. defaults to "item:}

  \item{ans_chunk}{character chunk denoting answer count.}
}
\description{
  This function extracts a zoo time series object from a
  data.table that was created by \code{\link{calc_shares}}
  and processed by \code{\link{dt2ts_table}}. The resulting
  zoo object is stored to an environmen
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{data.table}}
}

