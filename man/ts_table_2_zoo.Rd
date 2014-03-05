\name{ts_table_2_zoo}
\alias{ts_table_2_zoo}
\title{Turn time series data.table into zoo object}
\usage{
  ts_table_2_zoo(dtable, env, cname = "ch", pname = "kof",
    sname = "iq02", freq = 4, item_chunk = "item",
    ans_chunk = "an", var_in_key = 4)
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

  \item{var_in_key}{of variable name in data.table key,
  defaults to 4}
}
\description{
  This function extracts a zoo time series object from a
  data.table that was created by \code{\link{calc_shares}}
  and processed by \code{\link{dt_2_ts_table}}. The
  resulting zoo object is stored to an environment
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{data.table}}
}

