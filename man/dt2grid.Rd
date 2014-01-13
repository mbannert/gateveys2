\name{dt2grid}
\alias{dt2grid}
\title{Extract cartesian grid from data.table keys}
\usage{
  dt2grid(dt, exclude = "")
}
\arguments{
  \item{dt}{a data.table}

  \item{exclude}{a character vector denoting elements of
  the key that should not be respected.}
}
\description{
  This functions extract a cartesian grid from data.table
  keys except those declared in exclude.
}
\author{
  Matthias Bannert
}

