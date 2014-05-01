\name{add_up_list_elements}
\alias{add_up_list_elements}
\title{Add up all elements of an unnested list}
\usage{
  add_up_list_elements(li)
}
\arguments{
  \item{li}{a list object that contains zoo time series}
}
\description{
  Add up all elents of a list. This works if all elements
  the list are of the same length. Such functionality is
  typically covered by data.frames. It is used to add up
  other more independent structures. The function was built
  to add up zoo time series.
}
\author{
  Matthias Bannert
}

