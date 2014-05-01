\name{cap}
\alias{cap}
\title{Exponentional Cap for Large Values in a Vector}
\usage{
  cap(n, threshold = 500, exponent = 0.7)
}
\arguments{
  \item{n}{vector to be capped}

  \item{threshold}{scalar at which the cap should come into
  play, defaults to 500.}

  \item{exponent}{an exponent typically below 1.}
}
\description{
  This function is often used in surveys to cap large
  weights caused by employment weighting.
}
\author{
  Matthias Banenrt
}

