\name{set_attributes_by_extract}
\alias{set_attributes_by_extract}
\title{Set attributes to each element of an environment}
\usage{
  set_attributes_by_extract(env, label = "group",
    extract_pattern = "[A-Z]{2}")
}
\arguments{
  \item{env}{an environment that contains all relevant
  objects}

  \item{label}{name of the attribute}

  \item{extract_pattern}{regular expression to extract a
  chunk of the object names}
}
\description{
  This function makes use of \code{\link{setattr}} from the
  Matthew Dowle's data.table package to set attributes for
  an an entire environment. The function can extract parts
  of an object's name and assign it to some attribute. The
  extraction uses \code{\link{str_extract}} from Hadley
  Wickham's stringr package.
}
\author{
  Matthias Bannert
}
\seealso{
  \code{\link{setattr}}, \code{\link{str_extract}}
}

