\name{get_by_attributes}
\alias{get_by_attributes}
\title{Filter environment by multiple object attributes}
\usage{
get_by_attributes(search_env, attr_list)
}
\arguments{
  \item{search_env}{environment that needs to be searched}

  \item{attr_list}{a named vector that lists all
  attribtutes to look for and its values}
}
\description{
This function searches all elements of an environment for
all objects whose attribute x_i ... x_n have a certain
value v_i ... v_n. This is very helpful in aggregation
processes.
}
\author{
Matthias Bannert
}

