\name{add_up_variables}
\alias{add_up_variables}
\title{Add time series that are elements of lists}
\usage{
  add_up_variables(var, input_env, result_env,
    main_chunk = "", attribute_name = "question")
}
\arguments{
  \item{var}{name of the variable}

  \item{input_env}{environment to search}

  \item{result_env}{environment that stores the output}

  \item{main_chunk}{character prefix for output}

  \item{attribute_name}{name of the attribute}
}
\description{
  This is specific function for weighting procedures. It is
  used to add all elements of an environment that have a
  certain value of attribute_name.
}
\author{
  Matthias Bannert
}

