\name{calc_shares}
\alias{calc_shares}
\title{Calculate the weighted share of an answer item}
\usage{
calc_shares(dt, var, sizeColumn)
}
\arguments{
  \item{dt}{a data.table containing the entire relevant
  data}

  \item{var}{a character representation denoting a
  particular column of the data.table given in dt.
  Typically this column represents a qualitative question}

  \item{sizeColumn}{a character representation of the
  data.table column that holds the quantitative weight. If
  you want to aggregate without weighting hand a vector of
  1s which is equal to nrow(dt).}
}
\value{
returns a data table that consists of the following
columns: - all columns that were keys of the input
data.table (this varies depending on the input) typically
this is: time, group, question - sumTest: sum of all
employees working in key group that chose a particular
answer item - sumTestTotal: sum of all employees working in
a key sector - AN: number of participants in the key group
that chose a particular answer item - ANTot: number of
participants in the key group - share: weighted share,
sumTest/sumTestTotal
}
\description{
This function calculates how often an answer item was
selected relatively to the other answer item of a question.
The result is given as a share. Besides the function
calculates additional information that is described in
greated detail below.
}
\author{
Matthias Bannert
}

