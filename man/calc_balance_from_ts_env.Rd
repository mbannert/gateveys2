\name{calc_balance_from_ts_env}
\alias{calc_balance_from_ts_env}
\title{Bulk Calculate Balances}
\usage{
calc_balance_from_ts_env(env, pos_name, neg_name, balance_label)
}
\arguments{
  \item{env}{an environment}

  \item{pos_name}{name of the positive item}

  \item{neg_name}{name of the negative item}

  \item{balance_label}{name of the label for the balance
  item}
}
\description{
This function calculates share balances from an environment
containing share time series. Balance basically mean to
substract negative items from positive items in a survey.
}
\author{
Matthias Bannert
}

