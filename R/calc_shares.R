# This file documents calcshares using the oxygen documentation standard
# google for roxygen
# you don't need to source this file, it's just for documentation 
# puporses.

#' Calculate the weighted share of an answer item
#' 
#' This function calculates how often an answer item was selected relatively
#' to the other answer item of a question. The result is given as a share. 
#' Besides the function calculates additional information that is described
#' in greated detail below. 
#' 
#' @param dt a data.table containing the entire relevant data
#' @param var a character representation denoting a particular column
#' of the data.table given in dt. Typically this column represents a 
#' qualitative question. The columns of the data.table to which var refers were
#' tested for factor, integer and character columns.  
#' @param sizeColumn a character representation of the data.table column
#' that holds the quantitative weight. If you want to aggregate without 
#' weighting hand a vector of 1s which is equal to nrow(dt).
#' 
#' @return returns a data table that consists of the following columns:
#' - all columns that were keys of the input data.table (this varies depending on the input)
#' typically this is: time, group, question
#' - sumTest: sum of all employees working in key group that chose a particular answer item
#' - sumTestTotal: sum of all employees working in a key sector
#' - AN: number of participants in the key group that chose a particular answer item
#' - ANTot: number of participants in the key group
#' - share: weighted share, sumTest/sumTestTotal 
#' @author Matthias Bannert
#' @export
calc_shares <- function (dt, var, sizeColumn) 
{
  # turn character representations into an R name Object
  v <- as.name(var)
  sizeColumn <- as.name(sizeColumn)
  
  # make sophisticated use of data.table to highly dynamic subsets
  ######################################
  # data.table subset i, do j, group by
  ######################################
  # i checks selects all the observations in the data.table for which 
  # question v is NOT NA (NA means not available, very much similar to SQL's NULL)
  # remember the data.table syntax always reads like:
  
  res_dt <- dt[i = !(is.na(eval(v))),
               j = {
                 # .SD stands for subset of data.table we use SD to 
                 # create different subsets within the same function call
                 # see also What does .SD mean:
                 # http://stackoverflow.com/questions/8508482/what-does-sd-stand-for-in-data-table-in-r
                 # n is the subset of all employees that represent a firm that
                 # has a valid value for question (that means not NA)
                 # sum just creates the sum of all employees for whom this condition holds
                 n <- sum(.SD[, eval(sizeColumn)])
                 # n1 is similar to n, it uses the same subset condition, but only counts the number of elements in 
                 # in the vector which is equal to the number of answers under that condition
                 n1 <- length(.SD[, eval(sizeColumn)])
                 # subsequently another, more constrained subset is built
                 # the by statement simply creates a GROUP BY (just like SQL's GROUP BY)
                 # and groups all the elements with respect to their value of answer 
                 # typically 2,3 or 4
                 # list prepares the output
                 # name of the output column = value that is calculated
                 .SD[, list(sumTest = sum(eval(sizeColumn)),
                            # just use n defined above
                            sumTestTotal = n, 
                            # this is a nested subset, we count
                            # the number of answers under the restriction of the GROUP BY answer item
                            AN = length(eval(sizeColumn)),
                            # just use n defined above
                            ANTot = n1, 
                            # simply calculate the share 
                            share = sum(eval(sizeColumn))/n
                            # end list
                 ),
                     # the inner group by statement respecting the answer items of question v
                     by = eval(as.character(v))]
                 # outer group by statement over all keys of the input dt
               }, by = key(dt)]
  # calc shares got to add a question specific key
  setkeyv(res_dt,c(key(res_dt),var))
  res_dt
}

