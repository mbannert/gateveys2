#' Convert data.table to a time series table
#' 
#' This function extract one particular item
#' over time from a data.table that contains
#' multiple items over time. It uses the data.table
#' key to identify relevant columns. 
#' 
#' @param dt a data.table created by \code{\link{calc_shares}}.
#' @param ... varying arguments needed to index over. 
#' @param tindex name of the time index column. 
#' @author Matthias Bannert
#' @seealso \code{\link{calc_shares}}
#' @export
dt2ts_table <- function(dt,...,
                        tindex = "time"
){
  
  dt[J(unique(get(tindex)),...)]
  
}