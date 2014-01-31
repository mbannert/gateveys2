
#' Find Missing Ranks in a data.table based on keys
#' 
#' This function creates a full ranked matrix based on the 
#' data.table key and gives back all rows of the full ranked 
#' matrix that are missing in the given restricted data.table. 
#' 
#' @param dt a data.table with specified keys
#' @param extend_cols a named vector containing colums that should be bound
#' to the result table. 
#' @author Matthias Bannert
#' @export
fill_up_missing_rows <- function(dt,extend_cols = NULL){
  stopifnot(length(key(dt)) != 0)
  # find maximum number of rows if key(dt)
  # was a unique PK
  full <- expand.grid(sapply(dt[,1:length(key(dt)),with = FALSE],unique))
  
  # add replacement for non-existing values
  # typically zero when calculating shares
  if(!is.null(extend_cols)){
    td <- t(extend_cols)  
    full <- cbind(full,td)
  }
  
  full <- data.table(full,key=key(dt))
  # find all rows that in restricted matrix
  # that are missing as opposed to the full rank
  full[!J(dt),]
}
