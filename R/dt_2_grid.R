#' Extract cartesian grid from data.table keys
#' 
#' This functions extract a cartesian grid from 
#' data.table keys except those declared in exclude.
#' 
#' @param dt a data.table
#' @param exclude a character vector denoting elements of 
#' the key that should not be respected. 
#' @author Matthias Bannert
#' @export
dt_2_grid <- function(dt,exclude=""){
  # extract key
  k  <- key(dt)
  # remove excluded elements
  k  <- k[!(k %in% exclude)]
  # select only non-excluded key cols
  # extract unique elements from each col
  # create and return a grid
  expand.grid(lapply(dt[,k,with=F],unique))
}


