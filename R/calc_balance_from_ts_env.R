#' Bulk Calculate Balances 
#' 
#' This function calculates share balances from an environment containing 
#' share time series. Balance basically mean to substract negative items from
#' positive items in a survey. 
#' 
#' @param env an environment
#' @param pos_name name of the positive item
#' @param neg_name name of the negative item
#' @param balance_label name of the label for the balance item
#' @author Matthias Bannert
#' @export
calc_balance_from_ts_env <- function(env,pos_name,neg_name,
                                     balance_label){
  # get positive and negative items as well as chunks for labelling
  ts_keys <- ls(envir=env)
  pos_keys <- sort(grep(pos_name,ts_keys,value=T))
  neg_keys <- sort(grep(neg_name,ts_keys,value=T))
  chunk <- gsub(neg_name,"",neg_keys)
  
  # sanity check, balance can only by calculated 
  # when there is a value for every period and both have 
  # the same length
  stopifnot(length(pos_keys) == length(neg_keys))
  
  # private function to select variables from environment by name 
  get_balance <- function(pos,neg,envir=env){
    envir[[pos]] - envir[[neg]]
  }
  
  # create a list of all balances and add it to env
  l <- lapply(seq_along(pos_keys),function(X) get_balance(pos_keys[[X]],neg_keys[[X]],env))
  names(l) <- paste(chunk,balance_label,sep="")
  list2env(l,envir=env)
}
