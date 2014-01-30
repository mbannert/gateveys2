#' Filter environment by multiple object attributes
#' 
#' This function searches all elements of an environment
#' for all objects whose attribute x_i ... x_n have a
#' certain value v_i ... v_n. This is very helpful 
#' in aggregation processes. 
#' 
#' @param search_env environment that needs to be searched
#' @param attr_list a named vector that lists all attribtutes 
#' to look for and its values
#' @author Matthias Bannert
#' @export
get_by_attributes <- function(search_env,attr_list){
  # create a list from environment
  li <- eapply(search_env,function(x) x)
  # create list of t/f vectors for every condition
  t_f_list <- list()
  for (i in 1:length(attr_list)){
    t_f_list[[i]] <- sapply(search_env,attr,
                            which = names(attr_list)[i]) == attr_list[i]
  }
  # find only those elements for which ALL filter conditions 
  # are true and give a list of those back
  t_f_df <- as.data.frame(t_f_list)
  names(t_f_df) <- NULL
  li[apply(t_f_df,1,all)]
}