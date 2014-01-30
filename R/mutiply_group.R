#' Multiply elements of an environment according to a named vector
#' 
#' There's no further description. 
#' @param grp name of the selected group, needs to be part of the vector names.
#' @param v_weights named vector
#' @param input_env environment that contains the input
#' @param result_env environment to store the results
#' @author Matthias Bannert
#' @export
multiply_group <- function(grp,v_weights,input_env,result_env){
  # select group specific weight
  s_w <- v_weights[grp]
  # list of all time series for this group
  grp_list <- get_by_attributes(input_env,c(group = grp))
  # multiple all elements of the list with the weight
  w_grp_list <- lapply(grp_list,"*",s_w)
  list2env(w_grp_list,result_env)
}
