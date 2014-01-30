
#' Add time series that are elements of lists
#' 
#' This is specific function for weighting procedures. 
#' It is used to add all elements of an environment that 
#' have a certain value of attribute_name. 
#' 
#' @param var name of the variable
#' @param input_env environment to search
#' @param result_env environment that stores the output
#' @param main_chunk character prefix for output
#' @param attribute_name name of the attribute
#' @author Matthias Bannert
#' @export
add_up_variables <- function(var,input_env,result_env,
                                      main_chunk = "",
                             attribute_name="question"){
  # character representation of environment name
  env_name <- deparse(substitute(input_env))
  # select all series for one variable
  series_by_variable <- get_by_attributes(input_env,c(attribute_name=var))
  nm <- names(series_by_variable)
  run_this <- paste(paste(env_name,"[['",
                          names(series_by_variable),
                          "']]",
                          sep=""),
                    collapse = " + ")
  # evaluate the summation of all weighted groups
  res <- eval(parse(text = run_this))
  # We need to define a name of the result
  output_name <- paste(main_chunk,var,sep=".")
  assign(output_name,res,envir=result_env)
}
