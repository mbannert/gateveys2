#' Set attributes to each element of an environment
#' 
#' This function makes use of \code{\link{setattr}} from the 
#' Matthew Dowle's data.table package to set attributes for an 
#' an entire environment. The function can extract 
#' parts of an object's name and assign it to some attribute.
#' The extraction uses \code{\link{str_extract}} from Hadley Wickham's
#' stringr package.
#' 
#' @param env an environment that contains all relevant objects
#' @param label name of the attribute
#' @param extract_pattern regular expression to extract a chunk of
#' the object names
#' @author Matthias Bannert
#' @seealso \code{\link{setattr}}, \code{\link{str_extract}}
#' @export
set_attributes_by_extract <- function(env,label = "group",
                                     extract_pattern = "[A-Z]{2}"){
  nm <- ls(envir = env)
  for (i in seq_along(nm)){
    group  <- str_extract(nm[i],extract_pattern)
    setattr(env[[nm[i]]], name = label, value = group)
  }
}
