#' Add up all elements of an unnested list
#' 
#' Add up all elents of a list. This works if all elements 
#' the list are of the same length. Such functionality is typically 
#' covered by data.frames. It is used to add up other more independent
#' structures. The function was built to add up zoo time series.
#' 
#' @param li a list object that contains zoo time series
#' @author Matthias Bannert
#' @export
add_up_list_elements <- function(li){
  run_this <- paste(paste("li[['",
                          names(li),
                          "']]",sep = ""),
                    collapse = " + "
  )
  eval(parse(text = run_this))
}
