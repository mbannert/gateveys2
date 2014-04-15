#' Exponentional Cap for Large Values in a Vector
#' 
#' This function is often used in surveys to cap large weights
#' caused by employment weighting. 
#' 
#' @param n vector to be capped
#' @param threshold scalar at which the cap should come into play, defaults to 500.
#' @param exponent an exponent typically below 1. 
#' @author Matthias Banenrt
#' @export
cap <- function (n, threshold = 500, exponent = 0.7)
{
  ifelse(n > threshold,
         threshold + (n - threshold)^exponent,
         n)
}