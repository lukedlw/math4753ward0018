#' mynbin
#'
#' Calculates negative binomial distribution
#'
#' when given a sample size, a rth success, and a probability, this function gives the negative binomial distribution
#'
#' @param y size
#' @param r rth success
#' @param p probability
#'
#' @return The Negative binomial distribution
#' @export
#'
#' @examples
#' mybin(10, 3, .4)
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
