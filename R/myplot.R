#' myplot
#'
#' Make a function that produces heights for inputs "x"
#'
#' This is a function created in the Math4753 package that when given the spruce.lm data set it outputs the height in a plot
#'
#' @param x a vector of values
#'
#' @return a series of points used to calculate a curve
#' @export
#'
#' @examples
#' curve(myplot, lwd=2, col="steelblue",add=TRUE)
#' #
myplot=function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}
