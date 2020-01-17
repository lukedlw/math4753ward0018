#' myf
#'
#' returns the squares and cubes of the given vector
#'
#' This is a function created in the Math4753 package that when given a vector it returns a list of squares and cubes
#'
#' @param x a vector of values
#'
#' @return a list of cubed and squared vectors
#' @export
#'
#' @examples
#' x=1:4;myf(x)
myf = function(x){
  obj1=x^2
  obj2=x^3
  list(square=obj1,cube=obj2)
}
