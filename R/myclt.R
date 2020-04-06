#' myclt
#'
#' graphical function using the CLT
#'
#' This function takes in an n,iter,a,b and returns a distribution and a graph using the central limit theorem
#'
#' @param n number
#' @param iter amount
#' @param a the starting value of the uniform sample
#' @param x the ending value of the uniform sample
#'
#' @return graphical output of the CLT tests
#' @export
#'
#' @examples
#' myclt(n=5,iter=10000,a=0,b=5)
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  invisible(sm)
}
