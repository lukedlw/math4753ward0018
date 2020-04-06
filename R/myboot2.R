#' myboot2
#'
#' Graphs the bootstrap confidence interval of the input of choice
#'
#' This is a function created in the Math4753 package that when given parameters of alpha and the given statistic it will graph the confidence interval using the bootstrap method
#'
#' @param iter number of iterations
#' @param x a vector of values
#' @param fun a string with the name of the value
#' @param alpha the value of alpha for the percentage calculation of CI
#' @param cx default cx value is 1.5 scale
#' @param ... edits the histogram
#'
#' @return a list with the given CI and other values
#' @export
#'
#' @examples
#' myboot2(10000,x=sam,fun="var",alpha=0.20,xlab="var(x)",col="Green",cx=1.5)
#' #
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  return(list(ci=ci,fun=fun,x=x,xstat=xstat))# Some output to use if necessary
}
