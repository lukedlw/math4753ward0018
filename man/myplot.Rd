% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myplot.R
\name{myplot}
\alias{myplot}
\title{myplot}
\usage{
myplot(x)
}
\arguments{
\item{x}{a vector of values}
}
\value{
a series of points used to calculate a curve
}
\description{
Make a function that produces heights for inputs "x"
}
\details{
This is a function created in the Math4753 package that when given the spruce.lm data set it outputs the height in a plot
}
\examples{
curve(myplot, lwd=2, col="steelblue",add=TRUE)
#
}
