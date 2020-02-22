#' myncurve
#'
#' displays a curve and an area
#'
#' when given a mean, sd, and an a value, this function graphs the normal distribution and area between -infinity and a
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a x = a (-inf < y < a)
#'
#' @return graphical output
#' @export
#'
#' @examples
#' myncurve(10, 5, 6)
myncurve = function(mu, sigma, a){
  curve(dnorm(x, mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  base =mu-4*sigma

  xcurve=seq(base,a,length=1000)

  # Y values corresponding t0 the x values
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  # Fill in the polygon with the given vertices
  polygon(c(base,xcurve,a),c(0,ycurve,0),col=rgb(.5,.8,0,.5))

  prob = pnorm(a, mean = mu, sd = sigma) - pnorm(base, mean = mu, sd = sigma)
  prob = round(prob, 4)

  text(x = a, y= max(ycurve)/2, paste("Area = ", prob, sep=""))

}
