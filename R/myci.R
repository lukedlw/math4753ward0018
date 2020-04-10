#' myci
#'
#' returns a ci for the given sample set
#'
#' when given a sample vector of values (x) this function will return a confidence interval constructed from a ttest
#'
#' @param x a vector of values
#'
#' @return confidence interval of x
#' @export
#'
#' @examples
#' myci(x)
#'
myci=function(x){
  return(t.test(x, conf.level = .95)$conf.int)
}
