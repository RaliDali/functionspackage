#' @title myncurve
#'
#' @param mu Mean
#' @param sigma Standard deviation
#' @param a Upper tail value
#' @import graphics
#' @import stats
#'
#' @return Finds area under normal curve
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu = 3, sigma = 2, a = 4)}

myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu - 3*sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col = "Red")
  prob = pnorm(a, mu, sigma) - pnorm(mu-3*sigma, mu, sigma)
  prob = round(prob, 4)
  list(prob)
}
