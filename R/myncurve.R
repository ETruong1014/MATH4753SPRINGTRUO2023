#' Plot for normal distribution and area of P(X<=a)
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a value to calculate P(X<=a)
#'
#' @importFrom stats dnorm pnorm
#' @importFrom graphics curve polygon
#'
#' @return P(X<=a) in a list
#' @export
#'
#' @examples
#' myncurve(5, 10, 3)
myncurve <- function(mu, sigma, a){
  # Create the normal distribution curve
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  # Calculate the area from -infinity to a
  area = pnorm(a, mu, sigma)
  area = round(area, 4)
  # Calculate polygon boundaries for the area
  xcurve = seq(mu-3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mu, sigma)
  # Make the polygon for the area
  polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col="Red")

  # Return the area value
  return(area)
}
