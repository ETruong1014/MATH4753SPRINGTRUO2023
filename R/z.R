#' Create z from vector
#'
#' @param x quantitative vector
#'
#' @importFrom stats sd
#'
#' @return a vector containing z
#' @export
#'
#' @examples
#' z(1:4)
z <- function(x) {
  z = (x - mean(x))/sd(x)
  return(z)
}
