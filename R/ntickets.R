#' Find the optimal number of tickets to sell for a flight
#'
#' @param N number of seats on flight
#' @param gamma probability of overbooking
#' @param p probability of a person showing
#'
#' @importFrom graphics abline layout title
#' @importFrom stats pbinom qbinom uniroot
#'
#' @return list containing optimal tickets for discrete and continuous, along with parameters
#' @export
#'
#' @examples
#' ntickets(N=400, gamma=0.02, p=0.95)
ntickets <- function(N, gamma, p) {
  # Using qbinom to find nd
  fd.q <- function(x) {
    qbinom(1 - gamma, x, p)
  }

  # Using pbinom to plot discrete objective function
  fd.p <- function(x) {
    1 - gamma - pbinom(N, x, p)
  }

  # Using pnorm to find nc and to plot it
  # Endpoint correction of 0.5
  fc <- function(x) {
    1 - gamma - pnorm(N + 0.5, x*p, sqrt(x*p*(1-p)))
  }

  # Define upper limit for finding minimum and plotting
  upperxlim = N + (1/10)*N

  # Using absolute value and which.min to find nd
  nd = fd.q(N:upperxlim)
  # Find where abs(qbinom - N) = 0 to match the distribution
  nd = abs(nd-N)
  nd = N + (which.min(nd) - 1)

  # Objective values for discrete distribution using pnorm
  objvals.d = fd.p(N:upperxlim)

  # Using uniroot to find nc
  nc = uniroot(fc, interval=c(N, upperxlim))$root

  # Plots
  layout(matrix(1:2, nrow = 2, ncol = 1))

  # Plot the discrete distribution
  plot(x=N:upperxlim, y=objvals.d,
       xlim=c(N, upperxlim), xlab="n", ylab="Objective",
       type="b", bg="Blue", pch=21)
  abline(v=nd, h=0, col="Red")
  title(main = paste0("Objective vs n to find optimal tickets sold\n(",
                      nd,
                      ") gamma=",
                      gamma,
                      " N=",
                      N,
                      " discrete"))

  # Plot the continuous distribution
  curve(fc, xlim=c(N, upperxlim), xlab="n", ylab="Objective")
  abline(v=nc, h=0, col="Blue")
  title(main = paste0("Objective vs n to find optimal tickets sold\n(",
                      nc,
                      ") gamma=",
                      gamma,
                      " N=",
                      N,
                      " continuous"))

  # Store values in final list
  finalList = list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  # Return final list
  return(finalList)
}

