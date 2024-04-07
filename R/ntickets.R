#' Project
#'
#' @param N
#'   The total number of tickets available.
#' @param gamma
#'   The risk tolerance level.
#' @param p
#'   The probability threshold for ticket sales.
#'
#' @return
#'   A list containing the optimal number of tickets to be sold under discrete and continuous distribution assumptions.
#'   The list includes:
#'   - 'nd': The optimal number of tickets sold under discrete distribution.
#'   - 'nc': The optimal number of tickets sold under continuous distribution.
#'   - 'N': The total number of tickets available.
#'   - 'p': The probability threshold for ticket sales.
#'   - 'gamma': The risk tolerance level.
#'
#' @export
#'
#' @examples
#' ntickets(400, 0.02, 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  n <- seq(N, floor(N + (0.1*N)), by = 1)
  discrete <- 1 - gamma - pbinom(q = N, size = n, prob = p) # discrete distribution
  cont <- function(n) {
    1 - gamma - pnorm(N+0.5, n*p, sqrt(n*p*(1-p))) # continuous distribution
  }
  continuous <- 1 - gamma - pnorm(N+0.5, n*p, sqrt(n*p*(1-p))) # continuous approximation
  nd <- n[which.min(abs(discrete))]
  nc <- optimize(f = function(x) abs(cont(x)), interval = c(N, floor(N + 0.1*N)))$minimum

  plot(n, discrete, type = 'b', main = paste("Objective Vs n to find optimal tickets sold \n(", nd, ") gamma =", gamma, " N =", N, "discrete"), ylab = "Objective", pch = 21, bg = "blue")
  abline(v = nd, h = 0, col = "red")
  curve(cont(x), N, floor(N + 0.1*N), main = paste("Objective Vs n to find optimal tickets sold \n(", nc, ") gamma =", gamma, " N =", N, "continuous"), ylab = "Objective", xlab = "n", lwd = 2)
  abline(v = nc, h = 0, col = "blue")

  x <- list(nd=nd, nc=nc, N=N, p=p, gamma=gamma)

  print(x)

}
