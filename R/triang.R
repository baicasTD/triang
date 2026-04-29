#' Triangular density function
#'
#' Computes the probability density function of the triangular distribution.
#'
#' @param x Numeric vector of values
#' @param min Lower bound
#' @param max Upper bound
#' @param mode Mode of the distribution
#'
#' @return Numeric vector of densities
#' @export
dtriang <- function(x, min, max, mode) {

  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be in [min, max]")

  a <- min
  b <- max
  c <- mode

  dens <- numeric(length(x))

  # fuera del rango
  idx0 <- x < a | x > b
  dens[idx0] <- 0

  # tramo creciente
  idx1 <- x >= a & x <= c
  dens[idx1] <- 2 * (x[idx1] - a) / ((b - a) * (c - a))

  # tramo decreciente
  idx2 <- x > c & x <= b
  dens[idx2] <- 2 * (b - x[idx2]) / ((b - a) * (b - c))

  return(dens)
}

#' Triangular cumulative distribution function
#'
#' @param q Numeric vector
#' @param min Lower bound
#' @param max Upper bound
#' @param mode Mode
#'
#' @return Numeric vector
#' @export
ptriang <- function(q, min, max, mode) {

  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be in [min, max]")

  a <- min
  b <- max
  c <- mode

  cdf <- numeric(length(q))

  # izquierda
  cdf[q < a] <- 0

  # tramo creciente
  idx1 <- q >= a & q <= c
  cdf[idx1] <- ((q[idx1] - a)^2) / ((b - a) * (c - a))

  # tramo decreciente
  idx2 <- q > c & q <= b
  cdf[idx2] <- 1 - ((b - q[idx2])^2) / ((b - a) * (b - c))

  # derecha
  cdf[q > b] <- 1

  return(cdf)
}

#' Triangular quantile function
#'
#' @param p Numeric vector of probabilities
#' @param min Lower bound
#' @param max Upper bound
#' @param mode Mode
#'
#' @return Numeric vector
#' @export
qtriang <- function(p, min, max, mode) {

  if (min >= max) stop("min must be < max")
  if (mode < min || mode > max) stop("mode must be in [min, max]")
  if (any(p < 0 | p > 1)) stop("p must be in [0,1]")

  a <- min
  b <- max
  c <- mode

  pc <- (c - a) / (b - a)

  q <- numeric(length(p))

  idx1 <- p <= pc
  q[idx1] <- a + sqrt(p[idx1] * (b - a) * (c - a))

  idx2 <- p > pc
  q[idx2] <- b - sqrt((1 - p[idx2]) * (b - a) * (b - c))

  return(q)
}

#' Random generation from triangular distribution
#'
#' @param n Number of observations
#' @param min Lower bound
#' @param max Upper bound
#' @param mode Mode
#'
#' @return Numeric vector
#' @importFrom stats runif
#' @export
rtriang <- function(n, min, max, mode) {

  if (n <= 0) stop("n must be > 0")

  u <- runif(n)
  qtriang(u, min, max, mode)
}
