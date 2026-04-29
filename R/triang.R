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

  dens <- ifelse(x < a | x > b, 0,
                 ifelse(x <= c,
                        2 * (x - a) / ((b - a) * (c - a)),
                        2 * (b - x) / ((b - a) * (b - c))
                 ))

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

  F <- ifelse(q < a, 0,
              ifelse(q <= c,
                     ((q - a)^2) / ((b - a) * (c - a)),
                     ifelse(q <= b,
                            1 - ((b - q)^2) / ((b - a) * (b - c)),
                            1)))

  return(F)
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

  q <- ifelse(p <= pc,
              a + sqrt(p * (b - a) * (c - a)),
              b - sqrt((1 - p) * (b - a) * (b - c)))

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
  x <- qtriang(u, min, max, mode)

  return(x)
}
