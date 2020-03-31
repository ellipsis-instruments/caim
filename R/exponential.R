#' Calculates decay rate from halflife
#' @export
halflife2decay <- function(halflife) {
  return(1 - log(2) / halflife)
}

#' Calculates halflife from decay rate
#' @export
decay2halflife <- function(decay) {
  return(log(2) / (1 - decay))
}

#' Returns exponential density vector
#'
#' All weights sum to 1
#' @export
d_exp <- function(decay, numobs=100) {
  d_exp <- rep(1, numobs)
  for (i in seq(numobs-1, 1, -1))
    d_exp[i] <- d_exp[i + 1] * decay
  return(d_exp / sum(d_exp)) # normalize so all weights sum to 1
}

#' Calculates exponentially weighted mean
#' @export
mean_exp <- function(data, decay) {
  dens <- d_exp(decay, length(data))
  return(data %*% dens)
}
