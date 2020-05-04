#' Calculate weighted mean
#'
#' @param x vector or matrix of data
#' @param wts vector of wts. NULL will produce equally-weighted results. wts will be normalized
#'   to sum to 1. default = NULL
#' @export
wtd_mean <- function(x, wts=NULL) {
  xmat <- as.matrix(x)
  N <- nrow(xmat)
  if (is.null(wts))
    wts <- rep(1/N, N)
  else
    # normalize weights to sum to 1
    wts <- wts / sum(wts)
  res <- as.numeric(wts %*% xmat)
  names(res) <- colnames(xmat)
  return(res)
}

#' Calculate weighted variance
#'
#' @param x vector or matrix of data
#' @param wts vector of weights. NULL will produce equally-weighted results. wts will be normalized
#'   to sum to 1. default = NULL
#' @param population F will return sample variance, T will return population variance. default=F
#' @export
wtd_variance <- function(x, wts=NULL, population=F) {
  xmat <- as.matrix(x)
  N <- nrow(xmat)
  numX <- ncol(xmat)

  if (is.null(wts))
    wts <- rep(1/N, N)
  else
    # normalize weights to sum to 1
    wts <- wts / sum(wts)

  xhat <- caim::wtd_mean(xmat, wts)

  if (!population)
    # adjust weights for sample variance
    wts <- wts * N / (N - 1)

  if(numX == 1)
    res <- as.numeric(wts %*% apply(xmat, 1, function(r) r - xhat) ^ 2)
  else
    res <- as.numeric(wts %*% t(apply(xmat, 1, function(r) r - xhat) ^ 2))
  names(res) <- colnames(xmat)
  return(res)
}

#' Calculate weighted standard deviation
#'
#' @param x vector or matrix of data
#' @param wts vector of weights. NULL will produce equally-weighted results. wts will be normalized
#'   to sum to 1. default = NULL
#' @param population F will return sample std dev, T will return population std dev. default=F
#' @export
wtd_sd <- function(x, wts=NULL, population=F) {
  return(sqrt(caim::wtd_variance(x, wts, population)))
}

#' Calculate weighted covariance
#'
#' @param x vector or matrix of data
#' @param y vector of data. default = NULL.
#' @param wts vector of weights. NULL will produce equally-weighted results. wts will be normalized
#'   to sum to 1. default = NULL
#' @param population F will return sample covariance, T will return population covariance. default=F
#' @return covariance matrix if y == NULL, otherwise covariance between x and y.
#' @export
wtd_cov <- function(x, y=NULL, wts=NULL, population=F) {
  if (!is.null(y))
    x <- cbind(x=as.matrix(x)[ , 1], y)
  xmat <- as.matrix(x)
  N <- nrow(xmat)
  numX <- ncol(xmat)
  xnames <- colnames(xmat)

  if (is.null(wts))
    # create vector of equal weights
    wts <- rep(1/N, N)
  else
    # normalize given weights to sum to 1
    wts <- wts / sum(wts)

  # calculate series means where wts are normalized to sum to 1
  xhat <- caim::wtd_mean(xmat, wts)

  # adjust weights for sample variance if necessary
  if (!population)
    wts <- wts * N / (N - 1)

  # calculate (xi - xhat) deviations for each column
  xdevs <- t(apply(xmat, 1, function(x) x - xhat))

  # calculate weighted covariance matrix via matrix multiplication
  # sum(wi * (xi-xhat) * (yi-yhat)) = t(wts * xdevs) %*% xdevs
  covmtx <- matrix(t(wts * xdevs) %*% xdevs, numX, numX, dimnames=list(xnames, xnames))

  if (is.null(y))
    return(covmtx)
  else
    # the off-diagonal part of covmtx is the covariance between the two series
    return(as.numeric(covmtx[1, numX]))

}

#' Calculate weighted correlation
#'
#' @param x vector or matrix of data
#' @param y vector of data. default = NULL.
#' @param wts vector of weights. NULL will produce equally-weighted results. wts will be normalized
#'   to sum to 1. default = NULL
#' @param population F will return sample correlation, T will return population correlation default=F
#' @return correlation matrix if y == NULL, otherwise correlation between x and y.
#' @export
wtd_cor <- function(x, y=NULL, wts=NULL, population=F) {
  if (!is.null(y))
    x <- cbind(x=as.matrix(x)[ , 1], y)
  xmat <- as.matrix(x)
  numX <- ncol(xmat)

  cormtx <- caim::cov2cor(caim::wtd_cov(xmat, wts=wts, population=population))

  if (is.null(y))
    return(cormtx)
  else
    return(as.numeric(cormtx[1, numX]))
}

#' Calculate correlation matrix from covariance matrix
#' @export
cov2cor <- function(covmtx) {
  return(stats::cov2cor(covmtx))
}

#' Calculate covariance matrix from correlation matrix and vector of standard deviations
#' @export
cor2cov <- function(cormtx, stdevs) {
  cov <- diag(stdevs) %*% cormtx %*% diag(stdevs)
  dimnames(cov) <- list(row.names(cor), colnames(cor))
  return(cov)
}

#' Extract standard deviations from covariance matrix
#' @export
cov2sd <- function(covmtx) {
  return(sqrt(diag(covmtx)))
}

#' Frequency below a given alpha
#' @export
freq_below <- function(data, alpha=0) {
  func <- function(dt,a) sum(dt<a)/length(dt)
  return(func(data,alpha))
}

#' Empirical Value-at-Risk (VaR)
#' @export
var_emp <- function(data, confidence=0.95) {
  return (quantile(data, 1 - confidence))
}

#' Empirical Conditional Value-at-Risk (CVaR)
#' @export
cvar_emp <- function(data, confidence=0.95) {
  return(mean(data[data <= caim::var_emp(data,confidence)]))
}
