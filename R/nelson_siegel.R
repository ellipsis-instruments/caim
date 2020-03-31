#' Calculates Nelson-Siegel beta1 loading
#' @export
ns_beta1_loading <- function(mat, lambda) {
  return ( (1 - exp(-lambda * mat)) / (lambda * mat) )
}

#' Calculates Nelson-Siegel beta2 loading
#' @export
ns_beta2_loading <- function(mat, lambda) {
  return ( (1 - exp(-lambda * mat)) / (lambda * mat) - exp(-lambda * mat) )
}

#' Generates Nelson-Siegel coefficients from maturities and yields
#' @export
ns_yields2coefs <- function(mats, yields, lambda = 0.7173239, wts = rep(1/length(mats), length(mats))) {
  res <- glm(yields
             ~ 1
             + ns_beta1_loading(mats, lambda)
             + ns_beta2_loading(mats, lambda)
             , weights = wts
  )
  coefs <- res$coefficients; names(coefs) <- c("beta0", "beta1", "beta2")
  wss <- res$deviance; names(wss) <- "wss"
  names(lambda) <- "lambda"
  return(c(coefs, lambda, wss))
}

#' Generates yield curve from Nelson-Siegel coefficients
#' @export
ns_coefs2yields <- function(coefs, mats = c(.0833, seq(0.25, 30, 0.25))) {
  x <- mats
  y <- (coefs["beta0"]
        + coefs["beta1"]*ns_beta1_loading(mats, coefs["lambda"])
        + coefs["beta2"]*ns_beta2_loading(mats, coefs["lambda"])
  )
  return(cbind(x, y))
}

#' Calculates Nelson-Siegel lambda that maximizes beta2 loading at mat
#' @export
ns_lambda2mat <- function(lambda, matrange=c(0, 30)) {
  return(optimize(ns_beta2_loading, interval=matrange, lambda=lambda, maximum=T)$maximum)
}

#' Calculates maturity where Nelson-Siegel beta2 loading is maximized for given lambda
#' @export
ns_mat2lambda <- function(mat, lambdarange=c(0, 1000)) {
  return(
    optimize(ns_beta2_loading,interval=lambdarange,
             mat=mat,maximum=T)$maximum )
  # return(lambda)
}

#' Calculates Nelson-Siegel lambda that maximizes beta2 loading at mat
#' @export
ns_fwd <- function(coefs, timefwd) {
  beta0_spot <- coefs["beta0"]
  beta1_spot <- coefs["beta1"]
  beta2_spot <- coefs["beta2"]
  lambda <- coefs["lambda"]

  beta0_fwd <- beta0_spot
  beta1_fwd <- beta1_spot * exp(-lambda * timefwd)
  + beta2_spot * lambda * timefwd * exp(-lambda * timefwd)
  beta2_fwd <- beta2_spot * exp(-lambda*timefwd)

  coefs <- c(beta0_fwd,beta1_fwd,beta2_fwd,lambda)
  names(coefs) <- c("beta0", "beta1", "beta2", "lambda")
  return(coefs)
}
