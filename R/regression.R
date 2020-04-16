#' Constrained weighted linear regression via quadratic programming
#'
#' Linear regression with weights and constraints can be expressed as a
#' quadratic programming problem. See, for example,
#' https://en.wikipedia.org/wiki/Weighted_least_squares
#'
#' @param y vector of Y values
#' @param X feature matrix
#' @param Aeq Amat for equality constraints. numconstraints x numX
#' @param beq bvec for inequality constraints
#' @param A Amat for >= inequality constraints
#' @param b bvec for >= inequality constraints
#' @param lb vector of lower bounds
#' @param ub vector of upper bounds
#' @param wts observation weightings. default=equal-weighted
#' @param fixed_coefs if !is.null, does not run regression, but builds model with given coefs
#' @export
lm_quad <- function(y, X
                    , Aeq=NULL, beq=NULL
                    , A=NULL, b=NULL
                    , lb=NULL, ub=NULL
                    , wts=NULL
                    , fixed_coefs=NULL
                    ) {
  X_names <- colnames(X)
  if (is.null(X_names)) X_names <- "x"
  X <- as.matrix(X)

  has_invalid <- rowSums(is.na(cbind(y, X))) > 0
  y <- y[!has_invalid]
  X <- X[!has_invalid, ]
  if (!is.null(wts))
    wts <- wts[!has_invalid]

  num_X <- ncol(X)
  N <- nrow(X)

  if (is.null(wts))
    wts <- rep(1, N)
  wts <- wts / sum(wts)

  if (is.null(fixed_coefs)) {
    W <- diag(wts)

    Dmat <- t(X) %*% W %*% X
    dvec <- t(y) %*% W %*% X

    Amat <- matrix(0, 0, num_X, dimnames=list(NULL, X_names))
    bvec <- NULL
    meq <- 0

    if (!is.null(Aeq)) {
      Amat <- rbind(Amat, Aeq)
      bvec <- c(bvec, beq)
      meq <- length(bvec)
    }

    if (!is.null(A)) {
      Amat <- rbind(Amat, A)
      bvec <- c(bvec, b)
    }

    if (!is.null(lb)) {
      Amat <- rbind(Amat, diag(num_X))
      bvec <- c(bvec, lb)
    }

    if (!is.null(ub)) {
      Amat <- rbind(Amat, -diag(num_X))
      bvec <- c(bvec, -ub)
    }

    qp <- quadprog::solve.QP(Dmat, dvec, t(Amat), bvec, meq)
  }

  res <- list()
  res$y <- y
  res$X <- X
  res$wts <- wts
  if (is.null(fixed_coefs))
    res$coefs <- qp$solution
  else
    res$coefs <- fixed_coefs
  names(res$coefs) <- X_names

  return (res)
}

