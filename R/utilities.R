#' Clears environment and console
#' @export
clr <- function() {
  rm(list=ls(envir=.GlobalEnv), envir=.GlobalEnv)
  cat("\014")
}

#' Maximum double value
#'
#' Replacement for Inf when functions can't handle Inf (like quadprog, apparently)
#' @export
INF <- .Machine$double.xmax

#' Minimum double value
#'
#' Replacement for -Inf when functions can't handle Inf (like quadprog, apparently)
#' @export
NEG_INF <- -.Machine$double.xmax

