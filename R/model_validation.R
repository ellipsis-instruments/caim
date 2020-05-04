#' Generates k-fold selection index
#' @param n number of observations.
#' @param k number of folds. default = 10
#' @param seed seed for random number generator. default = NA
#' @export
k_fold_ix <- function(n, k=10, seed=NA) {
  if (!is.na(seed))
    set.seed(seed)
  return(data.table(k=rep_len(seq(1, k), n), ix=sample(n), key=c("k", "ix")))
}

#' Generates equally-cut index
#' @export
k_cut_ix <- function(n, k=10) {
  hi <- round(seq(1, k) * (n / k))
  lo <- c(0, hi[-length(hi)]) + 1
  return (rbindlist(lapply(sapply(seq(1,k), function(x)
    cbind(k=x, ix=seq(lo[x], hi[x]))), data.table)))
}


