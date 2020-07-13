#' Bootstrap simulation "selection index"
#' @export
sim_ix <- function(num_obs, num_sims, block_size=1, prob=NULL, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  simix <- sample(num_obs, num_sims, replace=T, prob=prob)
  if (block_size > 1) { # conduct block bootstrap
    c <- 0
    for (i in 1:num_sims) {
      if (c == 0) { # start of a series, use the existing value
        ix <- simix[i]
      } else { # increment previous value
        val <- ix + c
        if (val > num_obs) { # greater than maximum value, start a new block
          ix <- simix[i]
          c <- 0
        } else { # update current observation
          simix[i] <- val
        }
      }
      c <- c + 1
      if (c > block_size - 1) { # have hit maximum blocksize, start a new block
        c <- 0
      }
    }

    # # note for posterity: the for-loop above is materially faster
    # # than the while loop below
    # i <- 1
    # while (i <= num_sims) {
    #   s <- simix[i]
    #   block <- min(block_size - 1, num_obs - s)
    #   ix <- i:min(num_sims, i + block)
    #   simix[ix] <- seq(s, s + block)[1:length(ix)]
    #   i <- i + block + 1
    # }
  }

  simix
}

#' Bootstrap simulation "selection table"
#' @export
sim_period_ix <- function(num_sims, num_periods, num_obs, block_size=1, prob=NULL, seed=NULL) {
  simtable <- data.table(expand.grid(
    sim = 1:num_sims
    , per = 1:num_periods
  ))[
    , ix := sim_ix(num_obs, num_periods, block_size, prob, seed)
    , keyby = sim
  ]
  simtable
}
