op <- par(no.readonly = T)

par(bg=rgb(231, 231, 231, maxColorValue = 255))
yield_paths_x <- 0:NUM_PERIODS_IN_SIM
yield_paths <- dcast(
  sim_rates_obs[
    country == YIELD_PATH_CHART_COUNTRY
    & class == YIELD_PATH_CHART_CLASS
    & maturity == YIELD_PATH_CHART_MATURITY
    & sim <= YIELD_PATH_CHART_NUM_SERIES
    , .(
      per
      , sim
      , yield_adj
    )
  ]
  , per ~ sim, value.var = "yield_adj"
)
x <- yield_paths$per
y <- yield_paths[ , !c("per")]
plot(
  range(x), range(y)
  , type="n"
  , bty="n"
  , ylab="Yield (pct)"
  , xlab="Month"
  , col.lab=rgb(89, 89, 89, maxColorValue = 255)
  , col.axis=rgb(89, 89, 89, maxColorValue = 255)
)
abline(h=0)
for (i in 1:YIELD_PATH_CHART_NUM_SERIES)
  lines(
    x, unlist(y[ , ..i])
    , col=caim::caim_colors(i, 0.2)
    , lwd=2.5
  )

# return paths
return_paths <- dcast(
  sim_rates_obs[
    country == YIELD_PATH_CHART_COUNTRY
    & class == YIELD_PATH_CHART_CLASS
    & maturity == YIELD_PATH_CHART_MATURITY
    & sim <= YIELD_PATH_CHART_NUM_SERIES
    , .(
      per
      , ret = cumprod(tot_ret) - 1
    )
    , by = .(country, class, sim, maturity)
  ]
  , per ~ sim, value.var = "ret"
)
x <- return_paths$per
y <- return_paths[ , !c("per")]
plot(
  range(x), range(y)
  , type="n"
  , bty="n"
  , ylab="Annual Return (pct)"
  , xlab="Month"
  , col.lab=rgb(89, 89, 89, maxColorValue = 255)
  , col.axis=rgb(89, 89, 89, maxColorValue = 255)
)
abline(h=0)
for (i in 1:YIELD_PATH_CHART_NUM_SERIES)
  lines(
    x, unlist(y[ , ..i])
    , col=caim::caim_colors(i, 0.2)
    , lwd=2.5
  )

par(op)
