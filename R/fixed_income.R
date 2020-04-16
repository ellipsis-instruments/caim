#' Calculates bond price
#'
#' @param yield annual yield to maturity, expressed as .05 = 5\%
#' @param coupon annual coupon, expressed as .05 = 5\%
#' @param maturity maturity in years
#' @param coupfreq number of coupons per year, default = 1
#' @param redemption redemption price, default = 100
#' @export
bond_price <- function(yield,coupon,maturity,coupfreq=1,redemption=100) {

  matmonths <- maturity * 12
  coupper <- coupon / coupfreq
  yieldper <- yield / coupfreq
  matpers <- coupfreq * matmonths / 12
  tail = matpers - floor(matpers)
  # if (!tail)
  #   numcoups = matpers
  # else
  #   numcoups = floor(matpers + 1)
  # let the above handle vectors
  numcoups <- (floor(matpers) + (tail > 0))

  #principal
  price <- redemption / ((1 + yieldper) ^ matpers)

  #coupons
  for (i in 1:length(numcoups)) {
    for (j in 1:numcoups[i]) {
      # if (!tail)
      #   m <- i
      # else
      #   m <- i - 1 + tail
      # let the above handle vectors
      m <- j - (tail[i] > 0) + tail[i]
      price[i] <- price[i] + ((coupper[i] * redemption)/((1 + yieldper[i])^m))
    }
  }

  #accrued
  # if (tail)
  #   price <- price - (redemption * coupper * (1 - tail))
  # let the above handle vectors
  price <- price - (tail > 0) * (redemption * coupper * (1 - tail))

  return(price)
}

#' Calculates Macaulay duration
#'
#' @param yield annual yield to maturity, expressed as .05 = 5\%
#' @param coupon annual coupon, expressed as .05 = 5\%
#' @param maturity maturity in years
#' @param coupfreq number of coupons per year, default = 1
#' @export
macaulay_duration <- function (yield, coupon, maturity, coupfreq) {
  # from fabozzi fixed income mathematics
  numpers <- maturity * coupfreq #number of coupons until maturity
  peryield <- yield / coupfreq
  percoup <- coupon / coupfreq
  H <- (100 * percoup * ((1 - 1/((1 + peryield)^numpers)) / peryield))/caim::bond_price(yield, coupon, maturity, coupfreq)
  macd <- ((1+peryield)/peryield)*H + ((peryield - percoup)/peryield) * numpers * (1 - H)
  macd <- macd / coupfreq #above is in coupon periods
  return(macd)
}

#' Calculates Modified duration
#'
#' @param yield annual yield to maturity, expressed as .05 = 5\%
#' @param coupon annual coupon, expressed as .05 = 5\%
#' @param maturity maturity in years
#' @param coupfreq number of coupons per year, default = 1
#' @export
modified_duration <- function (yield, coupon, maturity, coupfreq) {
  return (caim::macaulay_duration(yield, coupon, maturity, coupfreq)/(1 + yield/coupfreq))
}
