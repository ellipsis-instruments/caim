#' Returns last date in each month for date series
#'
#' @param dates vector of dates in "YYYY-MM-DD" format
#' @export
month_end_dates <- function(dates) {
  dates <- as.character(dates)
  return(lubridate::as_date(tapply(dates, substr(dates, 1, 7), max)))
}

#' Returns last date in each quarter for date series
#' @export
quarter_end_dates <- function(dates) {
  dates <- as.character(dates)
  return(lubridate::as_date(tapply(dates, lubridate::quarter(dates, with_year=T), max)))
}

#' Returns first date in each year for date series
#' @export
year_start_dates <- function(dates) {
  dates <- as.character(dates)
  return(lubridate::as_date(tapply(dates, lubridate::year(dates), min)))
}

#' Returns first date in each year for date series
#' @export
year_end_dates <- function(dates) {
  dates <- as.character(dates)
  return(lubridate::as_date(tapply(dates, lubridate::year(dates), max)))
}
