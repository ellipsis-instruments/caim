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

#' Returns last date in each year for date series
#' @export
year_end_dates <- function(dates) {
  dates <- as.character(dates)
  return(lubridate::as_date(tapply(dates, lubridate::year(dates), max)))
}

#' Checks to see whether a character string can be formatted as YYYY-MM-DD
#' @export
is_ymd <- function(date) {
  grepl("[12]\\d{3}-(0[1-9]|1[0-2])-(0[1-9]|[12]\\d|3[01])", date)
}

#' Finds earliest dates of data series in long data.table
#' @export
min_dates <- function(data, keycols, datecol="date") {
  # format data as data.table
  data <- as.data.table(data)
  # format date column as date
  data[ , ..datecol := lubridate::as_date(get(datecol))]
  # find minimum date for each series
  data[
    , .(min_date=min(get(datecol)))
    , keyby=c(keycols)
  ]
}

#' Finds latest dates of data series in long data.table
#' @export
max_dates <- function(data, keycols, datecol="date") {
  # format data as data.table
  data <- as.data.table(data)
  # format date column as date
  data[ , ..datecol := lubridate::as_date(get(datecol))]
  # find maximum date for each series
  data[
    , .(max_date=max(get(datecol)))
    , keyby=c(keycols)
  ]
}

#' Finds date range of data series in long data.table
#' @export
date_range <- function(data, keycols, datecol="date") {
  # format data as data.table
  data <- as.data.table(data)
  # format date column as date
  data[ , ..datecol := lubridate::as_date(get(datecol))]
  # find maximum date for each series
  data[
    , .(
      min_date = min(get(datecol))
      , max_date = max(get(datecol))
    )
    , keyby=c(keycols)
  ]
}

#' Extracts month end series from long data.table
#' @export
monthly_data <- function(data, keycols, datecol = "date") {
  # ensure data is data.table
  data <- as.data.table(data)
  # ensure date column is in date format
  data[ , (datecol) := lubridate::as_date(get(datecol))]
  # find freshest date in data
  max_date <- lubridate::as_date(data[ , max(get(datecol))])
  # set up column name selection vectors
  cols <- c(keycols, datecol)
  monthcols <- c(keycols, "year", "month")
  monthly_dates <- data[
    , ..cols
  ][
    # extract year info from date
    , year := lubridate::year(get(datecol))
  ][
    # extract month info from date
    , month := lubridate::month(get(datecol))
  ][
    # get maximum date in each month
    # NOTE: these might not be the same for each series
    , .(date=max(get(datecol)))
    , by=c(monthcols)
  ][
    # we don't need year and month columns anymore
    , !c("year", "month")
  ][
    # join monthly_dates with data
    data
    ,
    , on=(cols)
    , nomatch=0 # makes this an inner join
  ][
    # set all dates to last calendar date in month
    , (datecol) := lubridate::ceiling_date(get(datecol), "month") - 1
  ][
    # limit latest month's date to last available mtd date
    date > max_date
    , (datecol) := max_date
  ]
}
