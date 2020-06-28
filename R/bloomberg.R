require(data.table)

#' Bloomberg history download
#' @export
bdh <- function(tickers, fields="PX_LAST", start_date="-7CD"
                , end_date=NULL
                , period="DAILY"
                , conn=NULL) {

  # format start_date as date if possible
  if (caim::is_ymd(start_date))
    start_date <- lubridate::as_date(start_date)

  # format end_date as date if possible
  if (!is.null(end_date)) {
    if (caim::is_ymd(end_date))
      end_date <- lubridate::as_date(end_date)
  }

  # create Bloomberg connection if one wasn't passed in
  if (is.null(conn))
    Rblpapi::blpConnect()

  # set up period
  opt <- c("periodicitySelection"=period)

  # we'll add "download time" to data
  date_added <- lubridate::now()

  # download historical data from Bloomberg
  bb_data <- Rblpapi::bdh(tickers, fields, start.date=start_date
                          , end.date=end_date, options=opt)

  # if only one ticker was passed in, format data so it can be processed
  # by rbindlist below
  if (length(tickers) == 1) {
    bb_data <- list(bb_data)
    names(bb_data) <- tickers
  }

  # format data into one data.table
  data.table::rbindlist(lapply(names(bb_data), function(x) {

    tbl <- data.table::data.table(bb_data[[x]])
    key_var <- "date"
    long_vars <- names(tbl)[names(tbl) != key_var]
    # convert wide to long
    data.table::melt(tbl, id.vars=key_var, measure.vars=long_vars)[
      , .(
        bb_ticker = x
        , date = lubridate::as_date(date)
        , field = variable
        , value
        , date_added = ..date_added
      )
    ]

  }))[
    , .SD
    # sort data
    , key=.(bb_ticker, field, date)
  ]

}

