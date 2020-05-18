#' Imports data from ICE historical data download .csv files
#'
#' From Bloomberg IND13
#' @param filenames character vector
#' @param field_key if NULL, function will use default values
#' @export
import_ice_historical_data <- function(filenames, field_key=NULL) {
  # load default field_key if necessary
  if (is.null(field_key)) {
    field_key <- rbindlist(list(
      data.table(ice_field = "Date", name="date")
      , data.table(ice_field = "TRR Index Val LOC U", name="index_val")
      , data.table(ice_field = "TRR Index Val LOC H", name="index_val_h")
      , data.table(ice_field = "Yld to Maturity", name="yield")
      , data.table(ice_field = "Effective Yield", name="eff_yield")
      , data.table(ice_field = "Maturity / WAL", name="maturity")
      , data.table(ice_field = "Modified Dur", name="mod_dur")
      , data.table(ice_field = "Eff Dur", name="eff_dur")
      , data.table(ice_field = "Spread Duration", name="spread_dur")
      , data.table(ice_field = "Eff Convexity", name="eff_convexity")
      , data.table(ice_field = "Govt OAS", name="govt_oas")
      , data.table(ice_field = "Full Market Value LOC U", name="mkt_value")

      , data.table(ice_field = "TRR Index Val USD U", name="index_val")
      , data.table(ice_field = "TRR Index Val USD H", name="index_val_h")
      , data.table(ice_field = "Full Market Value USD U", name="mkt_value")
    ))
  }

  # import data
  ice_import <- rbindlist(lapply(filenames, function(f) {
    # import file to data.table
    raw_data <- fread(f, fill=T)
    # find rows where new index information starts
    new_series_start_ix <- which(raw_data$V1 == "Index")
    # find rows where index information ends
    new_series_end_ix <- c(new_series_start_ix[-1] - 1, nrow(raw_data))
    # find column names of imported data
    ice_fields <- data.table(ice_field=unlist(raw_data[new_series_start_ix[1] + 1]))
    # rename data.table colnames to ice field names (otherwise colnames will remain V1, V2 ...)
    names(raw_data) <- unlist(ice_fields)
    # set valid column names (some imports might have an <NA> column at the end; we don't want that)
    valid_cols <- unlist(ice_fields[ice_fields$ice_field %in% field_key$ice_field])
    # retain only valid columns
    raw_data <- raw_data[ , ..valid_cols]
    # rename columns to names we've set up in field_key
    names(raw_data) <- field_key[ice_fields, on=.(ice_field), nomatch=F]$name

    # import data.table for each index and merge/rbindlist
    rbindlist(lapply(1:length(new_series_start_ix), function (i) {
      # index name is in column 2
      index <- raw_data[new_series_start_ix[i], 2]
      # currency is in column 4
      currency <- raw_data[new_series_start_ix[i], 4]
      # build data.table
      raw_data[
        # data starts on 3rd row
        (new_series_start_ix[i] + 2):new_series_end_ix[i]
        ][
          # filter out blank rows - they'll have blank dates
          date != ""
          ][
            # add index column
            , index := index
            ][
              # add currency column
              , currency := currency
              ][
                # ensure dates are as_date, not character()
                , date := {
                  # finds delimiter in date strings, limited now to / or -
                  delimiter <- ifelse(sum(grepl("/", date)) > sum(grepl("-", date)), "/", "-")
                  # if - assume date is in YYYY-MM-DD format
                  if (delimiter=="-")
                    lubridate::as_date(date)
                  # if / assume date is in M/D/YYYY format
                  else if (delimiter=="/")
                    lubridate::as_date(as.Date(date, format="%m/%d/%Y"))
                  # return an error value if delimiter is not / or -
                  else
                    -999
                }
                ][
                  # ensure data columns have numeric values, not character()
                  , lapply(.SD, as.numeric)
                  , key = .(index, date, currency)
                  ]
    }))
  }), fill=T)

  # find merge key column names. everything but the ones in the vector below
  cols <- names(ice_import)[!names(ice_import) %in% c("currency", "index_val", "index_val_h", "mkt_value")]
  # this essentially takes relevant USD data as a couple of columns and cbinds it to
  # the data for LOC for each index. This way, we don't have repetitive yields, dur etc.,
  # and simplifies the data to only one row per index and date, with relevant LOC and USD
  # data together in the same row
  if (nrow(ice_import[currency=="USD"]) > 0) {
    # we have USD data
    ice_import <- ice_import[currency=="LOC"][
      ice_import[currency=="USD"]
      , c("index_val_usd"
          , "index_val_h_usd"
          , "mkt_value_usd"
      ) :=
        .(i.index_val
          , i.index_val_h
          , i.mkt_value
        )
      , on=cols][ , !"currency"]
  } else {
    # we don't have USD data
    ice_import[
      , c(
        "index_val_usd"
        , "index_val_h_usd"
        , "mkt_value_usd"
      ) := .(
        NA
        , NA
        , NA
      )
      ]
  }
  # remove currency column, which should only have LOC, it's not needed
  # TODO: have database of indices and actual local currencies
  ice_import <- ice_import[ , !"currency"]

  return(ice_import)
}
