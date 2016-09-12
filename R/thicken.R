#' Change datetime Granularity
#'
#' This function will take a datetime vector of class and changes its
#' interval level to the provided level.
#' @details The interval is moved to a higher level by 'rounding' \code{x}
#' to the closest instance of the provided \code{interval}
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct},
#' or a \code{data.frame} containing at least on column of these classes.
#' @param interval The desired interval of the output, which should be higher
#' then the interval of the datetime variable. Current choices are
#' \code{year, month, day, hour, minute}.
#' @param rounding = closest Should \code{x} be rounded \code{down} or \code{up}?
#' @param by If \code{x} is a \class{data.frame} and contains multiple datetime
#' variables, specify which column to pad by.
#' @param start_valChange the default start_valpoint of the time range to which
#' the datetime variable is thickened. See ?span_year for more information.
#' @param end_valChange the default end_valpoint of the time range to which
#' the datetime variable is thickened. See ?span_year for more information.
#' @return The original \code{data.frame x} with the thickened variable added
#' to it. If \code{x} is a datetime vector the return will be a
#' \code{data.frame} comprising \code{x} and the thickened variable.
#' @examples
#' X <- data.frame(day_var = seq(as.Date('2016-01-01'), as.Date('2016-12-31'), by = 'day'),
#'                 value   = runif(366, 50, 100))

thicken <- function(x,
                   interval = c('year',
                                'month',
                                'day',
                                'hour',
                                'minute'),
                   rounding = c('down',
                                 'up'),
                   by       = NULL,
                   start_val= NULL,
                   end_val  = NULL) {

  interval <- match.arg(interval)
  rounding <- match.arg(rounding)

  # Section 1: obtain datetime variable and see if the variable is valid
  arguments <- as.list(match.call())

  if(is.data.frame(x)){
    original_data_frame <- x
    x <- as.data.frame(x)

    if(!is.null(arguments$by)) {
      if(length(arguments$by) > 1) stop('by can indicate one variable only')
      dt_var  <- eval(arguments$by, x)
    } else {
      dt_var_name <- get_date_variables(x)
      if(length(dt_var_name) == 0) {
        stop('x does not contain a variabel of class Date, POSIXct, or POSIXlt',
             call. = FALSE)
      }
      if(length(dt_var_name) > 1){
        stop('x contanis multiple variables of class Date, POSIXct, or POSIXlt,
please specify which variable to use in the by argument',
             call. = FALSE)
      }
      dt_var <- x[ ,colnames(x) == dt_var_name]
    }
  } else {
    if( !( c('Date', "POSIXt") %in% class(x) %>% any) ) {
      stop('x should be a data.frame or a vector of class Date, POSIXct, or POSIXlt', call. = FALSE)
    }
    dt_var <- x
  }

  # Section 2: span a variable with all the relevant instances of interval
  int_hierarchy <- 1:6
  names(int_hierarchy) <- c('year','month','day','hour','minute', 'second')
  if(int_hierarchy[get_interval(dt_var)] < int_hierarchy[interval]) {
    stop('The interval in the datetime variable is larger than the interval given,
you might be looking fo smear rather than for thicken.')
  } else if (int_hierarchy[get_interval(dt_var)] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
you might be looking for pad rather than for thicken.')
  }

  if(interval == 'year') {
    span <- span_year(dt_var, start_val, end_val)
  } else if (interval == 'month') {
    span <- span_month(dt_var, start_val, end_val)
  } else if (interval == 'day') {
    span <- span_day(dt_var, start_val, end_val)
  } else if (interval == 'hour') {
    span <- span_hour(dt_var, start_val, end_val)
  } else if (interval == 'minute') {
    span <- span_minute(dt_var, start_val, end_val)
  } else {
    stop("Not reach span_function if else")
  }

  # Section 3: make the thicken and create the return frame
  if(rounding == 'down'){
    thickened <- round_down(dt_var, span)
  } else {
    thickened <- round_up(dt_var, span)
  }

  if(is.data.frame(x)) {
    ## this throws an error for tbl_df >> still to do
    original_data_frame$thickened <- thickened
    return(original_data_frame)
  } else {
    return_frame <- data.frame(dt_var, thickened)
    colnames(return_frame)[1] <- as.character(arguments$x)
    return(return_frame)
  }
}



