#' Change datetime interval up
#'
#' This function will take a datetime vector of class code{Date}, \code{POSIXlt}, or \code{POSIXct}
#' and changes its interval up level to the provided level.
#' @details The interval is moved to a higher level by 'rounding' \code{x}
#' to the closest instance of the provided \code{interval}
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct},
#' or a \code{data.frame} containing at least one column of these classes.
#' @param interval The desired interval of the output, which should be higher
#' then the interval of the datetime variable. Current choices are
#' \code{year, month, day, hour, minute}.
#' @param rounding = closest Should \code{x} be rounded \code{down} or \code{up}?
#' @param by If \code{x} is a \class{data.frame} and contains multiple datetime
#' variables, specify which column to pad by.
#' @param start_val Change the default start_valpoint of the time range to which
#' the datetime variable is thickened. See ?span_year for more information.
#' @param end_val Change the default end_valpoint of the time range to which
#' the datetime variable is thickened. See ?span_year for more information.
#' @return The original \code{data.frame x} with the thickened variable added
#' to it. If \code{x} is a datetime vector the return will be a
#' \code{data.frame} comprising \code{x} and the thickened variable.
#' @examples
#' X <- data.frame(day_var = seq(as.Date('2016-01-01'), as.Date('2016-12-31'), by = 'day'),
#'                 value   = runif(366, 50, 100))
#' @export

thicken <- function(x,
                   interval = c('year',
                                'month',
                                'day',
                                'hour',
                                'min'),
                   rounding = c('down',
                                 'up'),
                   by       = NULL,
                   start_val= NULL,
                   end_val  = NULL) {

  # Section 1: obtain datetime variable and see if the variable is valid

  arguments <- as.list(match.call())
  if('by' %in% names(arguments)) by_val <- as.character(arguments$by)

  if(is.data.frame(x)) {
    original_data_frame <- x
    x <- as.data.frame(x)
    if('by' %in% names(arguments)){
      dt_var <- check_data_frame(x, by = by_val)
    } else {
      dt_var <- check_data_frame(x)
    }
  } else {
    dt_var <- check_vector(x)
  }

  interval <- match.arg(interval)
  rounding <- match.arg(rounding)

  # Section 2: span a variable with all the relevant instances of interval
  int_hierarchy <- 1:6
  names(int_hierarchy) <- c('year','month','day','hour','min', 'sec')
  if(int_hierarchy[get_interval(dt_var)] < int_hierarchy[interval]) {
    stop('The interval in the datetime variable is lower than the interval given,
you might be looking fo smear rather than for thicken.')
  } else if (int_hierarchy[get_interval(dt_var)] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
you might be looking for pad rather than for thicken.')
  }

  if(!all(dt_var[1:(length(dt_var)-1)] < dt_var[2:length(dt_var)])) {
    warning('Datetime variable was unsorted, result will be unsorted as well.')
  }

  if(interval == 'year') {
    span <- span_year(dt_var, start_val, end_val)
  } else if (interval == 'month') {
    span <- span_month(dt_var, start_val, end_val)
  } else if (interval == 'day') {
    span <- span_day(dt_var, start_val, end_val)
  } else if (interval == 'hour') {
    span <- span_hour(dt_var, start_val, end_val)
  } else if (interval == 'min') {
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
    original_data_frame$thickened <- thickened
    return(original_data_frame)
  } else {
    return_frame <- data.frame(dt_var, thickened)
    colnames(return_frame)[1] <- as.character(arguments$x)
    return(return_frame)
  }
}

