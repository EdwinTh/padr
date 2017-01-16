#' Get the interval of a datetime variable.
#'
#' The interval is the highest time unit that can explain all instances of a
#' variable of class \code{Date}, class \code{POSIXct}, or class \code{POSIXct}.
#' This function will determine what the interval of the variable is.
#' @param x A variable of class of class \code{Date} or of class \code{POSIXt}.
#' @return A character string indicating the interval of \code{x}.
#' @details See \code{vignette("padr")} for more information on intervals.
#' @examples
#' x_month <- seq(as.Date('2016-01-01'), as.Date('2016-05-01'), by = 'month')
#' get_interval(x_month)
#'
#' x_sec <- seq(as.POSIXct('2016-01-01 00:00:00'), length.out = 100, by = 'sec')
#' get_interval(x_sec)
#' @export
get_interval <- function(x) {

  if ( !( inherits(x, 'Date') |  inherits(x, 'POSIXt')) ) {
    stop('x should be of class Date, POSIXct, or POSIXlt')
  }

  x_char <- datetime_char(x)

  differ <- lowest_differ(x_char)

  if ( length(differ) == 0 ) {
    stop("x does not vary, cannot determine the interval", call. = FALSE)
  }

  if (differ == 'month') {
    if (is_month_quarter(x_char)) differ <- 'quarter'
  }

  if (differ == 'day') {
    if (is_day_week(x_char)) differ <- 'week'
  }

  return(differ)
}

# change a variable of class Date or POSIXt to a character of length 18
# as input for the differing
datetime_char <- function(x) {
  x_char <- as.character(x)
  if (unique(nchar(x_char)) == 10){
    x_char <- paste(x_char, '00:00:00')
  }
  return(x_char)
}

# check what levels of the datetime variable differ, x is the output of datetime_char
lowest_differ <- function(x_char) {
  differ <- which(c(
    year   = ! length( unique ( substr(x_char, 1, 4) ) ) == 1,
    month  = ! length( unique ( substr(x_char, 6, 7) ) ) == 1,
    day    = ! length( unique ( substr(x_char, 9, 10) ) ) == 1,
    hour   = ! length( unique ( substr(x_char, 12, 13) ) ) == 1,
    min    = ! length( unique ( substr(x_char, 15, 16) ) ) == 1,
    sec    = ! length( unique ( substr(x_char, 18, 19) ) ) == 1
  ))
  return( names( differ[length(differ)] ) )
}

# using the lowest_differ we cannot detect quarter and week
# if the interval is month we look for quarter (quarter is special case of month)
is_month_quarter <- function(x_char) {
  m <- as.POSIXlt(x_char)$mon
  all(m %in% c(1, 4, 7, 10)) | all(m %in% c(2, 5, 8, 11)) | all(m %in% c(0, 3, 6, 9))
}

# if the interval is day we we will look for week
is_day_week <- function(x_char){
  all_weeks <- seq( as.POSIXlt(min(x_char), tz = 'UTC'),
                    as.POSIXlt(max(x_char), tz = 'UTC'),
                    by = '7 DSTdays')
  x_posix <- as.POSIXlt(x_char, tz = 'UTC')
  all(as.numeric(x_posix) %in% as.numeric(all_weeks))
}
