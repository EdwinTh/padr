#' Change datetime Granularity
#'
#' This function will take a datetime vector of class and changes its
#' granularity to a higher level that is provided.
#' @details The granularity is moved to a higher level by 'rounding' \code{x}
#' to the closest instance of the provided \code{interval}
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
#' @param interval The desired interval of the output, which should be higher
#' the interval of \codes{x}. Current choices are \code{year, quarter, month,
#' day, hour, minute}.
#' @param rounding = closest Should \code{x} be rounded to the \code{closest}
#' instance of \code{interval} or should it be rounde \code{up} or \down
#' @param allow_duplicates Logical indicating if we can have multiple instances
#' of the same value in return. If FALSE the value in \code{x} closest to the
#' return time point will be selected. Will throw an error if FALSE and two or
#' more timpe points are at the exact same distance.
#' @return A \code{data.frame} with two coluns, ther original vector \code{x}.
#' And a vector of the same class as \code{x} with the datetime points to which
#' \code{x} should be mapped to thicken it.  Datetime levels that are
#' lower in granularity than \code{interval} will be set to their first
#' possible value (month = 01, day = 01, hour = 00, second = 00, minute = 00).
#' @examples
#'

thickr <- function(x = date_seq('month'),
                   interval = c('year',
                                'quarter',
                                'month',
                                'day',
                                'hour',
                                'minute'),
                   rounding = c('closest',
                                'up',
                                'down'),
                   ...) {
  if(c('Date', "POSIXt") %in% class(x) %>% any %>% not) {
    stop('x should be of class Date, POSIXct, or POSIXlt', call. = FALSE)
  }
  interval <- match.arg(interval)
  rounding <- match.arg(rounding)

  outer(x, y, `-`) %>% abs

  # start by spanning the interval
  if(interval == 'year') {

  }

}

y = seq(as.POSIXct(strftime('2015-01-01')),
        as.POSIXct(strftime('2016-01-01')), by = 'year')




