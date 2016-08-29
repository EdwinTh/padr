#' Change datetime Granularity
#'
#' This function will take a datetime vector of class and changes its
#' interval level to the provided level.
#' @details The interval is moved to a higher level by 'rounding' \code{x}
#' to the closest instance of the provided \code{interval}
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
#' @param interval The desired interval of the output, which should be higher
#' the interval of \codes{x}. Current choices are \code{year, month, day,
#' hour, minute}.
#' @param rounding = closest Should \code{x} be rounded to the \code{closest}
#' instance of \code{interval} or should it be rounde \code{up} or \code{down}?
#' @param allow_duplicates Logical indicating if we can have multiple instances
#' of the same value in the return \code{data.frame}.
#' If FALSE the value in \code{x} closest to the
#' return time point will be selected. Will throw an error if FALSE and two or
#' more timpe points are at the exact same distance of an \code{interval} value.
#' @return A \code{data.frame} with two colums, the original vector \code{x}
#' and a vector of the same class as \code{x} with the datetime points to which
#' \code{x} should be mapped to thicken it.  Datetime levels that are
#' lower in granularity than
#' possible value (month = 1, day = 1, hour = 0, second = 0, minute = 0).
#' @examples
#'
thicken <- function(x,
                   interval = c('year',
                                'month',
                                'day',
                                'hour',
                                'minute'),
                   rounding = c('closest',
                                 'up',
                                 'down'),
                   allow_duplicates = TRUE) {

  if(c('Date', "POSIXt") %in% class(x) %>% any %>% not) {
    stop('x should be of class Date, POSIXct, or POSIXlt', call. = FALSE)
  }

  interval <- match.arg(interval)
  rounding <- match.arg(rounding)

  # start by spanning the interval
  # here assign one of the span functions based on interval to the main.

  # TODO figure out how to parse this to get rid off the if else
  # span_function('span', interval, sep = '_')
  if(interval == 'year') {
    span <- span_year(x) %>% as.POSIXlt
  } else if (interval == 'month') {
    span <- span_month(x) %>% as.POSIXlt
  } else if (interval == 'day') {
    span <- span_day(x) %>% as.POSIXlt
  } else if (interval == 'hour') {
    span <- span_hour(x) %>% as.POSIXlt
  } else if (interval == 'minute') {
    span <- span_minute(x) %>% as.POSIXlt
  } else {
    stop("Not reach span_function if else")
  }

  # TO DO sort out how to enforce a time to be midnight in local tz, instead of in GMT
  hour_dif <- outer(x, span, function(y,z) y-z)

  if(rounding == 'down') {
    hour_dif[hour_dif < 0]  <- Inf
  } else if (rounding == 'up') {
    hour_dif[hour_dif > 0]  <- Inf
    hour_dif <- abs(hour_dif)
  } else if (rounding == 'closest') {
    hour_dif <- abs(hour_dif)
  } else {
    stop('Not reach, bug in the rounding')
  }

  closest <- hour_dif %>% apply(1, which.min)
  closest_dif <- mapply(function(i,j){ hour_dif[i,j]},
                        1:nrow(hour_dif),
                        closest)
  return_frame <- data.frame(
    x           = x,
    thickened   = span[closest],
    x_thickened_dif = closest_dif
  )

  if(allow_duplicates %>% not) {
    return_frame_no_dups <-
      return_frame %>%
      dplyr::group_by(thickened) %>%
      dplyr::mutate(closest_dif = min(x_thickened_dif)) %>%
      dplyr::filter(x_thickened_dif == closest_dif)

    thickened_count <-
      return_frame_no_dups %>%
      dplyr::group_by(thickened) %>%
      dplyr::mutate(nr_of_thickened = n())
    still_dups <-
      thickened_count %>%
      ungroup %>%
      dplyr::select(nr_of_thickened) %>%
      unlist %>%
      magrittr::is_greater_than(1) %>%
      any

    if(still_dups) {
      return_frame <-
        thickened_count %>%
        dplyr::filter(nr_of_thickened > 1) %>%
        select(x, thickened)
      print(return_frame)
      stop(
cat("Could not remove duplicates on thickened level.
The distance of at least two values of x is exactly equal to the thickened date.
See the above datapoints."))
    } else {
      return_frame <- return_frame_no_dups
    }
  }
  return_frame %>% dplyr::select(x, thickened) %>% as.data.frame
}





