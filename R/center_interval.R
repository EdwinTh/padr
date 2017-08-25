#' Shift to the Middle of the Interval
#'
#' After padding and thickening all the values are either
#' shifted to the first or the last value of the interval.
#' Function creates a vector that shifts the datetime value to
#' the (approximate) center of the interval.
#' @param x A vector of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' @param shift Up or down.
#' @param interval The interval to be used for centering. If `NULL`, `get_interval`
#' will be applied on `x`.
#' @param return `x` with the values shifted to the (approximate) center.
#' @details The interval will be translated to number of days when
#' x is of class `Date``, or number of seconds when x is of class
#' `POSIXt`. For months and quarters this will be the average
#' length of the period. The translated number divided by two
#' will be added by or subtracted from `x`.
#' @examples
#' library(tidyverse)
#' plot_set <- emergency %>%
#'   thicken("hour", "h") %>%
#'   count(h) %>%
#'   head(24)
#'
#' ggplot(plot_set, aes(h, n)) + geom_col()
#'
#' plot_set %>%
#'   mutate(h_center = center_interval(h)) %>%
#'   ggplot(plot_set, aes(h_center, n)) + geom_col()
#' @export
center_interval <- function(x,
                            shift    = c("up", "down"),
                            interval = NULL) {
  stop_not_datetime(x)
  shift <- match.arg(shift)
  if (is.null(interval)) {
    interval_x <- get_interval_list(x)
  } else {
    interval_x <- convert_interval(interval)
    interval_x$interval <- uniform_interval_name(interval_x$interval)
  }

  interval_units <- int_to_units(x)

  if (inherits(x, "Date")) {
    interval_units <- int_to_days(interval_x)
  } else {
    interval_units <- int_to_secs(interval_x)
  }

  if (shift == "up") {
    x + (interval_units / 2)
  } else {
    x - (interval_units / 2)
  }
}

int_to_secs <- function(x) {
  day_secs <- 3600 * 24
  secs_string <- c(year = day_secs*365, quarter = day_secs*365/4,
                   month = day_secs*365/12, week = day_secs*7,
                   day = day_secs, hour = 3600, min = 60, sec = 1)
  ret <- secs_string[x$interval] * x$step
  unname(ret)
}

int_to_days <- function(x) {
  days_string <- c(year = 365, quarter = 365/4, month = 365/12, week = 7, day = 1)
  ret <- days_string[x$interval] * x$step
  unname(ret)
}

int_to_units <- function(x) {
  if (inherits(x, "Date")) {
    int_to_days(interval_x)
  } else {
    int_to_secs(interval_x)
  }
}

unname <- function(x) {
  names(x) <- NULL
  x
}
