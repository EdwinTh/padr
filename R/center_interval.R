#' Shift to the Middle of the Interval
#'
#' After padding and thickening all the values are either
#' shifted to the first or the last value of the interval.
#' Function creates a vector that shifts the datetime value to
#' the (approximate) center of the interval.
#' @param x A vector of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' @param shift Up or down.
#' @param return `x` with the values shifted to the (approximate) center.
#' @details The interval will be translated to number of days when
#' x is of class `Date``, or number of seconds when x is of class
#' `POSIXt`. For months and quarters will this be the average
#' length of the period. The floor of the translated number divided by two
#' and added by or subtracted from `x`.
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
center_interval <- function(x, shift = c("up", "down")) {
  stop_not_datetime(x)
  shift <- match.arg(shift)
  interval_x <- get_interval_list(x)
  if (inherits(x, "Date")) {
    interval_units <- int_to_secs(x)
  } else {
    interval_units <- int_to_days(x)
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
                   monht = day_secs*365/12, week = day_secs*7,
                   day = day_secs, hour = 3600, min = 60, sec = 1)
  secs_string[x$interval] * x$step
}

int_to_days <- function(x) {
  days_string <- c(year = 365, quarter = 365/4, month = 365/12, week = 7, day = 1)
  days_string[x$interval] * x$step
}

