#' Shift to the middle of each interval
#'
#' After thickening all the values are either
#' shifted to the first or the last value of their interval.
#' This function creates a vector from \code{x}, with the values shifted to
#' the (approximate) center of the interval. This can give a more accurate
#' picture of the aggregated data when plotting.
#' @param x A vector of class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' @param shift "up" or "down".
#' @param interval The interval to be used for centering. If \code{NULL},
#' \code{get_interval} will be applied on \code{x}.
#' @return
#' Vector of the same class as \code{x}, with the values shifted to the
#' (approximate) center.
#' @details The interval will be translated to number of days when
#' \code{x} is of class \code{Date}, or number of seconds when \code{x} is of
#' class \code{POSIXt}. For months and quarters this will be the average
#' length of the interval. The translated units divided by two
#' will be added by or subtracted from each value of \code{x}.
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' plot_set <- emergency %>%
#'   thicken("hour", "h") %>%
#'   count(h) %>%
#'   head(24)
#'
#' ggplot(plot_set, aes(h, n)) + geom_col()
#'
#' plot_set %>%
#'   mutate(h_center = center_interval(h)) %>%
#'   ggplot(aes(h_center, n)) + geom_col()
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

  interval_units <- int_to_units(x, interval_x)

  if (shift == "up") {
    x + (interval_units / 2)
  } else {
    x - (interval_units / 2)
  }
}

# x an object of class interval
int_to_secs <- function(x) {
  day_secs <- 3600 * 24
  secs_string <- c(year = day_secs * 365, quarter = day_secs * 365 / 4,
                   month = day_secs * 365 / 12, week = day_secs * 7,
                   day = day_secs, hour = 3600, min = 60, sec = 1)
  ret <- secs_string[x$interval] * x$step
  unname(ret)
}

# x an object of class interval
int_to_days <- function(x) {
  days_string <- c(year = 365, quarter = 365 / 4, month = 365 / 12, week = 7, day = 1)
  ret <- days_string[x$interval] * x$step
  unname(ret)
}

# x an object of class interval
int_to_units <- function(x, interval_x) {
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

#' Make a period character vector
#'
#' After applying \code{thicken} all the observations of a period are mapped
#' to a single time point. This function will convert a datetime variable to
#' a character vector that reflects the period, instead of a single time point.
#' \code{strftime} is used to format the start and the end of the interval.
#' @param x A vector of class \code{Date}, \code{POSIXct} or \code{POSIXlt},
#' of which the values are unique.
#' @param start_format String to format the start values of each period, to be used
#' in \code{strftime}.
#' @param end_format String to format the end values of each period, to be used
#' in \code{strftime}.
#' @param sep Character string that separates the \code{start_format} and the
#' \code{end_format}.
#' @param end_offset Units in days if \code{x} is \code{Date}, or in seconds if
#' \code{x} is \code{POSIXct} or \code{POSIXlt}.
#' Will be subtracted from the end of each period.
#' If 0, the end of the previous period is equal to the start of the next.
#' @param units_to_last To determine the formatting of the last value in \code{x},
#' the length of the last period has to be specified. If \code{NULL} the
#' function guesses based on the interval of \code{x}.
#' Specify in days when \code{x} is \code{Date}, or in seconds when \code{x} is
#' \code{POSIXct} or \code{POSIXlt}.
#' @return A character vector showing the interval.
#' @details The end of the periods will be determined by the next unique value
#' in \code{x}. It does so without regarding the interval of \code{x}. If a specific
#' interval is desired, \code{thicken} and / or \code{pad} should first be
#' applied to create an equally spaced datetime variable.
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' plot_set <- emergency %>%
#'   head(500) %>%
#'   thicken("hour", "h") %>%
#'   count(h)
#'
#' # this will show the data on the full hour
#' ggplot(plot_set, aes(h, n)) + geom_col()
#'
#' # adding a character to indicate the hours of the interval.
#' plot_set %>%
#'   mutate(h_int = format_interval(h, "%H", sep = "-"))
#'@export
format_interval <- function(x,
                            start_format  = "%Y-%m-%d",
                            end_format    = start_format,
                            sep           = " ",
                            end_offset    = 0,
                            units_to_last = NULL) {
  stop_not_datetime(x)
  stopifnot(length(x) == length(unique(x)))
  stopifnot(length(x) > 1)
  original_order <- order(x)

  if (is.null(units_to_last)) {
    units_to_last <- get_units_to_last(x)
  }

  end_vals   <- find_next_val(x, units_to_last) - (end_offset)
  start_char <- strftime(x, start_format, tz = attr(x, "tzone"))
  end_char   <- strftime(end_vals, end_format, tz = attr(x, "tzone"))
  ret <- paste(start_char, end_char, sep = sep)
  ret[original_order]
}

# x is a datetime variable of which we need to find the next value of each instance
find_next_val <- function(x,
                          fin_val_units) {
  n         <- length(x)
  x_srt     <- sort(x)
  ret       <- x_srt[2:n]
  fin_val   <- ret[n - 1] + fin_val_units
  ret_compl <- c(ret, fin_val)
  # by using c() the vector is changed to the tz of the locale! change back
  attr(ret_compl, "tzone") <- attr(ret, "tzone")
  ret_compl
}

get_units_to_last <- function(x) {
  interval <- get_interval(x)
  interval_x <- convert_interval(interval)
  int_to_units(x, interval_x)
}
