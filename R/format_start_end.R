#' Make a Period Character vector
#'
#' After applying `thicken` all the observations in an interval are mapped
#' to a single timepoint. This function will conver a datetime variable to
#' a character vector that reflects the full period.
#' @param x A vector of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt},
#' of which the values are unique.
#' @param start_format String to format the start values of each period, to be used
#' in `strftime`.
#' @param end_format String to format the end values of each period, to be used
#' in `strftime`.
#' @param sep Character string that separates the `start_format` and the `end_format`.
#' @param end_offset The offset for the end of the period. If 0 the end of the last
#' period is the start of the next period. In days if x is `Date`, in seconds
#' if x is `POSIXt`.
#' @return A character vector showing the interval.
#' @details The end of the periods will be determined by the next unique value
#' in `x`. It does so without regarding the interval of `x`. If a specific
#' interval is desired, `thicken` and / or `pad` should first be applied to
#' create an equally spaced datetime variable.
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
#'   mutate(h_int = format_start_end(h, "%H", sep = "-"))
format_start_end <- function(x,
                             start_format = "%Y-%m-%d",
                             end_format   = start_format,
                             sep          = " ",
                             end_offset   = 0,
                             final_value  = get_interval(x)) {
  stop_not_datetime(x)
  stopifnot(length(x) == length(unique(x)))
  original_order <- order(x)
  fin_val_units <- int_to_units(x)
  end_vals   <- find_next_val(x, fin_val_units) - (end_offset)
  start_char <- strftime(x, start_format)
  end_char   <- strftime(end_vals, end_format)
  paste(start_char, end_char, sep = sep)
}

# x is a datetime variable of which we need to find the next value of each instance
find_next_val <- function(x,
                          fin_val_units) {
  n         <- length(x)
  x_srt     <- sort(x)
  ret       <- x_srt[2:n]
  fin_val   <- return_vec[n-1] + fin_val_units
  ret_compl <- c(ret, fin_val)
  # by using c() the vector is changed to the tz of the locale! change back
  attr(ret_compl, "tzone") <- attr(ret, "tzone")
  ret_compl
}

