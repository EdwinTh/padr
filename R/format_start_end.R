#' Add a Character of the Interval
#'
#' Format a date variable to a character string of the interval in which the
#' events toke place.
#' @param x A vector of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' @start_format String to format the start values of each period, to be used
#' in `strftime`.
#' @end_format String to format the end values of each period, to be used
#' in `strftime`.
#' @sep Character string that separates the `start_format` and the `end_format`.
#' @end_is_start If TRUE the start value of the next period, is the end value
#' of the previous period.
#' @return `x` with a character column added.
#' @details The end of the periods will be determined by the next unique value
#' in `x`. It does so without regarding the interval of `x`. If a specific
#' interval is desired, `thicken` and / or `pad` should first be applied.
#' @examples
#'
format_start_end <- function(x,
                             start_format = "%Y-%m-%d",
                             end_format   = start_format,
                             sep          = " ",
                             end_is_start = TRUE,
                             final_value  = get_interval(x)) {
  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
  } else {
    dt_var <- check_data_frame(x)
  }

  end_vals   <- find_ends_dt_var(dt_var, final_value) - (1 - end_is_start)
  start_char <- strftime(dt_var, start_format)
  end_char   <- strftime(end_vals, end_format)
  paste(start_char, end_char, sep = sep)
}


# x is a datetime variable of which we need to find the next value of each instance
find_ends_dt_var <- function(x,
                             interval) {

  map_func <- function(x, i) seq(x, length.out = 2, by = i)[2]
  x_df <- data.frame(x = x)
  x_df_with_end <- dplyr::mutate(dplyr::rowwise(x_df),
                                 end = map_func(x, interval))
  x_df_with_end$end
}
