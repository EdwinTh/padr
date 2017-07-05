#' From Datetime to Interval character
#'
#' Format a date variable to a character string of the interval in which the
#' events toke place.

# four functions: date, quarter, week,  posix
interval_format_date <- function(x,
                                 start_format       = "%Y-%m-%d",
                                 end_format         = "%Y-%m-%d",
                                 sep                = " ",
                                 last_first_offset  = NULL,
                                 colname            = NULL,
                                 by                 = NULL,
                                 check_completeness = TRUE
                                 ) {
  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
  } else {
    dt_var <- check_data_frame(x)
  }

  interval <- get_interval(dt_var)

  if (check_completeness) {
    check_completeness_func(dt_var, interval)
  }

  end_vals <- find_ends_dt_var(dt_var, interval)
  start_char <- strftime(dt_var, start_format)
  end_char   <- strftime(end_vals, end_format)
  interval_chars <- data_frame(new = paste(start_char, end_char, sep = sep))
  return_frame   <- dplyr::bind_cols(x, interval_chars)

  if (is.null(by)) {
    x_name <- get_date_variables(x)
  } else {
    x_name <- by
  }



}

# apply pad on the datetime variable to see if its complete. x is a variable
check_completeness_func <- function(x,
                                    interval) {
  check_df <- data.frame(x = x, ind = 1)
  check_df_padded <- suppressMessages(pad(check_df,
                                      interval = interval,
                                      break_above = 10000))
  if (any(is.na(check_df_padded$ind))) {
    stop_message <-
sprintf("Datetime variable is incomplete on interval %s.
If you want to format a complete datetime variable, apply pad first.
Otherwise rerun this function with check_completeness = FALSE.",
                       interval)
    stop(cat(stop_message))
  }
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
