#' From Datetime to Interval character
#'
#' Format a date variable to a character string of the interval in which the
#' events toke place.

# four functions: date, quarter, week,  posix
interval_format_date <- function(x,
                                 start_format       = "%Y-%m-%d",
                                 end_format         = "%Y-%m-%d",
                                 sep                = " ",
                                 last_is_first      = TRUE,
                                 colname            = NULL,
                                 by                 = NULL,
                                 check_completeness = TRUE,
                                 break_above        = 1
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


}

# apply pad on the datetime variable to see if its complete. x is a variable
check_completeness_func <- function(x,
                                    interval) {
  check_df <- data.frame(x = x, ind = 1)
  check_df_padded <- suppressMessages(pad(check_df,
                                      interval = interval))
  if (any(is.na(check_df_padded$ind))) {
    stop_message <-
sprintf("Datetime variable is incomplete on interval %s.
If you want to format a complete datetime variable, apply pad first.
Otherwise rerun this function with check_completeness = FALSE.",
                       interval)
    stop(cat(stop_message))
  }
}

