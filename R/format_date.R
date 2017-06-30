#' From Datetime to Interval character
#'
#' Format a date variable to a character string of the interval in which the
#' events toke place.

# four functions: date, quarter, week,  posix
x <- emergency %>% thicken("month") %>% count(time_stamp_month)

interval_format_date <- function(x,
                                 start_format       = "%Y-%m-%d",
                                 end_format         = "%Y-%m-%d",
                                 sep                = " ",
                                 last_is_first      = TRUE,
                                 interval           = NULL,
                                 colname            = NULL,
                                 by                 = NULL,
                                 check_completeness = TRUE) {
  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
  } else {
    dt_var <- check_data_frame(x)
  }

  if (is.null(interval)) {
    interval_dt_var <- get_interval(dt_var)
    if (interval_dt_var[[1]] == "return x here") {
      return(x)
    }
  }

  if (check_completeness) {
    check_completeness_func(dt_var, interval)
  }
  # step 2: format these two dates in the desired way

}

