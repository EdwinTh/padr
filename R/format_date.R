#' From Datetime to Interval character
#'
#' Format a date variable to a character string of the interval in which the
#' events toke place.

# four functions: date, quarter, week,  posix

format_date <- function(x,
                        year_part  = NULL,
                        month_part = NULL,
                        day_part   = NULL,
                        str_sep    = " ",
                        overlap    = FALSE,
                        interval   = FALSE,
                        colname    = NULL,
                        by         = NULL) {
  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
  } else {
    dt_var <- check_data_frame(x)
  }

  # step 1: determine the from-to
  dt_var

  # step 2: format these two dates in the desired way

}

library(dplyr)

emergency %>% thicken("month", start_val = as.Date("2015-11-24")) %>%
  count(time_stamp_month)
