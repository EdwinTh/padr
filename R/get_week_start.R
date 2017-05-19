#' Retrieve the closest given Weekday
#'
#' By default weeks start at Sundays when thickening to the interval "week".
#' For a different week start the offset should be specified at `start_val`.
#' This function will retrieve the offset for you, by searching the latest
#' requested weekdy before the first observation in the datetime variabe in `x`.
#'
#' @param x A dataframe containg a variable of class `Date`, class `POSIXct` or class
#' `POSIXlt` indicating the moment from which to calculate the offset.
#' If the length is longer than 1 the `min(x)` will be used.
#' @param wday Integer in the range 1-7 specifying the desired weekday start
#' (1 = Sunday, 7 = Saturday).
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt}. \code{by} indicates which variable to use for det.
#' @return Object of class `Date`, specifying the offset.
#' @examples
#' library(dplyr)
#' coffee %>% get_week_start()
#' thicken(coffee$ti)

get_week_start <- function(x,
                           wday = 2,
                           by   = NULL) {
  is_df(x)
  stopifnot(wday %in% 1:7)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
  } else {
    dt_var <- check_data_frame(x)
  }

  dt_var_start <- min(as.Date(dt_var))

  lookup <- 1:7
  names(lookup) <- weekdays( seq(as.Date("2017-05-21"), length.out = 7,
                                 by = "day"))

  current_wday_nr <- lookup[weekdays(dt_var_start)]
  nr_days_back    <- current_wday_nr - wday
  if (nr_days_back < 0) {
    nr_days_back <- nr_days_back + 7
  }

  return(dt_var_start - nr_days_back)
}
