#' Retrieve the closest given Weekday
#'
#' By default weeks start at Sundays when thickening to the interval "week".
#' For a different week start the offset should be specified at `start_val`.
#' This function will retrieve the offset for you, by searching the latest
#' requested weekdy before the first observation in the datetime variabe in `x`.
#' Function to be used within `thicken` or `pad`.
#' @param wday Integer in the range 1-7 specifying the desired weekday start
#' (1 = Sunday, 7 = Saturday).
#' @param rounding Down or up.
#' @return Object of class `Date`, specifying the offset.
#' @examples
#' library(dplyr)
#' coffee %>% get_week_start()
#' thicken(coffee$ti)

get_week_start <- function(wday = 2,
                           rounding = c("down", "up")) {
  rounding <- match.arg(rounding)
  stopifnot(wday %in% 1:7)
  week_start_args <- list(wday, rounding)
  attributes(week_start_args) <- list(class = "weekstart")
  return(week_start_args)
}

get_week_start_internal <- function(wday,
                                    rounding,
                                    x,
                                    by) {
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

  ret_value <- dt_var_start - nr_days_back
  names(ret_value) <- NULL
  if (rounding == "up") {
    ret_value <- ret_value + 7
  }

  return(ret_value)
}

thicken(coffee, "week", start_val = get_week_start(2))
