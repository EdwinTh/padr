#' Retrieve the closest given Weekday
#'
#' When applying \code{thicken} with the interval "week", use this function to
#' automatically start at the first weekday befor \code{min(datetime_var)}.
#' @param wday Integer in the range 1-7 specifying the desired weekday start
#' (1 = Sun, 2 = Mon, 3 = Tue, 4 = Wed, 5 = Thu, 6 = Fri, 7 = Sat).
#' @return Object of class `Date`, specifying the offset.
#' @examples
#' library(dplyr)
#' coffee %>% thicken("week", start_val = get_week_start())
#' coffee %>% thicken("week", start_val = get_week_start(3))
#' @export

get_week_start <- function(wday = 2) {
  stopifnot(wday %in% 1:7)
  attributes(wday) <- list(class = "weekstart")
  return(wday)
}

get_week_start_internal <- function(wday,
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

  ret_value <- dt_var_start - as.numeric(nr_days_back)
  names(ret_value) <- NULL

  return(ret_value)
}
