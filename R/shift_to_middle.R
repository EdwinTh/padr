library(dplyr)

# pad and get_inteval are currently broken (fixed in a version that is not yet checked in)
shift_to_middle <- function(x) {

}


# this function assumes that the interval is no higher than hour
# for day and higher it should be converted to date
shift_to_middle_posix <- function(x) {
  stopifnot(inherits(x, "POSIXt"))
  # interval_x <- get_interval_list(x)
  interval_x <- list(interval = "hour", step = 1)
  interval_x_secs <- int_to_secs(interval_x)
  x + interval_x_secs / 2
}

int_to_secs <- function(x) {
  secs_string <- c(hour = 3600, min = 60, sec = 1)
  secs_string[x$interval] * x$step
}


shift_to_middle_date <- function(x) {
  stopifnot(inherits(x, "Date"))
  # interval_x <- get_interval_list(x)
  interval_x <- list(interval = "day", step = 15)
  interval_x_days <- int_to_days(interval_x)
  x + interval_x_days / 2
}

int_to_days <- function(x) {
  days_string <- c(year = 365, quarter = 91, month = 30, week = 7, day = 1)
  days_string[x$interval] * x$step
}

