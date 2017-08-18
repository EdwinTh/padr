#'

shift_to_middle <- function(x) {
  stop_not_datetime(x)
  if (inherits(x, "Date")) {
    shift_to_middle_date(x)
  } else {
    shift_to_middle_posix(x)
  }
}

shift_to_middle_posix <- function(x) {
  stopifnot(inherits(x, "POSIXt"))
  interval_x <- get_interval_list(x)
  interval_x_secs <- int_to_secs(interval_x)
  x + interval_x_secs / 2
}

int_to_secs <- function(x) {
  day_secs <- 3600 * 24
  secs_string <- c(year = day_secs*365, quarter = day_secs*365/4,
                   monht = day_secs*365/12, week = day_secs*7,
                   day = day_secs, hour = 3600, min = 60, sec = 1)
  secs_string[x$interval] * x$step
}

shift_to_middle_date <- function(x) {
  stopifnot(inherits(x, "Date"))
  interval_x <- list(interval = "day", step = 15)
  interval_x_days <- int_to_days(interval_x)
  x + interval_x_days / 2
}

int_to_days <- function(x) {
  days_string <- c(year = 365, quarter = 365/4, month = 365/12, week = 7, day = 1)
  days_string[x$interval] * x$step
}

