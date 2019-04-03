#' Get the interval of a datetime variable
#'
#' The interval is the highest datetime unit that can explain all instances of a
#' variable of class \code{Date}, class \code{POSIXct}, or class \code{POSIXct}.
#' This function will determine what the interval of the variable is.
#' @param x A variable of class of class \code{Date} or of class \code{POSIXt}.
#' @return A character string indicating the interval of \code{x}.
#' @details See \code{vignette("padr")} for more information on intervals.
#' @examples
#' x_month <- seq(as.Date('2016-01-01'), as.Date('2016-05-01'), by = 'month')
#' get_interval(x_month)
#'
#' x_sec <- seq(as.POSIXct('2016-01-01 00:00:00'), length.out = 100, by = 'sec')
#' get_interval(x_sec)
#' get_interval(x_sec[seq(0, length(x_sec), by = 5)])
#' @export
get_interval <- function(x) {
  stop_on_NA(x)
  interval <- get_interval_list(x)
  if (interval$step == 1) {
    return(interval$interval)
  } else {
    return(paste(interval$step, interval$interval))
  }
}

stop_on_NA <- function(x) {
  if (anyNA(x)) {
    stop("interval cannot be determined when x contains NAs")
  }
}

get_interval_list <- function(x){
  stop_not_datetime(x)

  x_char <- datetime_char(x)
  differ <- lowest_differ(x_char)

  if ( length(differ) == 0 ) {
    stop("x does not vary, cannot determine the interval", call. = FALSE)
  }

  if (differ == 'month') {
    if (is_month_quarter(x_char)) differ <- 'quarter'
  }

  if (differ == 'day') {
    if (is_day_week(x_char)) differ <- 'week'
  }

  # after assessing the differ, we check if we need only need 1 unit
  step <- get_step(x, differ)

  return(list(interval = differ, step = step))
}

# change a variable of class Date or POSIXt to a character, use format instead
# of as.character for performance
datetime_char <- function(x) {
  if (inherits(x, "Date")) {
    paste(format(x, "%F"), "00:00:00")
  } else {
    format(x, "%F %T")
  }
}

# check what levels of the datetime variable differ, x is the output of datetime_char
lowest_differ <- function(x_char) {
  differ <- which(c(
    year   = ! length( unique ( substr(x_char, 1, 4) ) ) == 1,
    month  = ! length( unique ( substr(x_char, 6, 7) ) ) == 1,
    day    = ! length( unique ( substr(x_char, 9, 10) ) ) == 1,
    hour   = ! length( unique ( substr(x_char, 12, 13) ) ) == 1,
    min    = ! length( unique ( substr(x_char, 15, 16) ) ) == 1,
    sec    = ! length( unique ( substr(x_char, 18, 19) ) ) == 1
  ))
  return( names( differ[length(differ)] ) )
}

# using the lowest_differ we cannot detect quarter and week
# if the interval is month we look for quarter (quarter is special case of month)
is_month_quarter <- function(x_char) {
  m <- as.POSIXlt(x_char)$mon
  all(m %in% c(1, 4, 7, 10)) | all(m %in% c(2, 5, 8, 11)) | all(m %in% c(0, 3, 6, 9))
}

# if the interval is day we we will look for week
is_day_week <- function(x_char){
  all_weeks <- seq( as.POSIXlt(min(x_char), tz = 'UTC'),
                    as.POSIXlt(max(x_char), tz = 'UTC'),
                    by = '7 DSTdays')
  x_posix <- as.POSIXlt(x_char, tz = 'UTC')
  all(as.numeric(x_posix) %in% as.numeric(all_weeks))
}


####################################################################################
# after finding the "whole" time unit, see if we need a higher level within the unit
get_step <- function(x, d) {
  if (d == "year") return(step_of_year(x))
  if (d == "quarter") return(step_of_quarter(x))
  if (d == "month") return(step_of_month(x))
  if (d == "week") return(step_with_difftime(x, "week"))
  if (d == "day") return(step_with_difftime(x, "day"))
  if (d == "hour") return(step_with_difftime(x, "hour"))
  if (d == "min") return(step_with_difftime(x, "min"))
  if (d == "sec") return(step_with_difftime(x, "sec"))
}

step_of_year <- function(x) {
  years <- sort( as.numeric(substr(x, 1, 4)) )
  max_val <- smallest_nonzero(years)
  return( get_max_modulo_zero( get_difs(years), max_t = max_val ) )
}

step_of_quarter <- function(x) {
  months <- sort( convert_month_to_number(x) )
  quarters <- months / 3
  max_val <- smallest_nonzero(quarters)
  return( get_max_modulo_zero( get_difs(quarters), max_t = max_val ) )
}

step_of_month <- function(x) {
  months <- sort( convert_month_to_number(x) )
  max_val <- smallest_nonzero(months)
  return( get_max_modulo_zero( get_difs(months), max_t = max_val) )
}

step_with_difftime <- function(x, unit) {
  if (inherits(x, "Date")) {
    time_dif <- numeric_dif_date(x, unit = unit)
  } else if (inherits(x, "POSIXt")) {
    time_dif <- numeric_dif_posix(x, unit = unit)
  }
  return(get_max_modulo_zero( time_dif, max_t = smallest_nonzero(time_dif)) )
}

# count each month as a number from the first
convert_month_to_number <- function(x) {
  all_months_in_x <- seq(min(x), to = max(x), by = "month")
  month_num_lookup <- 0:(length(all_months_in_x) - 1)
  names(month_num_lookup) <- all_months_in_x
  month_num_lookup[as.character(x)]
}

get_difs <- function(x) {
  n <- length(x)
  return(x[2:n] - x[1:(n - 1)])
}

get_max_modulo_zero <- function(d, min_t = 1, max_t = 60) {
  if (length(d) == 1) {
    return(d)
  }
  ints_to_check <- min_t:max_t
  modulos <- sapply(ints_to_check, function(x, y) y %% x, d)
  zero_modulos <- ints_to_check[colSums(modulos) == 0]
  if (length(zero_modulos) == 0) {
    return(1)
  } else {
    return(max(zero_modulos))
  }
}

smallest_nonzero <- function(x) {
  nonzero <- x[x > 0]
  min(nonzero)
}

# wrapper around get_interval that returns NA when interval can't be determined
# instead of breaking.
get_interval_try <- function(x) {
  int <- try(get_interval(x), silent = TRUE)
  if (inherits(int, "try-error")) {
    int <- NA
  }
  int
}

## knowing the interval we can convert to numeric to get the units for
## everything of a week and lower.
numeric_dif_date <- function(x, unit) {
  x_num <- sort(unique(as.numeric(x)))
  difs  <- get_difs(x_num)
  if (unit == "week") {
    difs / 7
  } else if (unit == "day") {
    difs
  }
}

numeric_dif_posix <- function(x, unit) {
  x_num <- sort(unique(as.numeric(x)))
  difs  <- get_difs(x_num)
  if (unit == "sec") {
    difs
  } else if (unit == "min") {
    difs / 60
  } else if (unit == "hour") {
    difs / 3600
  } else if (unit == "day") {
    difs / 86400
  } else if (unit == "week") {
    difs / 604800
  }
}
