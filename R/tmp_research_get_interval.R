#' Get the interval of a datetime variable.
#'
#' The interval is the highest time unit that can explain all instances of a
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
get_interval_tmp <- function(x) {
  stop_on_NA(x)
  check_datetime(x)
  if (inherits(x, "POSIXt")) {
    interval <- get_interval_posix(x)
  } else {
    interval <- get_interval_date(x)
  }
  if (interval$step == 1) {
    return(interval$interval)
  } else {
    return(paste(interval$step, interval$interval))
  }
}

get_interval_date <- function(x){

  differ <- lowest_differ(as.character(x))

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

# check what levels of the datetime variable differ, x is the output of datetime_char
lowest_differ <- function(x_char) {
  differ <- which(c(
    year   = ! length( unique ( substr(x_char, 1, 4) ) ) == 1,
    month  = ! length( unique ( substr(x_char, 6, 7) ) ) == 1,
    day    = ! length( unique ( substr(x_char, 9, 10) ) ) == 1
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
  if (d == "week") return(step_with_difftime(x, "weeks"))
  if (d == "day") return(step_with_difftime(x, "days"))
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

step_with_difftime <- function(x, units) {
  time_dif <- as.numeric ( get_difftime(sort(x), units) )
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

get_difftime <- function(x, units) {
  n <- length(x)
  difftime(x[2:n], x[1:(n - 1)], units = units)
}

smallest_nonzero <- function(x) {
  nonzero <- x[x > 0]
  min(nonzero)
}

check_datetime <- function(x) {
  if ( !( inherits(x, 'Date') |  inherits(x, 'POSIXt')) ) {
    stop('x should be of class Date, POSIXct, or POSIXlt.', call. = FALSE)
  }
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

## ----------- new work ----------------##
# steps:
# when Date >> go to old way right away.
# else: see if can get coerced to Date, if yes go to old way.
# else: determine min_dif_posix: if %% (24 * 3600) == 0, coerce to date and go to
   # old way (this require new function, that cuts hh:mm:ss and coerces to date)
# else: seconds to interval
get_interval_posix <- function(x) {
  x_num <- as.numeric(x)
  if (length(unique(x)) == 1) {
    stop("x does not vary, cannot determine the interval", call. = FALSE)
  }

}

min_dif_posix <- function(x) {
  second_difs <- get_difs(as.numeric(x))
  min(second_difs)
}

seconds_to_interval <- function(x) {
  if (x %% 3600 == 0) {
    glue::glue("{x / 3600} hour")
  } else if (x %% 60 == 0) {
    glue::glue("{x / 60} min")
  } else {
    glue::glue("{x} sec")
  }
}


