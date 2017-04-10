
span <- function(x,
                 interval,
                 start_val  = NULL) {

  if ( !( inherits(x, 'Date') |  inherits(x, 'POSIXt') ) ){
    break ('x should be of class Date, POSIXlt, or POSIXct', call. = FALSE)
  }

  start_and_end <- get_start_and_end(x, return_interval = interval)

  if ( is.null(start_val) ) {

    start_val <- start_and_end$start_val
    end_val   <- start_and_end$end_val

  } else if ( !is.null(start_val) ){

    end_val <- shift_end_from_start(start_and_end, start_val)
    end_val <- assure_greater_than_max_x(max(x), end_val, interval$interval)

  }

  by_val <- paste(interval$step, interval$interval)
  return_values <- seq(start_val, end_val, by = by_val)

  return(return_values)
}

shift_end_from_start <- function(start_and_end, start_val){

  start_when_null <- start_and_end$start_val
  end_when_null   <- start_and_end$end_val

  if ( inherits(start_val, 'POSIXt') & inherits(start_when_null, 'Date') ) {
    start_when_null <- as.POSIXct( as.character(start_when_null),
                                   tz = attr(start_val, 'tzone'))
    end_when_null <- as.POSIXct( as.character(end_when_null),
                                 tz = attr(start_val, 'tzone'))
  }
  start_offset <- start_when_null - start_val

  return(end_when_null - start_offset)
}

# by taking the offset in shift_end_from_start the end_val might be smaller
# than the largest value in x this function corrects this
assure_greater_than_max_x <- function(max_x,
                                      end_val,
                                      interval) {
  if ( inherits(end_val, 'POSIXt') & inherits(max_x, 'Date') ) {
    max_x <- as.POSIXct( as.character(max_x), tz = attr(end_val, 'tzone'))
  } else if ( inherits(max_x, 'POSIXt') & inherits(end_val, 'Date') ) {
    max_x <- as.Date( substr(max_x, 1, 10) )
  }

  while (end_val <= max_x) {
    end_val <- seq(end_val, length.out = 2, by = interval)[2]
  }

  return(end_val)

}

#----------------------------------------------------------------------------#
get_start_and_end <- function(dt_var,
                              return_interval) {
  min_v <- as.POSIXlt( min(dt_var) ) #nolint
  max_v <- as.POSIXlt( max(dt_var) )

  interval <- flatten_interval(return_interval)

  start_val_func <- sprintf("start_val_%s(min_v)", return_interval$interval)
  start_val <- eval(parse(text = start_val_func))
  span <- seq(start_val, max_v, by = interval)
  end_min_1 <- span[length(span)]
  end_val <- as.POSIXlt(seq(end_min_1, length.out = 2, by = interval)[2])

  to_date <- all( c(start_val$hour, start_val$min, start_val$sec,
                    end_val$hour, end_val$min, end_val$sec) == 0 )

  interval_allows_for_date <- !return_interval$inter %in%
    c("hour", "min", "sec")

  if (to_date & interval_allows_for_date) {
    start_val <- as.Date(strptime(start_val, format = '%Y-%m-%d'))
    end_val   <- as.Date(strptime(end_val, format = '%Y-%m-%d'))
  } else {
    start_val <- as.POSIXct(start_val)
    end_val   <- as.POSIXct(end_val)
  }

  return(list(start_val = start_val, end_val = end_val))
}

start_val_year <- function(min_v) {
  sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1 ( month_to_1 ( min_v ) ) ) ) )
}

start_val_quarter <- function(min_v) {
  sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1 ( this_quarter_month ( min_v ) ) ) ) )
}

start_val_month  <- function(min_v) {
  sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1 ( min_v ) ) ) )
}

start_val_week <- function(min_v) {
  sec_to_0 ( min_to_0 ( hour_to_0 ( this_week ( min_v ) ) ) )
}

start_val_day <- function(min_v) {
  sec_to_0 ( min_to_0 ( hour_to_0 ( min_v ) ) )
}

start_val_hour <- function(min_v) {
  sec_to_0 ( min_to_0 ( hour_to_0 ( min_v ) ) )
}

start_val_min <- function(min_v) {
  sec_to_0 ( min_to_0 ( min_v ) )
}

start_val_sec <- function(min_v) {
  sec_to_0 ( min_v )
}


# this set of functions take a POSIXlt and alter time units as named
next_year <- function(x) {
  x$year <- x$year + 1
  return(x)
}

next_month <- function(x) {
  x$mon <- x$mon + 1
  return(x)
}

next_day <- function(x) {
  x$mday <- x$mday + 1
  return(x)
}

next_hour <- function(x) {
  x$hour <- x$hour + 1
  return(x)
}

next_min <- function(x) {
  x$min <- x$min + 1
  return(x)
}

next_sec <- function(x) {
  x$sec <- x$sec + 1
  return(x)
}

month_to_1 <- function(x) {
  # note month ranges from 0 to 11
  x$mon <- 0
  return(x)
}

day_to_1 <- function(x) {
  x$mday <- 1
  return(x)
}

hour_to_0 <- function(x) {
  x$hour <- 0
  return(x)
}

min_to_0 <- function(x) {
  x$min <- 0
  return(x)
}

sec_to_0 <- function(x) {
  x$sec <- 0
  return(x)
}

this_quarter_month <- function(x) {
  x$mon <- floor(x$mon / 3) * 3
  return(x)
}

next_quarter_month <- function(x) {
  x$mon <- floor( x$mon   / 3) * 3 + 3
  return(x)
}

this_week <- function(x) {
  x$mday <- x$mday - x$wday
  return(x)
}

next_week <- function(x) {
  x$mday   <- x$mday   + (7 - x$wday)
  return(x)
}
