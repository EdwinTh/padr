
span <- function(x,
                 interval = c('year',
                              'quarter',
                              'month',
                              'week',
                              'day',
                              'hour',
                              'min',
                              'sec'),
                 start_val  = NULL) {

  if ( !( inherits(x, 'Date') |  inherits(x, 'POSIXt') ) ){
    break ('x should be of class Date, POSIXlt, or POSIXct')
  }

  interval <- match.arg(interval)

  start_and_end <- get_start_and_end(x, return_interval = interval)

  if ( is.null(start_val) ) {

    start_val <- start_and_end$start_val
    end_val   <- start_and_end$end_val

  } else if ( !is.null(start_val) ){

    end_val <- shift_end_from_start(start_and_end, start_val)
    end_val <- assure_greater_than_max_x(max(x), end_val, interval)

  }

  return_values <- seq(start_val, end_val, by = interval)

  return_values
}

# to_val is the end_val as obtained from the get_start_and_end function
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
  }

  while (end_val <= max_x) {
    end_val <- seq(end_val, length.out = 2, by = interval)[2]
  }

  return(end_val)

}

#----------------------------------------------------------------------------#
# get_start_and_end with all units
get_start_and_end <- function(dt_var,
                              return_interval) {

  # calculate min and max on ct for performance, convert those to lt
  min_v <- as.POSIXlt( min(dt_var) )
  max_v <- as.POSIXlt( max(dt_var) )

  if (return_interval == 'year') {

    start_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1 ( month_to_1 ( min_v ) ) ) ) )

    end_val <- sec_to_0 ( min_to_0 ( hour_to_0 (
      day_to_1 ( month_to_1 ( next_year ( max_v ) ) )
    ) ) )

  } else if (return_interval == 'quarter') {

    start_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1 ( this_quarter_month ( min_v ) ) ) ) )

    end_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1  ( next_quarter_month ( max_v ) ) ) ) )

  } else if (return_interval == 'month') {

    start_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1 ( min_v ) ) ) )

    end_val  <- sec_to_0 ( min_to_0 ( hour_to_0 ( day_to_1  ( next_month ( max_v ) ) ) ) )

  } else if (return_interval == 'week') {

    start_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( this_week ( min_v ) ) ) )

    end_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( next_week ( max_v ) ) ) )

  } else if (return_interval == 'day') {

    start_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( min_v ) ) )

    end_val <- sec_to_0 ( min_to_0 ( hour_to_0 ( next_day ( max_v ) ) ) )

  } else if (return_interval == 'hour') {

    start_val <- sec_to_0 ( min_to_0 ( min_v ) )

    end_val <- sec_to_0 ( min_to_0 ( next_hour ( max_v ) ) )

  } else if (return_interval == 'min') {

    start_val <-  sec_to_0 ( min_v )

    end_val <- sec_to_0 ( next_min ( max_v ) )

  } else if (return_interval == 'sec') {

    end_val <- next_sec ( max_v )
  }

  to_date <- all( c(start_val$hour, start_val$min, start_val$sec,
                    end_val$hour, end_val$min, end_val$sec) == 0 )

  if (to_date) {
    start_val <- as.Date(strptime(start_val, format = '%Y-%m-%d'))
    end_val   <- as.Date(strptime(end_val, format = '%Y-%m-%d'))
  } else {
    start_val <- as.POSIXct(start_val)
    end_val   <- as.POSIXct(end_val)
  }

  return(list(start_val = start_val, end_val = end_val))

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
