
# Function that will obtain the start and end values from a vector
# to be applied when start_val and end_val are both NULL

get_start_and_end <- function(dt_var,
                              return_interval) {

  start_val <- as.POSIXlt( min(dt_var) )
  end_val   <- as.POSIXlt( max(dt_var) )

  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year', 'quarter', 'month', 'week', 'day', 'hour','min', 'sec')
  return_position <- int_hierarchy[return_interval]

  # year only : set year and month
  if(return_position == 1) {
    start_val$mon <- 0
    end_val$year <- end_val$year + 1
    end_val$mon <- 0
  }

  # quarter only : set month
  if(return_position == 2) {
    start_val$mon <- floor( start_val$mon / 3) * 3
    end_val$mon   <- floor( end_val$mon   / 3) * 3 + 3
  }

  # month only : set month
  if(return_position == 3){
    end_val$mon <- end_val$mon + 1
  }

  # up untill month : set day
  if(return_position < 4) {
    start_val$mday <- end_val$mday <- 1
  }

  # week only : set day
  if (return_position == 4) {
    # note that when applying weekdays() or $wday it will return original value
    start_val$mday <- start_val$mday - start_val$wday
    end_val$mday   <- end_val$mday   + (7 - end_val$wday)
  }

  # day only : set day
  if(return_position == 5) {
    end_val$mday <- end_val$mday + 1
  }

  # up untill day : set hour
  if(return_position < 6) {
    start_val$hour <- end_val$hour <- 0
  }

  # hour only : set hour
  if(return_position == 6) {
    end_val$hour <- end_val$hour + 1
  }

  # up untill hour : set minute
  if(return_position < 7) {
    start_val$min <- end_val$min <- 0
  }

  # minute only : set minute
  if(return_position == 7) {
    end_val$min <- end_val$min + 1
  }

  # up untill minute : set second
  if(return_position < 8) {
    start_val$sec <- end_val$sec <- 0
  }

  if(return_position == 8) {
    end_val$sec <- end_val$sec + 1
  }

  to_date <- all( c(start_val$hour, start_val$min, start_val$sec,
                    end_val$hour, end_val$min, end_val$sec) == 0 )

  if(to_date) {
    start_val <- as.Date(strptime(start_val, format = '%Y-%m-%d'))
    end_val   <- as.Date(strptime(end_val, format = '%Y-%m-%d'))
  } else {
    start_val <- as.POSIXct(start_val)
    end_val   <- as.POSIXct(end_val)
  }

   return(list(start_val = start_val, end_val = end_val))
}
