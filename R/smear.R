#' @rdname thicken
smear <- function(x,
                  interval = c('year',
                               'month',
                               'day',
                               'hour',
                               'minute'),
                  rounding = c('down',
                               'up'),
                  by       = NULL,
                  start_val= NULL,
                  end_val  = NULL) {

  arguments <- as.list(match.call())

  if(is.data.frame(x)) {
    original_data_frame <- x
    x <- as.data.frame(x)
    dt_var <- if('by' %in%  names(arguments)) {
      check_data_frame(x, arguments$by)
    } else {
      check_data_frame(x)
    }
  } else {
    dt_var <- check_vector(x)
  }

  interval <- match.arg(interval)
  rounding <- match.arg(rounding)


  int_hierarchy <- 1:6
  names(int_hierarchy) <- c('year','month','day','hour','minute', 'second')
  if(int_hierarchy[get_interval(dt_var)] > int_hierarchy[interval]) {
    stop('The interval in the datetime variable is higher than the interval given,
you might be looking fo thicken rather than for thicken.')
  } else if (int_hierarchy[get_interval(dt_var)] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
you might be looking for pad rather than for thicken.')
  }

  if(interval == 'year') {
    span <- span_year(dt_var, start_val, end_val)
  } else if (interval == 'month') {
    span <- span_month(dt_var, start_val, end_val)
  } else if (interval == 'day') {
    span <- span_day(dt_var, start_val, end_val)
  } else if (interval == 'hour') {
    span <- span_hour(dt_var, start_val, end_val)
  } else if (interval == 'minute') {
    span <- span_minute(dt_var, start_val, end_val)
  } else {
    stop("Not reach span_function if else")
  }

}

