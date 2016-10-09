thicken <- function(x,
                    interval = c('level_down',
                                 'year',
                                 'quarter',
                                 'month',
                                 'week',
                                 'day',
                                 'hour',
                                 'min'),
                    rounding = c('down',
                                 'up'),
                    by       = NULL,
                    start_val= NULL,
                    end_val  = NULL) {

  # Section 1: obtain datetime variable and see if the variable is valid

  arguments <- as.list(match.call())
  if('by' %in% names(arguments)) by_val <- as.character(arguments$by)

  if(is.data.frame(x)) {
    original_data_frame <- x
    x <- as.data.frame(x)
    if('by' %in% names(arguments)){
      dt_var <- check_data_frame(x, by = by_val)
    } else {
      dt_var <- check_data_frame(x)
    }
  } else {
    dt_var <- check_vector(x)
  }

  interval <- match.arg(interval)
  rounding <- match.arg(rounding)

  # Section 2: span a variable with all the relevant instances of interval
  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year', 'quarter', 'month', 'week', 'day', 'hour','min', 'sec')

  if(interval == 'level_down'){
    dt_var_interval_nr <- int_hierarchy[get_interval(dt_var)]
    interval <- names(int_hierarchy[dt_var_interval_nr - 1])
  }

  if(int_hierarchy[get_interval(dt_var)] < int_hierarchy[interval]) {
    stop('The interval in the datetime variable is lower than the interval given,
         you might be looking fo smear rather than for thicken.')
  } else if (int_hierarchy[get_interval(dt_var)] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
         you might be looking for pad rather than for thicken.')
  }

  if(!all(dt_var[1:(length(dt_var)-1)] < dt_var[2:length(dt_var)])) {
    warning('Datetime variable was unsorted, result will be unsorted as well.')
  }

  if(interval == 'year') {
    span <- span_year(dt_var, start_val, end_val)
  } else if (interval == 'quarter') {
    span <- span_quarter(dt_var, start_val, end_val)
  } else if (interval == 'month') {
    span <- span_month(dt_var, start_val, end_val)
  } else if (interval == 'week') {
    span <- span_week(dt_var, start_val, end_val)
  } else if (interval == 'day') {
    span <- span_day(dt_var, start_val, end_val)
  } else if (interval == 'hour') {
    span <- span_hour(dt_var, start_val, end_val)
  } else if (interval == 'min') {
    span <- span_minute(dt_var, start_val, end_val)
  } else {
    stop("Not reach span_function if else")
  }

  # Section 3: make the thicken and create the return frame
  if(rounding == 'down'){
    return(round_down(dt_var, span))
  } else {
    return(round_up(dt_var, span))
  }
}
