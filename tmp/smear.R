x <- seq(ymd(20160101), length.out = 12, by = 'month')

smear <- function(x,
                  interval = c('level_up',
                               'quarter',
                               'month',
                               'week',
                               'day',
                               'hour',
                               'min',
                               'sec'),
                  by        = NULL,
                  start_val = NULL) {

  arguments <- as.list(match.call())
  if('by' %in% names(arguments)) by_val <- as.character(arguments$by)

  if(is.data.frame(x)) {
    original_data_frame <- x
    x <- as.data.frame(x)
    if('by' %in% names(arguments)){
      dt_var <- check_data_frame(x, by = by_val)
      dt_var_name <- by_val
    } else {
      dt_var <- check_data_frame(x)
      dt_var_name <- get_date_variables(x)
    }
  } else {
    dt_var <- check_vector(x)
  }

  interval <- match.arg(interval)
  interval_dt_var <- get_interval(dt_var)

  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year','quarter', 'month', 'week', 'day','hour','min', 'sec')

  if(interval == 'level_up'){
    dt_var_interval_nr <- int_hierarchy[get_interval(dt_var)]
    interval <- names(int_hierarchy[dt_var_interval_nr + 1])
  }

  if(int_hierarchy[interval_dt_var] > int_hierarchy[interval]) {
    stop('The interval in the datetime variable is higher than the interval given,
         you might be looking fo thicken rather than for thicken.')
  } else if (int_hierarchy[get_interval(dt_var)] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
         you might be looking for pad rather than for thicken.')
  }

  if(!all(dt_var[1:(length(dt_var)-1)] < dt_var[2:length(dt_var)])) {
    warning('Datetime variable was unsorted, result will be unsorted as well.')
  }
  x <- ymd_hms('20160101 000000')
  dt_var = seq(x, length.out = 24, by = 'hour')
  interval = 'min'
  spanned <- span(dt_var, interval, end_offset = interval_dt_var)
  data.frame(original = round_down(spanned, dt_var),
             smeared  = spanned)

}

