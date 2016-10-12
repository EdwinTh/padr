thicken <- function(x,
                    pulse = c('level_down',
                                 'year',
                                 'quarter',
                                 'month',
                                 'week',
                                 'day',
                                 'hour',
                                 'min'),
                    rounding = c('down',
                                 'up'),
                    by        = NULL,
                    start_val = NULL) {

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

  pulse <- match.arg(pulse)
  rounding <- match.arg(rounding)

  # Section 2: span a variable with all the relevant instances of pulse
  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year', 'quarter', 'month', 'week', 'day', 'hour','min', 'sec')

  if(pulse == 'level_down'){
    dt_var_pulse_nr <- int_hierarchy[get_pulse(dt_var)]
    pulse <- names(int_hierarchy[dt_var_pulse_nr - 1])
  }

  if(int_hierarchy[get_pulse(dt_var)] < int_hierarchy[pulse]) {
    stop('The pulse in the datetime variable is lower than the pulse given,
         you might be looking fo smear rather than for thicken.')
  } else if (int_hierarchy[get_pulse(dt_var)] == int_hierarchy[pulse]) {
    stop('The pulse in the datetime variable is equal to the pulse given,
         you might be looking for pad rather than for thicken.')
  }

  if(!all(dt_var[1:(length(dt_var)-1)] < dt_var[2:length(dt_var)])) {
    warning('Datetime variable was unsorted, result will be unsorted as well.')
  }

  spanned <- span(dt_var, pulse, start_val)

  # Section 3: make the thicken and create the return frame
  if(rounding == 'down'){
    return(round_down(dt_var, spanned))
  } else {
    return(round_up(dt_var, spanned))
  }
}
