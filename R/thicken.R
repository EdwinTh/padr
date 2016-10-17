#' Create a variable of a higher pulse from a datetime variable
#'
#' If the pulse of the data is too low and it needs to be aggregated to a higher
#' pulse thicken will create this variable of a higher pulse.
#'
#' @param x Either a data frame containing at least one datetime variable or
#' an object of class \code{Date} or class \class{POSIXt}.
#' @param pulse The pulse of the returned datetime variable, should be higher
#' than the pulse of the input datetime variable. Default mode is one level
#' higher than the pulse of the input datetime variable.
#' @param rounding Should a value in the input datetime variable be mapped to
#' the closest value that is lower (\code{down}) or that is higher (\code{up})
#' than itself.
#' @param by Only needs to be specified when x is a data frame containing
#' multiple variables that are eligable for padding. \code{by} indicates the
#' bare column name that should be used.
#' @param start_val By default the first instance of \code{pulse} that is lower
#' than the lowest value of the input datetime variable, with all time units on
#' default value. Specify \code{start_val} as an offset to change the values
#' of the time units.
#' @return A vector of class \code{Date} or \code{POSIXTct}, dependant on its
#' pulse. This vector serves as a mapping between the input datetime variable
#' and the variable of the desired pulse.
#' @examples
#' x_hour <- seq(lubridate::ymd_hms('20160301 000000'), by = 'hour',
#'               length.out = 1000)
#' thicken(x_hour)
#' thicken(x_hour, 'month')
#'
#' library(dplyr)
#' x_df <- data.frame(
#'   x_day = seq(lubridate::ymd(20130101), by = 'day', length.out = 1000) %>%
#'     sample(500),
#'   y = runif(500, 10, 50) %>% round) %>%
#'   arrange(x_day)
#'
#' # get the max per month
#' x_df %>% mutate(x_month = thicken(., 'month')) %>% group_by(x_month) %>%
#'   summarise(y_max = max(y))
#'
#' # get the average per week, but you want your week to start at Mondays instead
#' # of Sundays
#' start_day <- span(x_df$x_day, pulse = 'week')[1] + 1
#' x_df %>% mutate(x_week = thicken(., start_val = start_day)) %>%
#'   group_by(x_week) %>% summarise(y_avg = mean(y))

thicken <- function(x,
                    pulse = c('level_up',
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

  if(pulse == 'level_up'){
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
