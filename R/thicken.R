#' Add a variable of a higher interval to a data frame.
#'
#' \code{thicken} will take the datetime variable in a data frame and map this
#' to a variable of a higher interval. The mapping is added to the data frame
#' in a new variable. After applying \code{thicken} the user can aggregate the
#' other variables in the data frame to the higher interval, for instance using
#' \code{dplyr}.
#'
#' @param x A data frame containing at least one datetime variable of
#' class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' @param interval The interval of the added datetime variable, which should be higher
#' than the interval of the input datetime variable. If \code{NULL} it will be one level
#' higher than the interval of the input datetime variable.
#' @param colname The column name of the added variable. If \code{NULL} it will
#' be the name of the original datetime variable with the interval name added to
#' it, separeted by an underscore.
#' @param rounding Should a value in the input datetime variable be mapped to
#' the closest value that is lower (\code{down}) or that is higher (\code{up})
#' than itself.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' \code{by} indicates which to use.
#' @param start_val By default the first instance of \code{interval} that is lower
#' than the lowest value of the input datetime variable, with all time units on
#' default value. Specify \code{start_val} as an offset if you want the range
#' to be nonstandard.
#' @return The data frame \code{x} with the variable added to it.
#' @details See \code{vignette("padr")} for more information on \code{thicken}.
#' See \code{vignette("padr_implementation")} for detailed information on
#' daylight savings time, different timezones, and the implementation of
#' \code{thicken}.
#' @examples
#' x_hour <- seq(lubridate::ymd_hms('20160302 000000'), by = 'hour',
#'               length.out = 200)
#' some_df <- data.frame(x_hour = x_hour)
#' thicken(some_df)
#' thicken(some_df, 'month')
#' thicken(some_df, start_val = lubridate::ymd_hms('20160301 120000'))
#'
#' library(dplyr)
#' x_df <- data.frame(
#'   x = seq(lubridate::ymd(20130101), by = 'day', length.out = 1000) %>%
#'     sample(500),
#'   y = runif(500, 10, 50) %>% round) %>%
#'   arrange(x)
#'
#' # get the max per month
#' x_df %>% thicken('month') %>% group_by(x_month) %>%
#'   summarise(y_max = max(y))
#'
#' # get the average per week, but you want your week to start on Mondays
#' # instead of Sundays
#' min_x <- x_df$x %>% min
#' weekdays(min_x)
#' x_df %>% thicken(start_val = min_x - 1) %>%
#'   group_by(x_week) %>% summarise(y_avg = mean(y))
#' @export
thicken <- function(x,
                    interval = c('level_up',
                                 'year',
                                 'quarter',
                                 'month',
                                 'week',
                                 'day',
                                 'hour',
                                 'min'),
                    colname  = NULL,
                    rounding = c('down',
                                 'up'),
                    by        = NULL,
                    start_val = NULL) {

  if (!is.data.frame(x)) {
    stop('x should be a data frame.')
  }

  arguments <- as.list(match.call())
  if (!missing(by)) by_val <- as.character(arguments$by) else by_val <- NULL

  # TO DO give original data frame type back
  original_data_frame <- x
  x <- as.data.frame(x)

  if ('by' %in% names(arguments)){
    dt_var <- check_data_frame(x, by = by_val)
  } else {
    dt_var <- check_data_frame(x)
  }

  interval <- match.arg(interval)
  rounding <- match.arg(rounding)

  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year', 'quarter', 'month', 'week', 'day', 'hour', 'min', 'sec')
  dt_var_interval <- get_interval(dt_var)

  if (interval == 'level_up'){
    dt_var_interval_nr <- int_hierarchy[dt_var_interval]
    interval <- names(int_hierarchy[dt_var_interval_nr - 1])
  }

  if (int_hierarchy[dt_var_interval] < int_hierarchy[interval]) {
    stop('The interval in the datetime variable is lower than the interval given,
         you might be looking fo pad rather than for thicken.')
  } else if (int_hierarchy[dt_var_interval] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
         you might be looking for pad rather than for thicken.')
  }

  if (!all(dt_var[1:(length(dt_var) - 1)] <= dt_var[2:length(dt_var)])) {
    warning('Datetime variable was unsorted, result will be unsorted as well.')
  }

  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
      start_val <- enforce_time_zone(start_val, dt_var)
  }

  spanned <- span(dt_var, interval, start_val)

  thickened <- round_thicken(dt_var, spanned, rounding)

  x_name <- get_date_variables(x)
  if (is.null(colname)) colname <- paste(x_name, interval, sep = '_')
  return_frame <- cbind(x, thickened)
  colnames(return_frame)[ncol(return_frame)] <- colname

  return_frame <- set_to_original_type(return_frame, original_data_frame)

  return(return_frame)
}

# restore to data_frame of data.table if the input data was of this type
set_to_original_type <- function(x,
                                  original) {
  if (inherits(original, "tbl_df")) {
    x <- dplyr::as_data_frame(x)
  } else if (inherits(original, "data.table")) {
    x <- data.table::as.data.table(x)
  }
  return(x)
}
