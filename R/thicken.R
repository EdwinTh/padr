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
#' @param interval The interval of the added datetime variable.
#' Any character string that would be accepted by \code{seq.Date()} or
#' \code{seq.POSIXt}. It can only be higher than the interval and step size of
#' the input data.
#' @param colname The column name of the added variable. If \code{NULL} it will
#' be the name of the original datetime variable with the interval name added to
#' it (including the unit), separated by underscores.
#' @param rounding Should a value in the input datetime variable be mapped to
#' the closest value that is lower (\code{down}) or that is higher (\code{up})
#' than itself.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' \code{by} indicates which to use for thickening.
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
#' thicken(some_df, 'week')
#' thicken(some_df, 'month')
#' thicken(some_df, 'day', start_val = lubridate::ymd_hms('20160301 120000'))
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
#' x_df %>% thicken('week', start_val = min_x - 1) %>%
#'   group_by(x_week) %>% summarise(y_avg = mean(y))
#' @export
thicken <- function(x,
                    interval,
                    colname  = NULL,
                    rounding = c('down',
                                 'up'),
                    by        = NULL,
                    start_val = NULL) {

  is_df(x)
  check_start_and_end(start_val, NULL)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
  } else {
    dt_var <- check_data_frame(x)
  }

  interval_converted <- convert_interval(interval)
  interval_converted$interval <- uniform_interval_name(interval_converted$interval)
  rounding <- match.arg(rounding)

  dt_var_interval <- get_interval_list(dt_var)

  interval_higher <- convert_int_to_hours(interval_converted) >
    convert_int_to_hours(dt_var_interval)
  interval_equal <- convert_int_to_hours(interval_converted) ==
    convert_int_to_hours(dt_var_interval)

  # here I originally put in "& lenght(dt_var) > 2" but I don't recall why
  # removed it, but it might break in some situation now.
  if (!interval_higher) {
    stop('The interval in the datetime variable is lower than the interval given,
         you might be looking fo pad rather than for thicken.', call. = FALSE)
  } else if (interval_equal) {
    stop('The interval in the datetime variable is equal to the interval given,
         you might be looking for pad rather than for thicken.', call. = FALSE)
  }

  if (!all(dt_var[1:(length(dt_var) - 1)] <= dt_var[2:length(dt_var)])) {
    warning('Datetime variable was unsorted, result will be unsorted as well.', call. = FALSE)
  }

  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
  }

  ind_to_keep <- start_val_after_min_dt(start_val, dt_var)
  x <- x[ind_to_keep, , drop = FALSE] #nolint
  dt_var <- dt_var[ind_to_keep]

  spanned <- span(dt_var, interval_converted, start_val)

  thickened <- round_thicken(dt_var, spanned, rounding)

  if (is.null(by)) {
    x_name <- get_date_variables(x)
  } else {
    x_name <- by
  }

  colname <- get_colname(x, x_name, colname, interval_converted)
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
    groups <- as.character(groups(original))
    x <- dplyr::group_by_(x, .dots = groups)
  } else if (inherits(original, "data.table")) {
    x <- data.table::as.data.table(x)
  }
  return(x)
}

# take the character form of the interval and put it into list form
# using get_interval_list, check if valid right away
convert_interval <- function(interval) {
  start_val <- as.POSIXct("2017-01-01 00:00:00")
  x <- tryCatch(
    seq(start_val, length.out = 10, by = interval),
    error = function(e){
      stop("interval is not valid", call. = FALSE)
    })
  return(make_interval_list_from_string(interval))
}

make_interval_list_from_string <- function(interval_string) {
  interval_split <- strsplit(interval_string, " ")[[1]]
  if (length(interval_split) == 1) {
    return(list(interval = interval_split,
                step     = 1))
  } else {
    return(list(interval = interval_split[2],
                step     = as.numeric(interval_split[1])))
  }
}

# in order to compare different intervals we need to set them to the same unit
convert_int_to_hours <- function(interval_obj) {
  # we take # month = # year / 12
  hours_in_unit <- c(8760, 2190, 730, 168, 24, 1, 1 / 60, 1 / 3600)
  names(hours_in_unit) <- c("year", "quarter", "month", "week", "day",
                            "hour", "min", "sec")
  hours_in_unit[interval_obj$interval] * interval_obj$step
}

get_colname <- function(x, x_name, colname, interval_converted) {
  if (is.null(colname)) {
    if (interval_converted$step == 1) {
      colname <- paste(x_name, interval_converted$interval, sep = "_")
    } else {
      colname <- paste(x_name, interval_converted$step,
                       interval_converted$interval, sep = "_")
    }
  }
  return(colname)
}

uniform_interval_name <- function(interval) {
  if (interval %in% c("y", "ye", "yea", "years")) {
    interval <- "year"
  } else if (interval %in% c("q", "qu", "qua", "quar", "quart", "quarte", "quarters")){
    interval <- "quarter"
  } else if (interval %in% c("m", "mo", "mon", "mont", "months")) {
    interval <- "month"
  } else if (interval %in% c("w", "we", "wee", "weeks")){
    interval <- "week"
  } else if (interval %in% c("d", "da", "days")) {
    interval <- "day"
  } else if (interval %in% c("h", "ho", "hou", "hours")) {
    interval <- "hour"
  } else if (interval %in% c("mi", "mins")) {
    interval <- "min"
  } else if (interval %in% c("s", "se", "secs")) {
    interval <- "sec"
  }
  return(interval)
}

start_val_after_min_dt <- function(start_val, dt_var) {
  if (is.null(start_val)) {
    return(1:length(dt_var))
  } else {
    start_val <- to_posix(start_val, dt_var)$a
    dt_var    <- to_posix(start_val, dt_var)$b
    ind <- dt_var > start_val
    return(ind)
  }
}
