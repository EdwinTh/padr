#' Add a variable of a higher interval to a data frame
#'
#' Take the datetime variable in a data frame and map this
#' to a variable of a higher interval. The mapping is added to the data frame
#' in a new variable. After applying \code{thicken} the user can aggregate the
#' other variables in the data frame to the higher interval, for instance using
#' \code{dplyr}.
#'
#' @param x A data frame containing at least one datetime variable of
#' class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' @param interval The interval of the added datetime variable.
#' Any character string that would be accepted by \code{seq.Date} or
#' \code{seq.POSIXt}. It can only be higher than the interval and step size of
#' the input data.
#' @param colname The column name of the added variable. If \code{NULL} it will
#' be the name of the original datetime variable with the interval name added to
#' it (including the unit), separated by underscores.
#' @param rounding Should a value in the input datetime variable be mapped to
#' the closest value that is lower (\code{down}) or that is higher (\code{up})
#' than itself.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' Indicates which to use for thickening.
#' @param start_val By default the first instance of \code{interval} that is lower
#' than the lowest value of the input datetime variable, with all time units on
#' default value. Specify \code{start_val} as an offset if you want the range
#' to be nonstandard.
#' @param drop Should the original datetime variable be dropped from the
#' returned data frame? Defaults to \code{FALSE}.
#' @param ties_to_earlier By default when the original datetime observations is
#' tied with a value in the added datetime variable, it is assigned to the
#' current value when rounding is down or to the next value when rounding
#' is up. When \code{TRUE} the ties will be assigned to the previous observation
#' of the new variable instead.
#' @return The data frame \code{x} with the variable added to it.
#' @details When the datetime variable contains missing values, they are left
#' in place in the dataframe. The added column with the new datetime variable,
#' will have a missing values for these rows as well.
#'
#' See \code{vignette("padr")} for more information on \code{thicken}.
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
#' x_df %>% thicken('week',
#'                  start_val = closest_weekday(x_df$x, 2)) %>%
#'   group_by(x_week) %>% summarise(y_avg = mean(y))
#'
#' # rounding up instead of down
#' x <- data.frame(dt = lubridate::ymd_hms('20171021 160000',
#'                                         '20171021 163100'))
#' thicken(x, interval = "hour", rounding = "up")
#' thicken(x, interval = "hour", rounding = "up", ties_to_earlier = TRUE)
#' @export
thicken <- function(x,
                    interval,
                    colname  = NULL,
                    rounding = c("down",
                                 "up"),
                    by        = NULL,
                    start_val = NULL,
                    drop      = FALSE,
                    ties_to_earlier = FALSE) {

  is_df(x)
  has_rows(x)

  stopifnot(is.logical(drop), is.logical(ties_to_earlier))

  original_data_frame <- x
  x <- as.data.frame(x)

  dt_var_info <- get_dt_var_and_name(x, by)
  dt_var      <- dt_var_info$dt_var
  dt_var_name <- dt_var_info$dt_var_name

  check_start_and_end(start_val, NULL)

  interval_converted <- convert_interval(interval)
  interval_converted$interval <- uniform_interval_name(interval_converted$interval)
  rounding <- match.arg(rounding)

  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
  }

  ind_to_keep <- start_val_after_min_dt(start_val, dt_var)
  x <- x[ind_to_keep, , drop = FALSE] #nolint
  dt_var <- dt_var[ind_to_keep]

  if (is.null(by)) {
    x_name <- get_date_variables(x)
  } else {
    x_name <- by
  }

  colname <- get_colname(x, x_name, colname, interval_converted)

  na_ind <- which(is.na(dt_var))
  dt_var <- check_for_NA_thicken(dt_var, dt_var_name, colname)

  spanned <- span(dt_var, interval_converted, start_val)

  thickened <- round_thicken(dt_var, spanned, rounding, ties_to_earlier)

  if (all(all.equal(thickened, dt_var) == TRUE)) {
    stop("The thickened result is equal to the original datetime variable,
the interval specified is too low for the interval of the datetime variable", call. = FALSE)
  }

  thickened_with_na <- add_na_to_thicken(thickened, na_ind)
  thickened_frame   <- data.frame(thickened_with_na)

  return_frame <- dplyr::bind_cols(x, thickened_frame)
  colnames(return_frame)[ncol(return_frame)] <- colname

  if (drop) return_frame <- remove_original_var(return_frame, dt_var_name)

  set_to_original_type(return_frame, original_data_frame)
}

# restore to data_frame of data.table if the input data was of this type
set_to_original_type <- function(x,
                                 original) {
  if (inherits(original, "tbl_df")) {
    x <- dplyr::as_tibble(x)
    grps <- as.character(dplyr::groups(original))
    x <- dplyr::group_by(x, .dots = grps)
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

check_for_NA_thicken <- function(dt_var, dt_var_name, colname) {
  if (sum(is.na(dt_var))  > 0) {
    dt_var <- dt_var[!is.na(dt_var)]

      warn_mess <- sprintf(
"There are NA values in the column %s.
Returned dataframe contains original observations, with NA values for %s and %s.",
        dt_var_name, dt_var_name, colname
      )
    warning(warn_mess, call. = FALSE)
  }
  dt_var
}

add_na_to_thicken <- function(thickened, na_ind) {
  return_var <- rep(NA, length(thickened) + length(na_ind))
  attributes(return_var) <- attributes(thickened)
  ind <- seq_along(return_var)
  return_var[!ind %in% na_ind] <- thickened
  return_var
}

remove_original_var <- function(x, var_name) {
  x[, colnames(x) != var_name]
}
