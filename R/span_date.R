#' Wrapper around `seq.Date`
#'
#' Quickly create a sequence of dates from minimal specifications of dates.
#'
#' @param from Integer or character of length 4 (yyyy), 6 (yyyymm), or 8 (
#' yyymmdd). Indicating the start value of the sequence.
#' @param to Integer or character of length 4 (yyyy), 6 (yyyymm), or 8 (
#' yyymmdd). Optional.
#' @param len_out The desired length of the sequence. Optional.
#' @param interval The desired interval. Optional.
#'
#' @details Minimal specifications of dates set unspecified date parts to
#' default values. These are 01 for both month and day.
#'
#' In addition to `from`, `to` or `length` must be specified. If the
#' interval is not specified, `span_date` will set the interval to the highest
#' of the specified date parts in either `from` or `to`. E.g. if they are
#' 2011 and 2015 it will be "year", if they are 2011 and 201501 it will be
#' "month".
#'
#' @return An object of class Date.
#'
#' @examples
#' # using to
#' span_date(2011, 2015)
#' span_date(201101, 201501)
#' span_date(2011, 2015, interval = "month")
#' span_date(2011, 201501)
#' span_date(20111225, 2012)
#'
#' # using len_out
#' span_date(2011, len_out = 4)
#' span_date(201101, len_out = 4)
#' span_date(20110101, len_out = 4)
#' span_date(20110101, len_out = 4, interval = "month")
#' @export
span_date <- function(from,
                      to       = NULL,
                      len_out  = NULL,
                      interval = NULL) {
  check_to_len_out(len_out, to)
  check_valid_input_span(from, name = "from", "date")
  from_dt <- convert_to_date(from)
  if (is.null(interval)) {
    interval <- interval_from_short(max(nchar(from), nchar(to)))
  }
  if (!is.null(to)) {
    check_valid_input_span(to, name = "to", "date")
    to_dt <- convert_to_date(to)
    seq.Date(from_dt, to_dt, by = interval)
  } else {
    seq.Date(from_dt, length.out = len_out, by = interval)
  }
}

#' Wrapper around `seq.POSIXct`
#'
#' Quickly create a sequence of dates from minimal specifications of datetimes.
#'
#' @param from Integer or character of length 4 (yyyy), 6 (yyyymm), or 8 (
#' yyymmdd). Character of length 11 (yyyymmdd hh), 13 (yyyymmdd hhmm), or 15 (
#' yyyymmdd hhmmss). Indicating the start value of the sequence.
#' @param to Integer or character of length 4 (yyyy), 6 (yyyymm), or 8 (
#' yyymmdd). Character of length 11 (yyyymmdd hh), 13 (yyyymmdd hhmm), or 15 (
#' yyyymmdd hhmmss). Indicating the end value of the sequence. Optional.
#' @param len_out The desired length of the sequence. Optional.
#' @param interval The desired interval. Optional.
#' @param tz The desired timezone.
#'
#' @details Minimal specifications of datetimes set unspecified date parts to
#' default values. These are 01 for both month and day and 00 for hour, minute,
#' and second.
#'
#' In addition to `from`, `to` or `length` must be specified. If the
#' interval is not specified, `span_time` will set the interval to the highest
#' of the specified datetime parts in either `from` or `to`. E.g. if they are
#' "20160103 01" and "20160108 05" it will be "day", if they are "2011" and
#' "20110101 021823" it will be "second".
#'
#' @return An object of class POSIXct.
#' @examples
#' # using to
#' span_time(2011, 2013)
#' span_time("2011", "2013")
#' span_time(2011, 201301)
#' span_time(2011, 20130101)
#' span_time(2011, "20110101 0023")
#' span_time(2011, "20110101 002300")
#'
#' # using len_out
#' span_time(2011, len_out = 3)
#' span_time("2011", len_out = 3)
#' span_time(2011, len_out = 10, interval = "month")
#' span_time(2011, len_out = 10, interval = "day")
#' span_time(2011, len_out = 10, interval = "hour")
#' span_time("20110101 00", len_out = 10)
#' span_time("20110101 002300", len_out = 10)
#'
#' @export
span_time <- function(from,
                      to       = NULL,
                      len_out  = NULL,
                      interval = NULL,
                      tz       = "UTC") {
  check_to_len_out(len_out, to)
  check_valid_input_span(from, name = "from", "time")
  from_dt <- convert_to_datetime(from, tz = tz)
  if (is.null(interval)) {
    interval <- interval_from_long(max(nchar(from), nchar(to)))
  }
  if (!is.null(to)) {
    check_valid_input_span(to, name = "to", "time")
    to_dt <- convert_to_datetime(to, tz = tz)
    seq.POSIXt(from_dt, to_dt, by = interval)
  } else {
    seq.POSIXt(from_dt, length.out = len_out, by = interval)
  }
}

check_to_len_out <- function(to, len_out) {
  if (is.null(to) && is.null(len_out)) {
    stop("either to or len_out must be specified", call. = FALSE)
  } else if (!is.null(to) && !is.null(len_out)) {
    warning("both to and len_out are specified, len_out is ignored",
            call. = FALSE)
  }
}

check_valid_input_span <- function(x,
                                   name         = "from",
                                   date_or_time = c("date", "time")) {
  if (is.numeric(x)) {
    valid_numeric_dt(x, name = name)
  } else if (is.character(x)) {
    valid_char_dt(x, name = name, date_or_time = date_or_time)
  } else {
    stop(sprintf("%s is not a character or numeric", name), call. = FALSE)
  }
}

valid_numeric_dt <- function(x, name = name) {
  if (x %% 1 != 0) {
    stop(sprintf("%s is not a valid integer", name), call. = FALSE)
  }
  if (!nchar(x) %in% c(4, 6, 8)) {
    stop(sprintf("%s is not of a valid length", name), call. = FALSE)
  }
}

valid_char_dt <- function(x,
                          name,
                          date_or_time = c("date", "time")) {
  if (date_or_time == "date") {
    if (!match_date_pattern(x)) {
      stop(sprintf("%s is an unvalid character for span_date", name), call. = FALSE)
    }
  } else {
    if (!match_date_pattern(x) & !match_date_time_pattern(x)) {
      stop(sprintf("%s is an unvalid character for span_time", name), call. = FALSE)
    }
  }
}

match_date_pattern <- function(x) {
  grepl("^\\d{4}$", x) |
    grepl("^\\d{6}$", x) |
    grepl("^\\d{8}$", x)
}

match_date_time_pattern  <- function(x) {
  grepl("^\\d{8}\\s\\d{2}$", x) |
    grepl("^\\d{8}\\s\\d{4}$", x) |
    grepl("^\\d{8}\\s\\d{6}$", x)
}

convert_to_date <- function(x) {
  x_string <- substr(paste0(x, "0101"), 1, 8)
  date_string <- paste(substr(x_string, 1, 4),
                       substr(x_string, 5, 6),
                       substr(x_string, 7, 8), sep = "-")
  as.Date(date_string)
}

interval_from_short <- function(x) {
  int_string <- c("year", "month", "day")
  int_string[(x-2) / 2]
}

convert_to_datetime <- function(x,
                                tz = "UTC") {
  date_string <- substr(paste0(x, "0101"), 1, 8)
  date_pt <- paste(substr(date_string, 1, 4),
                   substr(date_string, 5, 6),
                   substr(date_string, 7, 8), sep = "-")
  time_string <- substr(paste0(x, "0000000000000000"), 10, 15)
  time_pt <- paste(substr(time_string, 1, 2),
                   substr(time_string, 3, 4),
                   substr(time_string, 5, 6), sep = ":")
  as.POSIXct(paste(date_pt, time_pt), tz = tz)
}


interval_from_long <- function(x) {
  if (x < 9) {
    interval <- interval_from_short(x)
  } else {
    int_string <- c("hour", "min", "sec")
    interval <- int_string[(x-9) / 2]
  }
  interval
}
