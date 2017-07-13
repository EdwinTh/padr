#' Wrapper around `seq.Date`
#'
#' Quickly create a sequence of dates from integer values.
#'
#' @param from Integer or charaacter of length 4 (yyyy), 6 (yyyymm), or 8 (
#' yyymmdd). Indicating the start value of the sequence.
#' @param to Integer or character of the same length as `from`.
#' Indicating the end value of the sequence. Optional.
#' @param len_out The desired length of the sequence. Optional.
#' @param interval The desired interval. Optional.
#'
#' @details In addition to `from`, `to` or `length` must be specified. If `from`
#' length is 4, the first day of that year will be used and the interval is
#' year. If its length is 6, the first day of that month will be used,
#' and the interval is month. If the length is 8, that day will be used and the
#' interval is day. Use `interval` to override these defaults.
#'
#' @return An object of class Date.
#'
#' @examples
#' # using to
#' span_date(2011, 2015)
#' span_date(201101, 201501)
#' span_date(2011, 2015, interval = "month")
#' span_date(20110101, 20110201)
#' span_date(20110101, 20110201, interval = "month")
#'
#' # using len_out
#' span_date(2011, len_out = 4)
#' span_date(201101, len_out = 4)
#' span_date(20110101, len_out = 4)
#' span_date(20110101, len_out = 4, interval = "month")
#' @export
#'
span_date <- function(from,
                      to = NULL,
                      len_out = NULL,
                      interval = NULL) {
  check_two_null(len_out, to)
  check_equal_length(from, to)
  check_valid_input(from, name = "from", "date")
  from_dt <- convert_short(from)
  if (!is.null(to)) to_dt <- convert_short(to, "date")
  if (is.null(interval)) interval <- interval_from_short(nchar(from))
  if (!is.null(to)) {
    return(seq.Date(from_dt, to_dt, by = interval))
  } else {
    seq.Date(from_dt, length.out = len_out, by = interval)
  }
}

#' Wrapper around `seq.POSIXct`
#'
#' Quickly create a sequence of datetimes from chararcter values.
#'
#' @param from Integer or charaacter of length 4 (yyyy), 6 (yyyymm), or 8 (
#' yyymmdd). Character of length 11 (yyyymmdd hh), 13 (yyyymmdd hhmm), or 15 (
#' yyyymmdd hhmmss). Indicating the start value of the sequence.
#' @param to Integer or character of the same length as `from`
#' Indicating the end value of the sequence. Optional.
#' @param len_out The desired length of the sequence. Optional.
#' @param interval The desired interval. Optional.
#' @param tz The desired timezone.
#'
#' @details In addition to `from`, `to` or `length` must be specified. If `from`
#' length is 11, the first minute of that hour will be used and the interval is
#' hour. If its length is 13, the first second of that minute will be used,
#' and the interval is minute. If the length is 15, the iterval is second.
#'
#' @return An object of class POSIXct.
#'
span_time <- function(from,
                      to       = NULL,
                      len_out  = NULL,
                      interval = NULL,
                      tz       = "UTC") {
  check_two_null(len_out, to)
  check_equal_length(from, to)
  check_valid_input(from, name = "from", "time")
  ## hier doorgaan
  from_dt <- char_to_datetime(from, tz = tz)
  if (!is.null(to)) to_dt <- char_to_datetime(to, tz = tz)
  if (is.null(interval)) interval <- interval_from_char(nchar(from))
  if (!is.null(to)) {
    return(seq.POSIXt(from_dt, to_dt, by = interval))
  } else {
    seq.POSIXt(from_dt, length.out = len_out, by = interval)
  }
}


check_two_null <- function(to, len_out) {
  if (is.null(to) && is.null(len_out)) {
    stop("either to or len_out must be specified", call. = FALSE)
  } else if (!is.null(to) && !is.null(len_out)) {
    warning("both to and len_out are specified, len_out is ignored",
            call. = FALSE)
  }
}

check_equal_length <- function(from, to) {
  if (!is.null(to) && nchar(from) != nchar(to)) {
    stop("from and to should be of equal length")
  }
}

check_valid_input <- function(x, name = "from", date_or_time = c("date", "time")) {
  if (is.numeric(x)) {
    valid_numeric_dt(x, name = name)
  } else if (is.character(x)) {
    valid_char_dt(x, name = name, date_or_time = date_or_time)
  } else {
    stop(sprintf("%s is not a character or numeric", name))
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

convert_short <- function(x, date_or_time = c("date", "time"), tz = "UTC") {
  x_string <- substr(paste0(x, "0101"), 1, 8)
  date_string <- paste(substr(x_string, 1, 4),
                       substr(x_string, 5, 6),
                       substr(x_string, 7, 8), sep = "-")
  date_or_time <- match.arg(date_or_time)
  if (date_or_time == "date") {
    as.Date(date_string)
  } else {
    as.POSIXct(date_string, tz = tz)
  }
}

interval_from_short <- function(x) {
  int_string <- c("year", "month", "day")
  int_string[(x-2) / 2]
}
