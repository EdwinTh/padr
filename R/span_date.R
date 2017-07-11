#' Wrapper around `seq.Date`
#'
#' Quickly create a sequence of dates from integer values.
#'
#' @param from Integer of length 4 (year only), 6 (year and month), or 8 (
#' year, month, and day). Indicating the start value of the sequence.
#' @param to Integer of the same length as `from`.
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
  valid_integer_dt(from, name = "from")
  from_dt <- integer_to_date(from, "from")
  if (!is.null(to)) to_dt <- integer_to_date(to, "to")
  if (is.null(interval)) interval <- interval_from_int(nchar(from))
  if (!is.null(to)) {
    return(seq.Date(from_dt, to_dt, by = interval))
  } else {
    seq.Date(from_dt, length.out = len_out, by = interval)
  }
}

integer_to_date <- function(x, name = "from") {
  x <- as.integer(x)
  x_string <- substr(paste0(x, "0101"), 1, 8)
  date_string <- paste(substr(x_string, 1, 4),
                       substr(x_string, 5, 6),
                       substr(x_string, 7, 8), sep = "-")
  as.Date(date_string)
}

valid_integer_dt <- function(x, name = name) {
  if (!is.numeric(x)) {
    stop(sprintf("%s should be numeric", name), call. = FALSE)
  }
  if (x %% 1 != 0) {
    stop(sprintf("%s is not a valid integer", name), call. = FALSE)
  }
  if (!nchar(x) %in% c(4, 6, 8)) {
    stop(sprintf("%s is not of a valid length", name), call. = FALSE)
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

interval_from_int <- function(x) {
  int_string <- c("year", "month", "day")
  int_string[(x-2) / 2]
}
