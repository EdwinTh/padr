#' Wrapper around `seq.Date`
#'
#' Quickly create a sequence of dates from integer values.
#'
#' @param from Integer of length 4 (year only), 6 (year and month), or 8 (
#' year, month, and day). Indicating the start value of the sequence.
#' @param to Integer of the same length as `from`.
#'Indicating the end value of the sequence. Optional.
#' @param out_len The desired length of the sequence. Optional.
#' @param interval The desired interval. Optional.
#'
#' @details In addition to `from`, `to` or `length` must be specified. If `from`
#' length is 4, the first day of that year will be used and the interval is
#' year. If its length is 6, the first day of that month will be used,
#' and the interval is month. If the length is 8, that day will be used and the
#' interval is day. Use `interval` to override these defaults.

span_date <- function(from, to = NULL, out_len = NULL, interval = NULL) {
  check_two_null(out_len, to)
  check_equal_length(from, to)
  valid_integer_dt(from, name = "from")
  from_dt <- integer_to_date(from, "from")
  if (!is.null(to)) to_dt <- integer_to_date(to, "to")
  if (is.null(interval)) interval <- interval_from_int(from)
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
    stop(sprintf("%s cannot be coerced to integer", name), call. = FALSE)
  }
  if (x %% 1 != 0) {
    stop(sprintf("%s is not a valid integer", name), call. = FALSE)
  }
  if (!nchar(x) %in% c(4, 6, 8)) {
    stop(sprintf("%s is not of a valid length", name), call. = FALSE)
  }
}

check_two_null <- function(to, out_len) {
  if (is.null(to) && is.null(out_len)) {
    stop("either to or out_len must be specified", call. = FALSE)
  }
}

check_equal_length <- function(from, to) {
  if (!is.null(to) && nchar(from) != nchar(to)) {
    stop("from and to should be of equal length")
  }
}

interval_from_int <- function(x) {
  int_string <- c()
}
