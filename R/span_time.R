#' Wrapper around `seq.POSIXct`
#'
#' Quickly create a sequence of datetimes from chararcter values.
#'
#' @param from Character of length 11 (ymd h), 13 (ymd hm), or 15 (
#' ymd hms). Indicating the start value of the sequence.
#' @param to Character of the same length as `from`.
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
  valid_char_dt(from, name = "from")



}

valid_char_dt <- function(x, name) {
  if (!is.character(x)) {
    stop(sprintf("%s should be a character", name), call. = FALSE)
  }
  if (!match_date_pattern(x)) {
    stop(sprintf("%s should be 11, 13, or 15 characters long", name), call. = FALSE)
  }
}

match_date_pattern <- function(x) {
  grepl("^\\d{8}\\s\\d{2}$", x) |
    grepl("^\\d{8}\\s\\d{4}$", x) |
    grepl("^\\d{8}\\s\\d{6}$", x)
 }
