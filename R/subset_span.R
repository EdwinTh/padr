#' Subset a spanned datetime vector
#'
#' Take a \code{Date}, \code{POSIXct}, or \code{POSIXlt} vector and subset it by
#' a pattern of date and/or time parts.
#' @param spanned A vector of class  \code{Date}, \code{POSIXct}, or
#' \code{POSIXlt}.
#' @param pattern_list A list with the desired pattern for each of the following
#' datetime parts: year, mon, mday, wday, hour, min, sec.
#' @return
#' Vector of the same class as \code{spanned}, containing all the data points in
#' \code{spanned} that meets the requirements in \code{pattern_list}.
#' @details For subsetting weekdays, they run from 0 (Sunday) to 6 (Saturday).
#' @examples
#' date_span <- span_date(20170701, len_out = 100)
#' subset_span(date_span, list(wday = 1:5))
#'
#' time_span <- span_time("20170101 00", 201702)
#' subset_span(time_span, list(hour = 7:17))
#' subset_span(time_span, list(hour = c(10, 16), mday = seq(5, 30, 5)))
#' @export
subset_span <- function(spanned,
                        pattern_list){
  test_datetime(spanned)
  original_type <- class(spanned)
  spanned_lt    <- as.POSIXlt(spanned)
  parts         <- names(pattern_list)
  check_filter_on(parts)
  spanned_subsetted <- filter_subset(spanned_lt, pattern_list)
  if (original_type[1] == "POSIXct") {
    spanned_subsetted <- as.POSIXct(spanned_subsetted)
  } else if (original_type == "Date") {
    spanned_subsetted <- as.Date(spanned_subsetted)
  }
  spanned_subsetted
}

check_filter_on <- function(x) {
  if (!all(x %in% c("year", "mon", "mday", "wday", "hour", "min", "sec"))) {
    stop("invalid name(s) in the pattern_list, the following are valid:
       year, mon, mday, wday, hour, min, sec", call. = FALSE)
  }
}

# did not manage to get this to work with a generic function
# because spanned_lt$ does not seem to accept arguments whatsoever.
# need help for refactoring this
filter_one_part <- function(spanned_lt,
                            pattern_list,
                            part) {
  if (part == "year") {
    spanned_lt[(spanned_lt$year  + 1900) %in% pattern_list$year]
  } else if (part == "mon") {
    spanned_lt[(spanned_lt$mon + 1) %in% pattern_list$mon]
  } else if (part == "mday") {
    spanned_lt[spanned_lt$mday %in% pattern_list$mday]
  } else if (part == "wday") {
    spanned_lt[spanned_lt$wday %in% pattern_list$wday]
  } else if (part == "hour") {
    spanned_lt[spanned_lt$hour %in% pattern_list$hour]
  } else if (part == "min") {
    spanned_lt[spanned_lt$min %in% pattern_list$min]
  } else if (part == "sec") {
    spanned_lt[spanned_lt$sec %in% pattern_list$sec]
  }
}

filter_subset <- function(spanned_lt,
                          pattern_list) {
  for (prt in names(pattern_list)) {
    spanned_lt <- filter_one_part(spanned_lt, pattern_list, prt)
  }
  spanned_lt
}

test_datetime <- function(x) {
  if (!(inherits(x, "POSIXt") | inherits(x, "Date"))){
    stop("x is not of class POSIXct, POSIXlt, or Date", call. = FALSE)
  }
}
