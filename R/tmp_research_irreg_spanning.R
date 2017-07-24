#' Subset a Spanned Datetime Vector
#'
#' Take a `Date`, `POSIXct`, or `POSIXlt` vector and subset it by
#' a pattern of date and/or time parts.
#' @param spanned vector of class `Date`, `POSIXct`, or `POSIXlt`
#' @param pattern_list list with the desired pattern for each of the following
#' datetime parts: "year", "mon", "mday", "wday", "hour", "min", "sec"

subset_span <- function(spanned,
                        pattern_list){
  original_type <- class(spanned)
  spanned_lt    <- as.POSIXlt(spanned)
  parts         <- names(pattern_list)
  check_filter_on(parts)
  if ("year" %in% parts) pattern_list <- adjust_year(pattern_list)
  spanned_subsetted <- filter_subset(spanned_lt, pattern_list)
  if (original_type[1] == "POSIXct") {
    spanned_subsetted <- as.POSIXct(spanned_subsetted)
  } else if (original_type == "Date") {
    spanned_subsetted <- as.Date(spanned_subsetted)
  }
  spanned_subsetted
}

check_filter_on <- function(x) {
  if (!any(x %in% c("year", "mon", "mday", "wday", "hour", "min", "sec"))) {
    stop("invalid names in the span_list", call. = FALSE)
  }
}

adjust_year <- function(pattern_list) {
  pattern_list$year <- pattern_list$year - 1900
  pattern_list
}

# did not manage to get this to work with a generic function
# because spanned_lt$ does not seem to accept arguments whatsoever.
# need help for refactoring this
filter_one_part <- function(spanned_lt,
                            pattern_list,
                            part) {
  if (part == "year") {
    spanned_lt[spanned_lt$year %in% pattern_list$year]
  } else if (part == "mon") {
    spanned_lt[spanned_lt$mon %in% pattern_list$mon]
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

filter_subset <- function(spanned,
                          pattern_list) {
  for(prt in names(pattern_list)) {
    spanned_lt <- filter_one_part(spanned_lt, pattern_list, prt)
  }
  spanned_lt
}
