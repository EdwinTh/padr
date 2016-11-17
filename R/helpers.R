
get_date_variables <- function(df){
  if (!is.data.frame(df)) {
    stop('df should be a data.frame', call. = FALSE)
  }
  classes <- lapply(df, class)
  date_classes <- (sapply(classes, function(x) 'POSIXt' %in% x) |
                     sapply(classes, function(x) 'Date' %in% x))
  return(names ( which (date_classes) ))
}

# enforce the time zone of val2 on val1, without changing its value
enforce_time_zone <- function(val1, val2) {
  tz_val1 <- attr(val1, 'tzone')
  tz_val2 <- attr(val2, 'tzone')
  if ( is.null(tz_val1) ) {
    warning(paste("coercing time zone from", tz_val1, "to", tz_val2), call. = FALSE)
    val1 <- as.POSIXct(as.character(val1), tz = tz_val2)
  } else if (tz_val1 != tz_val2) {
    warning(paste("coercing time zone from", tz_val1, "to", tz_val2), call. = FALSE)
    val1 <- as.POSIXct(as.character(val1), tz = tz_val2)
  }
  return(val1)
}

# These two Roxygen tags are required to use Cpp code (they could be anywhere
# in the package)

#' @useDynLib padr
#' @importFrom Rcpp sourceCpp
NULL
