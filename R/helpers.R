
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

is_df <- function(x){
  if (!is.data.frame(x)) {
    stop('x should be a data frame.', call. = FALSE)
  }
}

has_rows <- function(x) {
  if (nrow(x) == 0) {
    stop("x has no rows", call. = FALSE)
  }
}

check_start_and_end <- function(start_val, end_val) {
  if (!is.null(start_val)) {
    if (! (inherits(start_val, "Date") | inherits(start_val, "POSIXt")) ){
      stop("start_val should be of class Date, POSIXlt, or POSIXct", call. = FALSE)
    }
  }
  if (!is.null(end_val)) {
    if (! (inherits(end_val, "Date") | inherits(end_val, "POSIXt")) ){
      stop("end_val should be of class Date, POSIXlt, or POSIXct", call. = FALSE)
    }
  }
}


is_datetime <- function(x) {
  inherits(x, 'Date') |  inherits(x, 'POSIXt')
}

stop_not_datetime <- function(x) {
  if (!is_datetime(x)) {
    stop('x should be of class Date, POSIXct, or POSIXlt.', call. = FALSE)
  }
}


# These two Roxygen tags are required to use Cpp code (they could be anywhere
# in the package)

#' @useDynLib padr
#' @importFrom Rcpp sourceCpp
NULL
