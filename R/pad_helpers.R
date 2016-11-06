#' Get the interval of a datetime variable
#'
#' The interval is the lowest highest time unit that can explain all instances of a
#' variable of class \code{Date} or of class \code{POSIXct}. This function
#' will determine what the interval of the variable is.
#'
#' @param x A variable of class of class \code{Date} or of class \code{POSIXt}.
#' @return A character string indicating the interval of \code{x}.
#' @examples
#' x_month <- seq(as.Date('2016-01-01'), as.Date('2016-05-01'), by = 'month')
#' get_interval(x_month)
#' @export
get_interval <- function(x) {

  if( !(c('Date', "POSIXt") %in% class(x) %>% any)) {
    stop('x should be of class Date, POSIXct, or POSIXlt')
  }

  x_char <- as.character(x)
  if(unique(nchar(x_char)) == 10){
    x_char <- paste(x_char, '00:00:00')
  }

  differs <- c(
    year   = ! length( unique ( substr(x_char, 1, 4) ) ) == 1,
    month  = ! length( unique ( substr(x_char, 6, 7) ) ) == 1,
    day    = ! length( unique ( substr(x_char, 9, 10) ) ) == 1,
    hour   = ! length( unique ( substr(x_char, 12, 13) ) ) == 1,
    min    = ! length( unique ( substr(x_char, 15, 16) ) ) == 1,
    sec    = ! length( unique ( substr(x_char, 18, 19) ) ) == 1
  )

  does_differ <- differs %>% which

  if(does_differ %>% length %>% `==`(0)) {
    stop("x does not vary, cannot determine the interval", call. = FALSE)
  } else {
    lowest_level <- does_differ[length(does_differ)] %>% names

    if(lowest_level == 'month') {
      # quarter must be a special case of month, in order to be a quarter all
      # months must be 1 of 4, irrespective of time between them
      # remember $mon starts at 0 not at 1!!
      m       <- as.POSIXlt(x_char)$mon
      quarter <- all(m %in% c(1,4,7,10)) | all(m %in% c(2,5,8,11)) |
                      all(m %in% c(0, 3,6,9))
      if(quarter) lowest_level <- 'quarter'

    } else if(lowest_level == 'day') {
      distances <- difftime(x[2:length(x)], x[1:(length(x)-1)], units = 'weeks')
      weeks     <- all( ( as.numeric(distances) %% 1 ) == 0 )
      if(weeks) lowest_level <- 'week'
    }
    return(lowest_level)
  }
}

get_date_variables <- function(df){
  if(!is.data.frame(df)) {
    stop('df should be a data.frame', call. = FALSE)
  }
  classes <- lapply(df, class)
  date_classes <- (sapply(classes, function(x) 'POSIXt' %in% x) |
    sapply(classes, function(x) 'Date' %in% x))
  return(names ( which (date_classes) ))
}


# These two Roxygen tags are required to use Cpp code (they could be anywhere
# in the package)

#' @useDynLib padr
#' @importFrom Rcpp sourceCpp
NULL
