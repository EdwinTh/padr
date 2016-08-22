#' Determine the interval of a date variable.
#'
#' The interval is the lowest time level that can explain all instances of a
#' variable of class \code{Date} or of class \code{POSIXct}. This function
#' will determine what the interval of the variable is. It is used within
#' the \code{pad_date} function when the interval is not provided.
#' @param x A variable of class of class \code{Date} or of class \code{POSIXt}.
#' @return A character string indicating the interval of \code{x}.
#' @examples
#' x_month <- seq(as.Date('2016-01-01'), as.Date('2016-05-01'), by = 'month')
#' get_interval(x_month)

get_interval <- function(x) {
  not    <- magrittr::not
  equals <- magrittr::equals
  if(c('Date', "POSIXt") %in% class(x) %>% any %>% not) {
    stop('x should be of class Date, POSIXct, or POSIXlt', call. = FALSE)
  }

  x_char <- strftime(x)
  if(unique(nchar(x_char)) == 10){
    x_char <- paste(x_char, '00:00:00')
  }

  differs <- c(
    year   = substr(x_char, 1, 4) %>% unique %>% length %>% equals(1) %>% not,
    month  = substr(x_char, 6, 7) %>% unique %>% length %>% equals(1) %>% not,
    day    = substr(x_char, 9, 10) %>% unique %>% length %>% equals(1) %>% not,
    hour   = substr(x_char, 12, 13) %>% unique %>% length %>% equals(1) %>% not,
    minute = substr(x_char, 15, 16) %>% unique %>% length %>% equals(1) %>% not,
    second = substr(x_char, 18, 19) %>% unique %>% length %>% equals(1) %>% not
  )

  does_differ <- differs %>% which
  if(does_differ %>% length %>% equals(0)) {
    stop("x does not vary, cannot determine the interval", call. = FALSE)
  } else {
    lowest_level <- does_differ[length(does_differ)] %>% names
    return(lowest_level)
  }
}


