#' Determine the Interval of a Date Variable
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

  if( !(c('Date', "POSIXt") %in% class(x) %>% any)) {
    stop('x should be of class Date, POSIXct, or POSIXlt', call. = FALSE)
  }

  x_char <- strftime(x)
  if(unique(nchar(x_char)) == 10){
    x_char <- paste(x_char, '00:00:00')
  }

  differs <- c(
    year   = substr(x_char, 1, 4) %>% unique %>% length %>% `==`(1) %>% `!`,
    month  = substr(x_char, 6, 7) %>% unique %>% length %>% `==`(1) %>% `!`,
    day    = substr(x_char, 9, 10) %>% unique %>% length %>% `==`(1) %>% `!`,
    hour   = substr(x_char, 12, 13) %>% unique %>% length %>% `==`(1) %>% `!`,
    min    = substr(x_char, 15, 16) %>% unique %>% length %>% `==`(1) %>% `!`,
    sec    = substr(x_char, 18, 19) %>% unique %>% length %>% `==`(1) %>% `!`
  )

  does_differ <- differs %>% which

  if(does_differ %>% length %>% `==`(0)) {
    stop("x does not vary, cannot determine the interval", call. = FALSE)
  } else {
    lowest_level <- does_differ[length(does_differ)] %>% names

    if(lowest_level == 'month') {
      # quarter must be a special case of month, in order to be a quarter all
      # months must be 1 of 4, irrespective of their intervals
      # (difftime stops at months so we need this hack)
      m <- lubridate::month(x)
      months <- all(m %in% c(1,4,7,10)) | all(m %in% c(2,5,8,11)) |
                      all(m %in% c(3,6,9,12))
      if(months) lowest_level <- 'quarter'
    } else if(lowest_level == 'day') {
      weeks <- difftime(x[2:length(x)], x[1:(length(x)-1)], units = 'weeks') %>%
        as.numeric %>% `%%`(1) %>% `==`(0) %>% all
      if(weeks) lowest_level <- 'week'
    }
    return(lowest_level)
  }
}

#' Look for Variables that are of class \code{Date} or class \code{POSIXt}
#'
#' This function is used within \code{pad_date} to find the variable to pad by,
#' so the user doesn't have to specify it if the variable only contains one
#' date variable.
#' @param df An object of class \code{data.frame}.
#' @return A character vector of the column names of the variables that are
#' of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}. If \code{df} does
#' not contain any variable thes classes the character vector will be empty.
#' @examples
#' df_with_date <- data.frame(
#' x = seq(as.Date('2014-01-01), as.Date('2014-12-01'), by = 'month),
#' y = runif(12))
#' get_date_variables(df_with_date)
#' df_with_dates <- copy(df_with_date)
#' df_with_dates$x2 <- seq(as.Date('2015-01-01), as.Date('2015-12-01'),
#' by = 'month)
#' get_date_variables(df_with_dates)
#' get_date_variables(mtcars)

get_date_variables <- function(df){
  if(!is.data.frame(df)) {
    stop('df should be a data.frame', call. = FALSE)
  }
  classes <- lapply(df, class)
  date_classes <- (sapply(classes, function(x) 'POSIXt' %in% x) |
    sapply(classes, function(x) 'Date' %in% x)) %>%
    which %>%
    names
  return(date_classes)
}


# These two Roxygen tags are required to use Cpp code (they could be anywhere
# in the package)

#' @useDynLib padr
#' @importFrom Rcpp sourceCpp
NULL
