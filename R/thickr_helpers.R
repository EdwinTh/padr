#' Span a vector of dates
#'
#' The spanning functions take a vector of class \code{Date}, \code{POSIXlt}, or
#' \code{POSIXct} and span a vector of its given interval from the first instance
#' smaller than the minimum of \code{x} to the first instance larger than the
#' maximum of \code{x}. The functions are designed as hepers for the \code{thickr}
#' function.
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct} for
#' \code{span_year} or \code{span_month}. A vector of class  \code{POSIXlt}, or
#' \code{POSIXct} for the others.
#' @return A vector of the same data type as \code{x}.
#' @examples
#' x <- as.POSIXct(strftime(c('2014-03-04 10:43:16',
#'                            '2014-03-05 08:22:12')))
#' span_year(x)
#' span_month(x)
#' span_day(x)
#' span_hour(x)
#' span_minute(x)

span_year <- function(x) {
  start_date <- x %>% min
  lubridate::month(start_date) <- lubridate::day(start_date) <- 1
  if('POSIXt' %in% class(x)) {
    lubridate::hour(start_date) <-  lubridate::minute(start_date) <-
      lubridate::second(start_date) <- 0
  }

  end_date <- x %>% max
  lubridate::year(end_date) <- lubridate::year(end_date) + 1
  lubridate::month(end_date) <- lubridate::day(end_date) <- 1
  if('POSIXt' %in% class(x)) {
    lubridate::hour(end_date) <- lubridate::minute(end_date) <-
      lubridate::second(end_date) <- 0
  }

  seq(start_date, end_date, 'year')
}

#' @rdname span_year
span_month <- function(x){
  start_date <- x %>% min
  lubridate::day(start_date) <- 1
  if('POSIXt' %in% class(x)) {
    lubridate::hour(start_date) <-  lubridate::minute(start_date) <-
      lubridate::second(start_date) <- 0
  }

  end_date <- x %>% max
  lubridate::month(end_date) <- lubridate::month(end_date) + 1
  lubridate::day(end_date) <- 1
  if('POSIXt' %in% class(x)) {
    lubridate::hour(end_date) <- lubridate::minute(end_date) <-
      lubridate::second(end_date) <- 0
  }

  seq(start_date, end_date, 'month')
}

#' @rdname span_year
span_day <- function(x){
  if('Date' %in% class(x)){
    stop('To use span_day x should be of class POSIXt', call. = FALSE)
  }
  start_date <- x %>% min
  lubridate::hour(start_date) <-  lubridate::minute(start_date) <-
    lubridate::second(start_date) <- 0

  end_date <- x %>% max
  lubridate::day(end_date) <- lubridate::day(end_date) + 1
  lubridate::hour(end_date) <- lubridate::minute(end_date) <-
    lubridate::second(end_date) <- 0

  seq(start_date, end_date, 'day')
}

#' @rdname span_year
span_hour <- function(x){
  if('Date' %in% class(x)){
    stop('To use span_hour x should be of class POSIXt', call. = FALSE)
  }
  start_date <- x %>% min
  lubridate::minute(start_date) <- lubridate::second(start_date) <- 0

  end_date <- x %>% max
  lubridate::hour(end_date) <- lubridate::hour(end_date) + 1
  lubridate::minute(end_date) <- lubridate::second(end_date) <- 0

  seq(start_date, end_date, 'hour')
}

#' @rdname span_year
span_minute <- function(x) {
  if('Date' %in% class(x)){
    stop('To use span_minute x should be of class POSIXt', call. = FALSE)
  }
  start_date <- x %>% min
  lubridate::second(start_date) <- 0

  end_date <- x %>% max
  lubridate::minute(end_date) <- lubridate::minute(end_date) + 1
  lubridate::second(end_date) <- 0

  seq(start_date, end_date, 'min')
}

