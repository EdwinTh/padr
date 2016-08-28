#' Span a vector of dates
#'
#' The spanning functions take a vector of class \code{Date}, \code{POSIXlt}, or
#' \code{POSIXct} and span a vector of its given interval from the first instance
#' smaller than the minimum of \code{x} to the first instance larger than the
#' maximum of \code{x}. The functions are designed as hepers for the \code{thicken}.
#' function.
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct} for
#' \code{span_year} or \code{span_month}. A vector of class  \code{POSIXlt}, or
#' \code{POSIXct} for the others.
#' @param start An object of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
#' See \code{details} for further information.
#' @param end An object of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
#' #' See \code{details} for further information.
#' @details The \code{start} and \code{end} arguments can be used in two ways to
#' change the default behavior of the \code{span_} functions. First one can
#' lengthen or shorten the output by specifying a timepoint that is outside
#' the default range. Second by default the lower order time units are set to the
#' lowest possible value (that is 1 for month and day and 0 for hour, minute and
#' second). \code{start} and \code{end} can be used as an offset for these
#' default values. Of course these two functionalities are not mutually exclusive.
#' @return A vector of the same data type as \code{x}.
#' @examples
#' x <- as.POSIXct(strftime(c('2014-03-04 10:43:16',
#'                            '2014-03-05 08:22:12')))
#' span_year(x)
#' span_month(x)
#' span_day(x)
#' span_hour(x)
#' span_minute(x)
start = strftime(c('2015-03-04 10:00:00')) %>% as.POSIXct()
x = date_seq('month') %>% sort

span_year <- function(x,
                      start = NULL,
                      end   = NULL) {

  # when both start and end are specified, they must be of interval year
  if( !(start %>% is.null) & !(end %>% is.null) ) {
    interval <- get_interval(c(start, end))
    if(interval != 'year') {
      stop('When start and end are both specified in the span_year function,
they can only differ from each other in years.')
    }
  }

  if(start %>% is.null){
    start <- x %>% min
    lubridate::month(start) <- lubridate::day(start) <- 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(start) <-  lubridate::minute(start) <-
        lubridate::second(start) <- 0}
  }

  if(end %>% is.null){
    end <- x %>% max
    lubridate::year(end) <- lubridate::year(end) + 1
    lubridate::month(end) <- lubridate::day(end) <- 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(end) <- lubridate::minute(end) <-
        lubridate::second(end) <- 0
    }
  }

  seq(start, end, 'year')
}

#' @rdname span_year
span_month <- function(x,
                       start = NULL,
                       end   = NULL){

  # when both start and end are specified, they must be of interval month
  if( !(start %>% is.null) & !(end %>% is.null) ) {
    interval <- get_interval(c(start, end))
    if(!interval %in%  c('year', 'month')) {
      stop('When start and end are both specified in the span_month function,
they can only differ from each other in months.')
    }
  }

  if(start %>% is.null){
    start <- x %>% min
    lubridate::day(start) <- 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(start) <-  lubridate::minute(start) <-
        lubridate::second(start) <- 0}
  }

  if(end %>% is.null){
    end <- x %>% max
    lubridate::month(end) <- lubridate::month(end) + 1
    lubridate::day(end) <- 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(end) <- lubridate::minute(end) <-
        lubridate::second(end) <- 0}
  }
  seq(start, end, 'month')
}

#' @rdname span_year
span_day <- function(x,
                     start = NULL,
                     end   = NULL){
  if('Date' %in% class(x)){
    stop('To use span_day x should be of class POSIXt', call. = FALSE)
  }

  # when both start and end are specified, they must be of interval day
  if( !(start %>% is.null) & !(end %>% is.null) ) {
    interval <- get_interval(c(start, end))
    if(!interval %in%  c('year', 'month', 'day')) {
      stop('When start and end are both specified in the span_day function,
they can only differ from each other in days.')
    }
  }

  if(start %>% is.null){
    start <- x %>% min
    lubridate::hour(start) <-  lubridate::minute(start) <-
      lubridate::second(start) <- 0
  }

  if(end %>% is.null){
    end <- x %>% max
    lubridate::day(end) <- lubridate::day(end) + 1
    lubridate::hour(end) <- lubridate::minute(end) <-
      lubridate::second(end) <- 0
  }

  seq(start, end, 'day')
}


#' @rdname span_year
span_hour <- function(x,
                      start = NULL,
                      end   = NULL){
  if('Date' %in% class(x)){
    stop('To use span_hour x should be of class POSIXt', call. = FALSE)
  }

  # when both start and end are specified, they must be of interval hour
  if( !(start %>% is.null) & !(end %>% is.null) ) {
    interval <- get_interval(c(start, end))
    if(!interval %in%  c('year', 'month', 'day', 'hour')) {
      stop('When start and end are both specified in the span_hour function,
they can only differ from each other in hours.')
    }
  }

  if(start %>% is.null){
    start <- x %>% min
    lubridate::minute(start) <- lubridate::second(start) <- 0
  }

  if(end %>% is.null){
    end <- x %>% max
    lubridate::hour(end) <- lubridate::hour(end) + 1
    lubridate::minute(end) <- lubridate::second(end) <- 0
  }
  seq(start, end, 'hour')
}

#' @rdname span_year
span_minute <- function(x,
                        start = NULL,
                        end   = NULL) {
  if('Date' %in% class(x)){
    stop('To use span_minute x should be of class POSIXt', call. = FALSE)
  }

  # when both start and end are specified, they must be of interval minute
  if( !(start %>% is.null) & !(end %>% is.null) ) {
    interval <- get_interval(c(start, end))
    if(!interval %in%  c('year', 'month', 'day', 'hour', 'minute')) {
      stop('When start and end are both specified in the span_minute function,
they can only differ from each other in minutes.')
    }
  }

  if(start %>% is.null){
    start <- x %>% min
    lubridate::second(start) <- 0
  }

  if(end %>% is.null){
    end <- x %>% max
    lubridate::minute(end) <- lubridate::minute(end) + 1
    lubridate::second(end) <- 0
  }
  seq(start, end, 'min')
}

