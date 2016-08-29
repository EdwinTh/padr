#' Adjust date by offset
#'
#' This function is a little helper to the span_ functions.
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
#' @param date_offset A numeric vector of respectively the year, month, day, hour,
#' minute and second to change in \code{x}.
#' @return \code{x} with the six time levels specified in \code{offset} added
#' to it.
#' @examples
#'  a_time_value <- as.POSIXct(strftime('2014-03-04 10:43:16'))
#'  desired_offset <- c(0, 2, 1, 2, 0, 18)
#'  offset_date(a_time_value, desired_offset)
offset_date <- function(x,
                        date_offset) {
  not = magrittr::not
  if(c('Date', "POSIXt") %in% class(x) %>% any %>% not) {
    stop('x should be of class Date, POSIXct, or POSIXlt', call. = FALSE)
  }
  if(c('numeric', "integer") %in% class(date_offset) %>% any %>% not) {
    stop('date_offset should be of class numeric or integer', call. = FALSE)
  }
  if( (length(date_offset) != 6) | any(is.na(date_offset)) ) {
    stop("date_offset should haven length 6, without NA's, use zeros at the positions you don't want to change", call. = FALSE)
  }
  if( date_offset[2] > 11 ){
    stop('month value cannot exceed 11, please use year offset instead', .call = FALSE)
  }

  lubridate::year(x) <- lubridate::year(x) + date_offset[1]
  new_month <- lubridate::month(x) + date_offset[2]
  new_month[new_month > 12] <- new_month[new_month > 12] %% 12
  lubridate::month(x) <- new_month
  lubridate::day(x) <- lubridate::day(x) + date_offset[3]
  lubridate::hour(x) <- lubridate::hour(x) + date_offset[4]
  lubridate::minute(x) <- lubridate::minute(x) + date_offset[5]
  lubridate::second(x) <- lubridate::second(x) + date_offset[6]
  return(x)
}

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

###################
#### span_year ####
###################
span_year <- function(x,
                      start = NULL,
                      end   = NULL) {

  # Get the necessary values for all the scenarios, this will make the ifelse
  # down a lot easier to follow.
  if( is.null(start) ){
    start_at_null <- min(x)
    lubridate::month(start_at_null) <- lubridate::day(start_at_null) <- 1
    if( 'POSIXt' %in% class(x) ) {
      lubridate::hour(start_at_null) <-  lubridate::minute(start_at_null) <- lubridate::second(start_at_null) <- 0
      }
  } else {
    end_offset <- c(lubridate::year(start), lubridate::month(start),
                    lubridate::day(start), lubridate::hour(start),
                    lubridate::minute(start), lubridate::second(start))
  }

  if( is.null(end) ) {
    end_at_null <- max(x)
    lubridate::year(end_at_null) <- lubridate::year(end_at_null) + 1
    lubridate::month(end_at_null) <- lubridate::day(end_at_null) <- 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(end_at_null) <- lubridate::minute(end_at_null) <- lubridate::second(end_at_null) <- 0
    }
  } else {
    start_offset <- c(lubridate::year(end), lubridate::month(end),
                      lubridate::day(end), lubridate::hour(end),
                      lubridate::minute(end), lubridate::second(end))
  }

  # Specify start and end values for the spanned sequence
  if( !is.null(start) & !is.null(end) ) {

    if(get_interval(c(start, end)) != 'year') {
      stop('When start and end are both specified in the span_year function,
           they can only differ from each other in years.')}

    start_seq <- start
    end_seq   <- end

  } else if( !is.null(start) & is.null(end) ){

    start_seq <- start
    end_seq   <- offset_date(end_at_null, end_offset)

  } else if ( is.null(start) & !is.null(end) ) {

    start_seq <- offset_date(start_at_null, start_offset)
    end_seq   <- end

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  seq(start_seq, end_seq, 'year')
}


####################
#### span_month ####
####################
#' @rdname span_year
span_month <- function(x,
                       start = NULL,
                       end   = NULL){

  if( is.null(start) ){
    start <- min(x)
    lubridate::day(start) <- 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(start) <-  lubridate::minute(start) <- lubridate::second(start) <- 0
      }
  } else {
    end_offset <- c(lubridate::year(start), lubridate::month(start),
                    lubridate::day(start), lubridate::hour(start),
                    lubridate::minute(start), lubridate::second(start))
  }

  if( is.null(end) ) {
    end <- max(x)
    lubridate::month(end) <- lubridate::month(end) + 1
    lubridate::day(end) <- 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(end) <- lubridate::minute(end) <- lubridate::second(end) <- 0
    }
  } else {
    start_offset <- c(lubridate::year(end), lubridate::month(end),
                      lubridate::day(end), lubridate::hour(end),
                      lubridate::minute(end), lubridate::second(end))
  }

  # Specify start and end values for the spanned sequence
  if( !is.null(start) & !is.null(end) ) {

    if(!get_interval(c(start, end)) %in%  c('year', 'month')) {
      stop('When start and end are both specified in the span_month function,
           they can only differ from each other in months.')
    }

    start_seq <- start
    end_seq   <- end

  } else if( !is.null(start) & is.null(end) ){

    start_seq <- start
    end_seq   <- offset_date(end_at_null, end_offset)

  } else if ( is.null(start) & !is.null(end) ) {

    start_seq <- offset_date(start_at_null, start_offset)
    end_seq   <- end

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  seq(start_seq, end_seq, 'month')
}


##################
#### span_day ####
##################
#' @rdname span_year
span_day <- function(x,
                     start = NULL,
                     end   = NULL){
  if('Date' %in% class(x)){
    stop('To use span_day x should be of class POSIXt', call. = FALSE)
  }

  if( is.null(start) ){
    start <- min(x)
    lubridate::hour(start) <- lubridate::minute(start) <- lubridate::second(start) <- 0
  } else {
    end_offset <- c(lubridate::year(start), lubridate::month(start),
                    lubridate::day(start), lubridate::hour(start),
                    lubridate::minute(start), lubridate::second(start))
  }

  if( is.null(end) ) {
    end <- max(x)
    lubridate::day(end) <- lubridate::day(end) + 1
    if('POSIXt' %in% class(x)) {
      lubridate::hour(end) <- lubridate::minute(end) <- lubridate::second(end) <- 0
    }
  } else {
    start_offset <- c(lubridate::year(end), lubridate::month(end),
                      lubridate::day(end), lubridate::hour(end),
                      lubridate::minute(end), lubridate::second(end))
  }

  # when both start and end are specified, they must be of interval day
  if( !(start %>% is.null) & !(end %>% is.null) ) {
    if(!get_interval(c(start, end)) %in%  c('year', 'month', 'day')) {
      stop('When start and end are both specified in the span_day function,
           they cannot differ from each other in an interval level lower than days')
    }

    start_seq <- start
    end_seq   <- end

  } else if( !is.null(start) & is.null(end) ){

    start_seq <- start
    end_seq   <- offset_date(end_at_null, end_offset)

  } else if ( is.null(start) & !is.null(end) ) {

    start_seq <- offset_date(start_at_null, start_offset)
    end_seq   <- end

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  seq(start_seq, end_seq, 'day')
}

###################
#### span_hour ####
###################
#' @rdname span_year
span_hour <- function(x,
                      start = NULL,
                      end   = NULL){
  if('Date' %in% class(x)){
    stop('To use span_hour x should be of class POSIXt', call. = FALSE)
  }

  # Initialize
  if( is.null(start) ){
    start <- min(x)
    lubridate::minute(start) <- lubridate::second(start) <- 0
  } else {
    end_offset <- c(lubridate::year(start), lubridate::month(start),
                    lubridate::day(start), lubridate::hour(start),
                    lubridate::minute(start), lubridate::second(start))
  }

  if( is.null(end) ) {
    end <- max(x)
    lubridate::hour(end) <- lubridate::hour(end) + 1
    if('POSIXt' %in% class(x)) {
      lubridate::minute(end) <- lubridate::second(end) <- 0
    }
  } else {
    start_offset <- c(lubridate::year(end), lubridate::month(end),
                      lubridate::day(end), lubridate::hour(end),
                      lubridate::minute(end), lubridate::second(end))
  }

  # Assign
  if( !is.null(start) & !is.null(end) ) {

    if(!get_interval(c(start, end)) %in%  c('year', 'month', 'day', 'hour')) {
      stop('When start and end are both specified in the span_hour function,
           they cannot differ from each other in an interval level lower than hours.')
    }

     start_seq <- start
     end_seq   <- end

    } else if( !is.null(start) & is.null(end) ){

      start_seq <- start
      end_seq   <- offset_date(end_at_null, end_offset)

    } else if ( is.null(start) & !is.null(end) ) {

      start_seq <- offset_date(start_at_null, start_offset)
      end_seq   <- end

    } else {

      start_seq <- start_at_null
      end_seq   <- end_at_null

    }

  seq(start_seq, end_seq, 'hour')
}


#####################
#### span_minute ####
#####################
#' @rdname span_year
span_minute <- function(x,
                        start = NULL,
                        end   = NULL) {
  if('Date' %in% class(x)){
    stop('To use span_hour x should be of class POSIXt', call. = FALSE)
  }

  # Initialize
  if( is.null(start) ){
    start <- min(x)
    lubridate::second(start) <- 0
  } else {
    end_offset <- c(lubridate::year(start), lubridate::month(start),
                    lubridate::day(start), lubridate::hour(start),
                    lubridate::minute(start), lubridate::second(start))
  }

  if( is.null(end) ) {
    end <- max(x)
    lubridate::minute(end) <- lubridate::minute(end) + 1
    if('POSIXt' %in% class(x)) {
      lubridate::second(end) <- 0
    }
  } else {
    start_offset <- c(lubridate::year(end), lubridate::month(end),
                      lubridate::day(end), lubridate::hour(end),
                      lubridate::minute(end), lubridate::second(end))
  }


  if('Date' %in% class(x)){
    stop('To use span_minute x should be of class POSIXt', call. = FALSE)
  }

  # Assign
  if( !is.null(start) & !is.null(end) ) {

    if(!get_interval(c(start, end)) %in%  c('year', 'month', 'day', 'hour', 'minute')) {
      stop('When start and end are both specified in the span_minute function,
           they cannot differ from each other in an interval level lower than minutes.')
    }

    start_seq <- start
    end_seq   <- end

    } else if( !is.null(start) & is.null(end) ){

      start_seq <- start
      end_seq   <- offset_date(end_at_null, end_offset)

    } else if ( is.null(start) & !is.null(end) ) {

      start_seq <- offset_date(start_at_null, start_offset)
      end_seq   <- end

    } else {

      start_seq <- start_at_null
      end_seq   <- end_at_null

    }

  seq(start, end, 'min')
}

