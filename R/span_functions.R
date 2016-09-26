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
#' @param start_val An object of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
#' See \code{details} for further information.
#' @param end_va lAn object of class \code{Date}, \code{POSIXlt}, or \code{POSIXct}.
#' #' See \code{details} for further information.
#' @details The \code{start_val} and \code{end_val} arguments can be used in two ways to
#' change the default behavior of the \code{span_} functions. First one can
#' lengthen or shorten the output by specifying a timepoint that is outside
#' the default range. Second by default the lower order time units are set to the
#' lowest possible value (that is 1 for month and day and 0 for hour, minute and
#' second). \code{start} and \code{end} can be used as an offset for these
#' default values. Of course these two functionalities are not mutually exclusive.
#'
#'
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
                      start_val = NULL,
                      end_val   = NULL) {

  # Initialize
  start_at_null <- min(x)

  lubridate::month(start_at_null) <- lubridate::day(start_at_null) <- 1
  if( 'POSIXt' %in% class(x) ) {
    lubridate::hour(start_at_null) <-
      lubridate::minute(start_at_null) <-
      lubridate::second(start_at_null) <- 0
  }

  if( !is.null(start_val) ){
    end_offset <- start_val - start_at_null
  }

  end_at_null <- max(x)
  lubridate::year(end_at_null) <- lubridate::year(end_at_null) + 1
  lubridate::month(end_at_null) <- lubridate::day(end_at_null) <- 1
  if('POSIXt' %in% class(x)) {
    lubridate::hour(end_at_null) <-
      lubridate::minute(end_at_null) <-
      lubridate::second(end_at_null) <- 0
    }

  if( !is.null(end_val) ){
    start_offset <- end_val- end_at_null
  }

  # Assign
  if( !is.null(start_val) & !is.null(end_val) ) {

    if(get_interval(c(start_val, end_val)) != 'year') {
      stop('When start_val and end_val are both specified in the span_year function,
           they can only differ from each other in years.')
    }

    start_seq <- start_val
    end_seq   <- end_val

  } else if( !is.null(start_val) & is.null(end_val) ){

    start_seq <- start_val
    end_seq   <- end_at_null + end_offset

  } else if ( is.null(start_val) & !is.null(end_val) ) {

    start_seq <- start_at_null + start_offset
    end_seq   <- end_val

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  span <- seq(start_seq, end_seq, 'year')
  if(class(x)[1] == 'POSIXlt') span <- span %>% as.POSIXlt
  return(span)
}


####################
#### span_month ####
####################
#' @rdname span_year
span_month <- function(x,
                       start_val= NULL,
                       end_val  = NULL){

  # Initialize
  start_at_null <- min(x)
  lubridate::day(start_at_null) <- 1
  if('POSIXt' %in% class(x)) {
    lubridate::hour(start_at_null) <-
      lubridate::minute(start_at_null) <-
      lubridate::second(start_at_null) <- 0
  }

  if( !is.null(start_val) ){
    end_offset <- start_val- start_at_null
  }

  end_at_null <- max(x)
  lubridate::day(end_at_null) <- 1
  lubridate::month(end_at_null) <- lubridate::month(end_at_null) + 1
  if('POSIXt' %in% class(x)) {
      lubridate::hour(end_at_null) <-
        lubridate::minute(end_at_null) <-
        lubridate::second(end_at_null) <- 0
  }

  if( !is.null(end_val) ){
    start_offset <- end_val- end_at_null
  }

  # Specify start_valand end_valvalues for the spanned sequence
  if( !is.null(start_val) & !is.null(end_val) ) {

    if(!get_interval(c(start_val, end_val)) %in%  c('year', 'month')) {
      stop('When start_val and end_val are both specified in the span_month function,
           they can only differ from each other in months.')
    }

    start_seq <- start_val
    end_seq   <- end_val


  } else if( !is.null(start_val) & is.null(end_val) ){

    start_seq <- start_val
    end_seq   <- end_at_null + end_offset

  } else if ( is.null(start_val) & !is.null(end_val) ) {

    start_seq <- start_at_null + start_offset
    end_seq   <- end_val

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  span <- seq(start_seq, end_seq, 'month')
  if(class(x)[1] == 'POSIXlt') span <- span %>% as.POSIXlt
  return(span)
}


##################
#### span_day ####
##################
#' @rdname span_year
span_day <- function(x,
                     start_val= NULL,
                     end_val  = NULL){
  # Initialize
  if('Date' %in% class(x)){
    stop('To use span_day x should be of class POSIXt', call. = FALSE)
  }

  start_at_null <- min(x)
  lubridate::hour(start_at_null) <-
    lubridate::minute(start_at_null) <-
    lubridate::second(start_at_null) <- 0

  if( !is.null(start_val) ){
    end_offset <- start_val- start_at_null
  }

  end_at_null <- max(x)
  lubridate::day(end_at_null) <- lubridate::day(end_at_null) + 1
  if('POSIXt' %in% class(x)) {
    lubridate::hour(end_at_null) <- lubridate::minute(end_at_null) <- lubridate::second(end_at_null) <- 0
  }

  if( !is.null(end_val) ){
    start_offset <- end_val- end_at_null
  }

  # Assign
  if( !(start_val %>% is.null) & !(end_val%>% is.null) ) {
    if(!get_interval(c(start_val, end_val)) %in%  c('year', 'month', 'day')) {
      stop('When start_val and end_val are both specified in the span_day function,
           they cannot differ from each other in an interval level lower than days')
    }

    start_seq <- start_val
    end_seq   <- end_val

  } else if( !is.null(start_val) & is.null(end_val) ){

    start_seq <- start_val
    end_seq   <- end_at_null + end_offset

  } else if ( is.null(start_val) & !is.null(end_val) ) {

    start_seq <- start_at_null + start_offset
    end_seq   <- end_val

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  if( 'POSIXt' %in% class(x) ) {

    span <- seq(start_seq, end_seq, by =  'DSTday')
    if(class(x)[1] == 'POSIXlt') span <- span %>% as.POSIXlt

  } else {

    span <- seq(start_seq, end_seq, by =  'day')

  }

  return(span)
}

###################
#### span_hour ####
###################
#' @rdname span_year
span_hour <- function(x,
                      start_val= NULL,
                      end_val  = NULL){
  # Initialize
  if('Date' %in% class(x)){
    stop('To use span_day x should be of class POSIXt', call. = FALSE)
  }

  start_at_null <- min(x)
  lubridate::minute(start_at_null) <- lubridate::second(start_at_null) <- 0

  if( !is.null(start_val) ){
    end_offset <- start_val- start_at_null
  }

  end_at_null <- max(x)
  lubridate::hour(end_at_null) <- lubridate::hour(end_at_null) + 1
  if('POSIXt' %in% class(x)) {
    lubridate::minute(end_at_null) <- lubridate::second(end_at_null) <- 0
  }

  if( !is.null(end_val) ){
    start_offset <- end_val- end_at_null
  }

  # Assign
  if( !is.null(start_val) & !is.null(end_val) ) {

    if(!get_interval(c(start_val, end_val)) %in%  c('year', 'month', 'day', 'hour')) {
      stop('When start_val and end_val are both specified in the span_hour function,
           they cannot differ from each other in an interval level lower than hours.')
    }

     start_seq <- start_val
     end_seq   <- end_val

  } else if( !is.null(start_val) & is.null(end_val) ){

    start_seq <- start_val
    end_seq   <- end_at_null + end_offset

  } else if ( is.null(start_val) & !is.null(end_val) ) {

    start_seq <- start_at_null + start_offset
    end_seq   <- end_val

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  span <- seq(start_seq, end_seq, 'hour')
  if(class(x)[1] == 'POSIXlt') span <- span %>% as.POSIXlt
  return(span)
}


#####################
#### span_minute ####
#####################
#' @rdname span_year
span_minute <- function(x,
                        start_val= NULL,
                        end_val  = NULL) {


  # Initialize
  if('Date' %in% class(x)){
    stop('To use span_day x should be of class POSIXt', call. = FALSE)
  }

  start_at_null <- min(x)
  lubridate::second(start_at_null) <- 0

  if( !is.null(start_val) ){
    end_offset <- start_val- start_at_null
  }

  end_at_null <- max(x)

  if('POSIXt' %in% class(x)) {
    lubridate::minute(end_at_null) <- lubridate::minute(end_at_null) + 1
    lubridate::second(end_at_null) <- 0
  }

  if( !is.null(end_val) ){
    start_offset <- end_val- end_at_null
  }


  # Assign
  if( !is.null(start_val) & !is.null(end_val) ) {

    if(!get_interval(c(start_val, end_val)) %in%  c('year', 'month', 'day', 'hour', 'min')) {
      stop('When start_val and end_val are both specified in the span_minute function,
           they cannot differ from each other in an interval level lower than minutes.')
    }

    start_seq <- start_val
    end_seq   <- end_val

  } else if( !is.null(start_val) & is.null(end_val) ){

    start_seq <- start_val
    end_seq   <- end_at_null + end_offset

  } else if ( is.null(start_val) & !is.null(end_val) ) {

    start_seq <- start_at_null + start_offset
    end_seq   <- end_val

  } else {

    start_seq <- start_at_null
    end_seq   <- end_at_null

  }

  span <- seq(start_seq, end_seq, 'min')
  if(class(x)[1] == 'POSIXlt') span <- span %>% as.POSIXlt
  return(span)
}


