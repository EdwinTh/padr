

span_year_pad <- function(x,
                         start_val = NULL,
                         end_val   = NULL) {

  if(is.null(start_val)) start_val <- min(x)
  if(is.null(end_val))   end_val   <- max(x)
  span <- seq(start_val, end_val, 'year')
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

    if(!get_interval(c(start, end_val)) %in%  c('year', 'month')) {
      stop('When start_valand end_valare both specified in the span_month function,
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
  #  if('Date' %in% class(x)){
  #    stop('To use span_day x should be of class POSIXt', call. = FALSE)
  #  }

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
    if(!get_interval(c(start, end_val)) %in%  c('year', 'month', 'day')) {
      stop('When start_val and end_valare both specified in the span_day function,
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

    if(!get_interval(c(start, end_val)) %in%  c('year', 'month', 'day', 'hour')) {
      stop('When start_valand end_valare both specified in the span_hour function,
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

  if(class(start_seq) == "Date") {
    start_seq <- as.POSIXct(start_seq)
    lubridate::hour(start_seq) <- 0
    end_seq <- as.POSIXct(end_seq)
    lubridate::hour(end_seq) <- 0
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

    if(!get_interval(c(start, end_val)) %in%  c('year', 'month', 'day', 'hour', 'min')) {
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

  if(class(start_seq) == "Date") {
    start_seq <- as.POSIXct(start_seq)
    lubridate::hour(start_seq) <- 0
    end_seq <- as.POSIXct(end_seq)
    lubridate::hour(end_seq) <- 0
  }


  span <- seq(start_seq, end_seq, 'min')
  if(class(x)[1] == 'POSIXlt') span <- span %>% as.POSIXlt
  return(span)
}


