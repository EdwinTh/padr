#' Change datetime Granularity
#'
#' This function will take a datetime vector of class and changes its
#' interval level to the provided level.
#' @details The interval is moved to a higher level by 'rounding' \code{x}
#' to the closest instance of the provided \code{interval}
#' @param x A vector of class \code{Date}, \code{POSIXlt}, or \code{POSIXct},
#' or a \code{data.frame} containing at least on column of these classes.
#' @param interval The desired interval of the output, which should be higher
#' then the interval of the datetime variable. Current choices are
#' \code{year, month, day, hour, minute}.
#' @param rounding = closest Should \code{x} be rounded to the \code{closest}
#' instance of \code{interval} or should it be rounde \code{up} or \code{down}?
#' @param by If \code{x} is a \class{data.frame} and contains multiple datetime
#' variables, specify which column to pad by.
#' @param start_valChange the default start_valpoint of the time range to which
#' the datetime variable is thickened. See ?span_year for more information.
#' @param end_valChange the default end_valpoint of the time range to which
#' the datetime variable is thickened. See ?span_year for more information.
#' @return The original \code{data.frame x} with the thickened variable added
#' to it. If \code{x} is a datetime vector the return will be a
#' \code{data.frame} comprising \code{x} and the thickened variable.
#' @examples
#' X <- data.frame(day_var = seq(as.Date('2016-01-01'), as.Date('2016-12-31'), by = 'day'),
#'                 value   = runif(366, 50, 100))

thicken <- function(x,
                   interval = c('year',
                                'month',
                                'day',
                                'hour',
                                'minute'),
                   rounding = c('closest',
                                 'up',
                                 'down'),
                   by       = NULL,
                   start_val= NULL,
                   end_val  = NULL) {

  interval <- match.arg(interval)
  rounding <- match.arg(rounding)

  # Section 1: obtain datetime variable and see if the variable is valid
  arguments <- as.list(match.call())

  if(is.data.frame(x)){
    original_data_frame <- x

    if(!is.null(arguments$by)) {
      if(length(arguments$by) > 1) stop('by can indicate one variable only')
      dt_var  <- eval(arguments$by, x)
    } else {
      dt_var_name <- get_date_variables(x)
      if(length(dt_var_name) == 0) {
        stop('x does not contain a variabel of class Date, POSIXct, or POSIXlt',
             call. = FALSE)
      }
      if(length(dt_var_name) > 1){
        stop('x contanis multiple variables of class Date, POSIXct, or POSIXlt,
please specify which variable to use in the by argument',
             call. = FALSE)
      }
      dt_var <- x[ ,colnames(x) == dt_var_name]
    }
  } else {
    if(c('Date', "POSIXt") %in% class(x) %>% any %>% not) {
      stop('x should be of class Date, POSIXct, or POSIXlt', call. = FALSE)
    }
    dt_var <- x
  }

  # Section 2: span a variable with all the relevant instances of interval
  int_hierarchy <- 1:5
  names(int_hierarchy) <- c('year','month','day','hour','minute')
  if(int_hierarchy[get_interval(dt_var)] < int_hierarchy[interval]) {
    stop('The interval in the datetime variable is larger than the interval given,
you might be looking fo smear rather than for thicken.')
  } else if (int_hierarchy[get_interval(dt_var)] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
you might be looking for pad rather than for thicken.')
  }
}

  if(interval == 'year') {
    span <- span_year(x, start_val= start_val, end_val= end_val)
  } else if (interval == 'month') {
    span <- span_month(x, start_val, end_val)
  } else if (interval == 'day') {
    span <- span_month(x, start_val, end_val)
  } else if (interval == 'hour') {
    span <- span_hour(x, start_val, end_val)
  } else if (interval == 'minute') {
    span <- span_minute(x, start_val, end_val)
  } else {
    stop("Not reach span_function if else")
  }

  # Section 3: find for each dt_var value the the value in span to thicken to
  hour_dif <- outer(x, span, function(y,z) y-z)

  if(rounding == 'down') {
    hour_dif[hour_dif < 0]  <- Inf
  } else if (rounding == 'up') {
    hour_dif[hour_dif > 0]  <- Inf
    hour_dif <- abs(hour_dif)
  } else if (rounding == 'closest') {
    hour_dif <- abs(hour_dif)
  } else {
    stop('Not reach, bug in the rounding')
  }

  closest <- hour_dif %>% apply(1, which.min)
  closest_dif <- mapply(function(i,j){ hour_dif[i,j]},
                        1:nrow(hour_dif),
                        closest)
  return_frame <- data.frame(
    x           = x,
    thickened   = span[closest],
    x_thickened_dif = closest_dif
  )

  if(!allow_duplicates) {
    return_frame_no_dups <-
      return_frame %>%
      dplyr::group_by(thickened) %>%
      dplyr::mutate(closest_dif = min(x_thickened_dif)) %>%
      dplyr::filter(x_thickened_dif == closest_dif)

    thickened_count <-
      return_frame_no_dups %>%
      dplyr::group_by(thickened) %>%
      dplyr::mutate(nr_of_thickened = n())
    still_dups <-
      thickened_count %>%
      ungroup %>%
      dplyr::select(nr_of_thickened) %>%
      unlist %>%
      magrittr::is_greater_than(1) %>%
      any

    if(still_dups) {
      return_frame <-
        thickened_count %>%
        dplyr::filter(nr_of_thickened > 1) %>%
        select(x, thickened)
      print(return_frame)
      stop(
cat("Could not remove duplicates on thickened level.
The distance of at least two values of x is exactly equal to the thickened date.
See the above datapoints."))
    } else {
      return_frame <- return_frame_no_dups
    }
  }

}

