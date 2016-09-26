

pad <- function(x,
                by       = NULL,
                interval = NULL,
                start_val= NULL,
                end_val  = NULL){

  arguments <- as.list(match.call())
  if('by' %in% names(arguments)) by_val <- as.character(arguments$by)

  if(is.data.frame(x)) {
    original_data_frame <- x
    x <- as.data.frame(x)
    if('by' %in% names(arguments)){
      dt_var <- check_data_frame(x, by = by_val)
      dt_var_name <- by_val
    } else {
      dt_var <- check_data_frame(x)
      dt_var_name <- get_date_variables(x)
    }
  } else {
    dt_var <- check_vector(x)
  }

  if(!all(dt_var[1:(length(dt_var)-1)] < dt_var[2:length(dt_var)])) {
    dt_var <- sort(dt_var)
    warning('Datetime variable was unsorted, pad result is sorted.')
  }

  if(is.null(interval)) {
    interval <- get_interval(dt_var)
  } else {
    if(! interval %in% c('year','month','day','hour','min')){
      stop("Argument interval has an invalid value.")
    }

    interval_dt_var <- get_interval(dt_var)

    int_hierarchy <- 1:6
    names(int_hierarchy) <- c('year','month','day','hour','min', 'sec')

    if(int_hierarchy[interval_dt_var] > int_hierarchy[interval]) {
      stop('The interval in the datetime variable is higher than the interval given,
            if you wish to pad at this interval you should thicken and aggregate first.')
    }
  }

  span <- span_pad(dt_var, start_val, end_val, interval)

  if(!is.data.frame(x)){
    return(span)
  } else {
    join_frame <- data.frame(span = span)
    colnames(original_data_frame)[colnames(original_data_frame) ==
                                    dt_var_name] <- 'span'
    return_frame <- suppressMessages(
      dplyr::right_join(original_data_frame, join_frame))
    colnames(return_frame)[colnames(return_frame) == 'span'] <- dt_var_name
    class(return_frame) <-  class(original_data_frame)
    return(return_frame)
  }
}


# this is a helper function for pad, spanning for pad is much simpler
# than for thicken. Adjusting the span_ functions for pad would make them
# too fuzzy.
span_pad <- function(x,
                     start_val = NULL,
                     end_val   = NULL,
                     interval  =  c('year','month','day','hour','min', 'sec')) {

  interval <- match.arg(interval)

  if(is.null(start_val)) start_val <- min(x)
  if(is.null(end_val))   end_val   <- max(x)

  span <- seq(start_val, end_val, interval)
  return(span)
}



