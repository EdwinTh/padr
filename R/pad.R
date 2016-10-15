#' Pad the dateteime column of a data frame or a datetime vector
#'
#' Pad will fill the gaps in incomplete datetime variables, by figuring out
#' what the pulse of the data is and what instances are missing.
#' @param x Either a data frame containing at least one datetime variable or
#' an object of class \code{Date} or class \class{POSIXt}.
#' @param pulse The pulse of the returned datetime variable. When NULL the
#' the pulse will be equal to the pulse of the input data and will be obtained
#' by applying \code{get_pulse}. If specified it can only be lower than the
#' pulse of the input data.
#' @param start_val An object of class \code{Date} or class \class{POSIXt} that
#' specifies the start of returned datatime variable. If NULL it will use the
#' lowest value of the input variable. See Details.
#' @param end_val An object of class \code{Date} or class \class{POSIXt} that
#' specifies the end of returned datatime variable. If NULL it will use the
#' highest value of the input variable.
#' @param by Only needs to be specified when x is a data frame containing
#' multiple variables that are eligable for padding. \code{by} indicates which
#' to use for padding.
#' @details The pulse of a datetime variable is the time unit at which the
#' observations occur. \code{pad} allows for eight differenct time units which
#' are from high to low \code{year}, \code{quarter}, \code{month}, \code{week},
#' \code{day}, \code{hour}, \code{min}, \code{sec}. \code{pad} will figure out
#' the pulse of the input data and will fill the gaps for the instances that
#' would be expected from the pulse but are missing from the input data. See
#' the vignette for the default behavior of \code{pad}.
#' @return If \code{x} is a data frame it the return will be the same data frame
#' with rows inserted where the datetime variable is padded. All other values
#' at the inserted rows will be NA. If \code{x} is an object of class
#' \code{Date} or class \class{POSIXt}, the return will be the padded \code{x}.
#' @examples
#' x_var <- seq(as.Date('2016-04-01'), as.Date('2017-04-01'), by = 'month')
#' x_var_incomplete <- x_var[c(1, 4, 5, 7, 9, 10, 13)]
#' all.equal(x_var, pad(x_var_incomplete))
#' # use a different pulse than the one of the data itself
#' pad(x_var_incomplete, pulse = 'day')
#'
#' library(dplyr)
#' x_df <- data.frame(x_var = x_var_incomplete,
#'                    y     = runif(length(x_var_incomplete), 10, 20) %>% round(0))
#' # forward fill the padded values with tidyr's fill
#' x_df %>% pad %>% tidyr::fill(y)
#' # TODO show an example of fill_by_value
#'
#' # padding a data.frame on group level
#' x_df_grp <- data.frame(grp  = rep(LETTERS[1:3], each =4),
#'                        y    = runif(12, 10, 20) %>% round(0),
#'                        date = sample(x_var, 12, TRUE)) %>%
#'  dplyr::arrange(grp, date)
#'
#' x_df_grp %>% dplyr::group_by(grp) %>% dplyr::do(pad(.)) %>% dplyr::ungroup %>%
#' tidyr::fill(grp)


pad <- function(x,
                pulse    = NULL,
                start_val= NULL,
                end_val  = NULL,
                by       = NULL){

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

  if(is.null(pulse)) {
    pulse <- get_pulse(dt_var)
  } else {
    if(! pulse %in% c('year','month','day','hour','min')){
      stop("Argument pulse has an invalid value.")
    }

    pulse_dt_var <- get_pulse(dt_var)

    int_hierarchy <- 1:6
    names(int_hierarchy) <- c('year','month','day','hour','min', 'sec')

    if(int_hierarchy[pulse_dt_var] > int_hierarchy[pulse]) {
      stop('The pulse of the datetime variable is higher than the pulse given,
            if you wish to pad at this pulse you should thicken and aggregate first.')
    }
  }

  span <- span_pad(dt_var, start_val, end_val, pulse)

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
# than for thicken.
span_pad <- function(x,
                     start_val = NULL,
                     end_val   = NULL,
                     pulse  =  c('year','quarter', 'month','week', 'day','hour','min', 'sec')) {

  pulse <- match.arg(pulse)

  if(is.null(start_val)) start_val <- min(x)
  if(is.null(end_val))   end_val   <- max(x)

  span <- seq(start_val, end_val, pulse)
  return(span)
}



