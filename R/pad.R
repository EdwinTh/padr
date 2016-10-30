#' Pad the dateteime column of a data frame or a datetime vector
#'
#' Pad will fill the gaps in incomplete datetime variables, by figuring out
#' what the interval of the data is and what instances are missing. For all
#' other variables a missing value will be insterted.
#'
#' @param x A data frame containing at least one variable of class \code{Date}
#' or of class \class{POSIXt}.
#' @param interval The interval of the returned datetime variable. When NULL the
#' the interval will be equal to the interval of the input data and will be obtained
#' by applying \code{get_interval}. If specified it can only be lower than the
#' interval of the input data.
#' @param start_val An object of class \code{Date} or class \class{POSIXt} that
#' specifies the start of returned datatime variable. If NULL it will use the
#' lowest value of the input variable. See Details.
#' @param end_val An object of class \code{Date} or class \class{POSIXt} that
#' specifies the end of returned datatime variable. If NULL it will use the
#' highest value of the input variable.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date} or of class \class{POSIXt}. \code{by}
#' indicates which to use for padding.
#' @details The interval of a datetime variable is the time unit at which the
#' observations occur. \code{pad} allows for eight different time units which
#' are from high to low \code{year}, \code{quarter}, \code{month}, \code{week},
#' \code{day}, \code{hour}, \code{min}, \code{sec}. \code{pad} will figure out
#' the interval of the input data and will fill the gaps for the instances that
#' would be expected from the interval but are missing from the input data. See
#' the vignette for the default behavior of \code{pad}.
#' @return \code{x} with the datetime variable padded. All other variables will
#' have missing values at the rows that are padded.
#' @examples
#' simple_df <- data.frame(day = as.Date(c('2016-04-01', '2016-04-03')),
#'                         some_value = c(3,4))
#' pad(simple_df)
#'
#' library(dplyr)
#' month <- seq(as.Date('2016-04-01'), as.Date('2017-04-01'),
#'               by = 'month')[c(1, 4, 5, 7, 9, 10, 13)]
#' month_df <- data.frame(month = month,
#'                        y = runif(length(month), 10, 20) %>% round)
#' # forward fill the padded values with tidyr's fill
#' month_df %>% pad %>% tidyr::fill(y)
#' # or fill it all with 0
#' month_df %>% pad %>% fill_by_value(y)
#'
#' # padding a data.frame on group level
#' day_var <- seq(as.Date('2016-01-01'), length.out = 12, by = 'month')
#' x_df_grp <- data.frame(grp  = rep(LETTERS[1:3], each =4),
#'                        y    = runif(12, 10, 20) %>% round(0),
#'                        date = sample(day_var, 12, TRUE)) %>%
#'  arrange(grp, date)
#'
#' x_df_grp %>% group_by(grp) %>% do(pad(.)) %>% ungroup %>%
#' tidyr::fill(grp)
#' @export
pad <- function(x,
                interval = NULL,
                start_val= NULL,
                end_val  = NULL,
                by       = NULL){

  if(!is.data.frame(x)) {
    stop('x should be a data frame.')
  }

  arguments <- as.list(match.call())
  if(!missing(by)) by_val <- as.character(arguments$by)

  original_data_frame <- x
  # remove optional extra classes from x, like data.table or tbl
  x <- as.data.frame(x)
  if('by' %in% names(arguments)){
    dt_var <- check_data_frame(x, by = by_val)
    dt_var_name <- by_val
  } else {
    dt_var <- check_data_frame(x)
    dt_var_name <- get_date_variables(x)
  }

  if(!all(dt_var[1:(length(dt_var)-1)] < dt_var[2:length(dt_var)])) {
    dt_var <- sort(dt_var)
    warning('Datetime variable was unsorted, pad result is sorted.')
  }

  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year','quarter', 'month', 'week', 'day', 'hour','min', 'sec')

  if(is.null(interval)) {
    interval <- get_interval(dt_var)
  } else {

    interval_dt_var <- get_interval(dt_var)

    if(int_hierarchy[interval_dt_var] > int_hierarchy[interval]) {
      stop('The interval of the datetime variable is higher than the interval given,
            if you wish to pad at this interval you should thicken and aggregate first.')
    }
  }

  # Proper handling of switching between Date and POSIX
  if(class(dt_var) == 'Date' & int_hierarchy[interval] > 5) {
     dt_var <- as.POSIXct(as.character(dt_var))
     pos    <- which(colnames(original_data_frame) == dt_var_name)
     original_data_frame[ ,pos] <- dt_var
  }

  if('Date' %in% class(dt_var) & ('POSIXt' %in% class(start_val) |
                                'POSIXt' %in% class(end_val))) {
    dt_var <- as.POSIXct(as.character(dt_var))
    pos    <- which(colnames(original_data_frame) == dt_var_name)
    original_data_frame[ ,pos] <- dt_var
  }

  if('POSIXt' %in% class(dt_var) & ('Date' %in% class(start_val) |
                                     'Date' %in% class(end_val))){
    stop('start_val and/or end_val should be of class POSIXt when the input variable is as well')
  }

  # Throw an error when start_val and / or end_val are not in sync with the interval
  all_elements <- list(start_val, dt_var, end_val)
  all_non_null <- all_elements[sapply(all_elements, function(x) !is.null(x))]
  all_non_null <- do.call('c', all_non_null)
  necesarry_interval <- get_interval(all_non_null)
  if(int_hierarchy[necesarry_interval] > int_hierarchy[interval]) {
    stop('start_val and/or end_val are invalid for the given combination of interval and the datetime variable')
  }

  spanned <- span_pad(dt_var, start_val, end_val, interval)

  join_frame <- data.frame(spanned = spanned)
  colnames(original_data_frame)[colnames(original_data_frame) ==
                                dt_var_name] <- 'spanned'
  return_frame <- suppressMessages(
    dplyr::right_join(original_data_frame, join_frame))
  colnames(return_frame)[colnames(return_frame) == 'spanned'] <- dt_var_name
  class(return_frame) <-  class(original_data_frame)
  return(return_frame)
}

# when spanning for pad we want to allow for an end_val that is (far) after
# max(x), when spanning for thicken this is not sensible. Since spanning for
# pad is simple rather make a simple span_pad function than adjusting the
# main span function for it.
span_pad <- function(x,
                     start_val = NULL,
                     end_val   = NULL,
                     interval  =  c('year','quarter', 'month','week', 'day','hour','min', 'sec')) {

  interval <- match.arg(interval)

  if(is.null(start_val)) start_val <- min(x)
  if(is.null(end_val))   end_val   <- max(x)

  span <- seq(start_val, end_val, interval)
  return(span)
}





