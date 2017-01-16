#' Pad the datetime column of a data frame.
#'
#' \code{pad} will fill the gaps in incomplete datetime variables, by figuring out
#' what the interval of the data is and what instances are missing. It will insert
#' a record for each of the missing time points. For all
#' other variables in the data frame a missing value will be insterted at the padded rows.
#'
#' @param x A data frame containing at least one variable of class \code{Date},
#' class \code{POSIXct} or class \code{POSIXlt}.
#' @param interval The interval of the returned datetime variable. When NULL the
#' the interval will be equal to the interval of the datetime variable. When
#' specified it can only be lower than the interval of the input data. See Details.
#' @param start_val An object of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt} that specifies the start of the returned datetime variable.
#' If NULL it will use the lowest value of the input variable.
#' @param end_val An object of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt} that specifies the end of returned datetime variable.
#' If NULL it will use the highest value of the input variable.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt}. \code{by} indicates which variable to use for padding.
#' @details The interval of a datetime variable is the time unit at which the
#' observations occur. The eight intervals in \code{padr} are from high to low
#' \code{year}, \code{quarter}, \code{month}, \code{week}, \code{day},
#' \code{hour}, \code{min}, and \code{sec}. \code{pad} will figure out
#' the interval of the input variable and will fill the gaps for the instances that
#' would be expected from the interval, but are missing in the input data.
#' See \code{vignette("padr")} for more information on \code{pad}.
#' See \code{vignette("padr_implementation")} for detailed information on
#' daylight savings time, different timezones, and the implementation of
#' \code{thicken}.
#' @return The data frame \code{x} with the datetime variable padded. All other variables
#' in the data frame will have missing values at the rows that are padded.
#' @examples
#' simple_df <- data.frame(day = as.Date(c('2016-04-01', '2016-04-03')),
#'                         some_value = c(3,4))
#' pad(simple_df)
#'
#' library(dplyr) # for the pipe operator
#' month <- seq(as.Date('2016-04-01'), as.Date('2017-04-01'),
#'               by = 'month')[c(1, 4, 5, 7, 9, 10, 13)]
#' month_df <- data.frame(month = month,
#'                        y = runif(length(month), 10, 20) %>% round)
#' # forward fill the padded values with tidyr's fill
#' month_df %>% pad %>% tidyr::fill(y)
#'
#' # or fill all y with 0
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

  if (!is.data.frame(x)) {
    stop('x should be a data frame.')
  }

  arguments <- as.list(match.call())
  if (!missing(by)) by_val <- as.character(arguments$by)

  original_data_frame <- x
  x <- as.data.frame(x)

  if ('by' %in% names(arguments)){
    dt_var <- check_data_frame(x, by = by_val)
    dt_var_name <- by_val
  } else {
    dt_var <- check_data_frame(x)
    dt_var_name <- get_date_variables(x)
  }

  if (!all(dt_var[1:(length(dt_var) - 1)] <= dt_var[2:length(dt_var)])) {
    dt_var <- sort(dt_var)
    warning('Datetime variable was unsorted, pad result is sorted.')
  }

  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year', 'quarter', 'month', 'week', 'day', 'hour', 'min', 'sec')

  if (is.null(interval)) {
    interval <- get_interval(dt_var)
  } else {

    interval_dt_var <- get_interval(dt_var)

    if (int_hierarchy[interval_dt_var] > int_hierarchy[interval]) {
      stop('The interval of the datetime variable is higher than the interval given,
            if you wish to pad at this interval you should thicken and aggregate first.')
    }
  }

  # When start_val or end_val are of a different time zone, coerce to tz of dt_var
  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
  }

  if (inherits(end_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(end_val, dt_var)
  }

  # if we want to pad a lower level than the dt_interval, we need to make it
  # a posix first to do proper padding
  if (inherits(dt_var, 'Date') & int_hierarchy[interval] > 5) {
     dt_var <- as.POSIXct(as.character(dt_var))
  }

  if (! is.null(start_val )) {
    dt_var <- to_posix(dt_var, start_val)$a
    start_val <- to_posix(dt_var, start_val)$b
  }

  if (! is.null(end_val )) {
    dt_var <- to_posix(dt_var, end_val)$a
    end_val <- to_posix(dt_var, end_val)$b
  }

  # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(original_data_frame) == dt_var_name)
  original_data_frame[, pos] <- dt_var

  check_start_end(dt_var, start_val, end_val, interval)

  spanned <- span_pad(dt_var, start_val, end_val, interval)

  join_frame <- data.frame(spanned = spanned)

  colnames(original_data_frame)[colnames(original_data_frame) ==
                                dt_var_name] <- 'spanned'
  return_frame  <- merge(join_frame, original_data_frame, by = 'spanned',
                         all.x = TRUE)
  colnames(return_frame)[colnames(return_frame) == 'spanned'] <- dt_var_name

  return_frame <- set_to_original_type(return_frame, original_data_frame)
  return(return_frame)
}

# when spanning for pad we want to allow for an end_val that is (far) after
# max(x), when spanning for thicken this is not sensible. Since spanning for
# pad is simple, rather make a simple span_pad function than adjusting the
# main span function for it.
span_pad <- function(
  x,
  start_val = NULL,
  end_val   = NULL,
  interval  =  c('year', 'quarter', 'month', 'week', 'day', 'hour', 'min', 'sec')
) {

  interval <- match.arg(interval)

  if (is.null(start_val)) start_val <- min(x)
  if (is.null(end_val))   end_val   <- max(x)

  span <- seq(start_val, end_val, interval)
  return(span)
}

# Throw an error when start_val and / or end_val are not in sync with the interval
check_start_end <- function(dt_var, start_val, end_val, interval){
  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year', 'quarter', 'month', 'week', 'day', 'hour', 'min', 'sec')
  all_elements <- list(start_val, dt_var, end_val)
  all_non_null <- all_elements[sapply(all_elements, function(x) !is.null(x))]
  all_non_null <- do.call('c', all_non_null)
  necesarry_interval <- get_interval(all_non_null)
  if (int_hierarchy[necesarry_interval] > int_hierarchy[interval]) {
    stop ('start_val and/or end_val are invalid for the given combination of interval and the datetime variable') # nolint
  }
}
