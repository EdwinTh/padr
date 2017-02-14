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
#' @param group Optional character vector that specifies the grouping
#' variable(s). Padding will take place within the different group values.
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
#' x_df_grp <- data.frame(grp1 = rep(LETTERS[1:3], each =4),
#'                        grp2 = letters[1:2],
#'                        y    = runif(12, 10, 20) %>% round(0),
#'                        date = sample(day_var, 12, TRUE)) %>%
#'  arrange(grp1, grp2, date)
#'
#' # pad by one grouping var
#' x_df_grp %>% pad(group = 'grp1')
#'
#' # pad by two groups vars
#' x_df_grp %>% pad(group = c('grp1', 'grp2'))

#' @export
pad <- function(x,
                interval  = NULL,
                start_val = NULL,
                end_val   = NULL,
                by        = NULL,
                group     = NULL){

  # this is preferred over as.list(match.call()) because of magrittr
  pad_args <- list(x         = x,
                   interval  = interval,
                   start_val = start_val,
                   end_val   = end_val,
                   by        = by,
                   group     = group)


  if (is.null(group)) {
    do.call(pad_single, pad_args)
  } else {
    do.call(pad_multiple, pad_args)
  }
}

# Function is almost equal to the previous function of pad, however it does
# padding with grouping vars. If group is NULL nothing changes to v 0.1.0.

# Note that the group here is different from the main pad. In the main function
# it is the character vector indicating which variable(s) to use for grouping.
# Here it is a single instance of the grouping variables, in a data frame.
# pad_single should be applied to each of the unique keys.
pad_single  <- function(x,
                        interval  = NULL,
                        start_val = NULL,
                        end_val   = NULL,
                        by        = NULL,
                        group     = NULL){

  if (!is.data.frame(x)) {
    stop('x should be a data frame.')
  }

  int_hierarchy <- get_int_hierarchy()

  arguments <- as.list(match.call())
  if (!is.null(by)) by_val <- as.character(arguments$by)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by_val)
    dt_var_name <- by_val
  } else {
    dt_var <- check_data_frame(x)
    dt_var_name <- get_date_variables(x)
  }

  # When start_val or end_val are of a different time zone, coerce to tz of dt_var
  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
  }

  if (inherits(end_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(end_val, dt_var)
  }

  if (! is.null(start_val )) {
    dt_var <- to_posix(dt_var, start_val)$a
    start_val <- to_posix(dt_var, start_val)$b
  }

  if (! is.null(end_val )) {
    dt_var <- to_posix(dt_var, end_val)$a
    end_val <- to_posix(dt_var, end_val)$b
  }


  # If we have just one value specified it depends on start_val / end_val what to do
  if (length(unique(dt_var)) == 1 ) {

    if (is.null(start_val) && is.null(end_val) ) {
      warning ('datetime variable contains one value only and start_val and end_val are not specified,\n  returning x without padding') #nolint
      return(x)
    }

  } else {

    if ( !all(dt_var[1:(length(dt_var) - 1)] <= dt_var[2:length(dt_var)] ) ) {
      dt_var <- sort(dt_var)
      warning('Datetime variable was unsorted, pad result is sorted.')
    }

  }

  interval <- check_interval(dt_var, start_val, end_val, interval)

  # if we want to pad a lower level than the dt_interval, we need to make it
  # a posix first to do proper padding
  if (inherits(dt_var, 'Date') & int_hierarchy[interval] > 5) {
    dt_var <- as.POSIXct(as.character(dt_var))
  }

  # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(original_data_frame) == dt_var_name)
  original_data_frame[, pos] <- dt_var

  spanned <- span_pad(dt_var, start_val, end_val, interval)

  join_frame <- data.frame(spanned = spanned)

  cols_to_join_on <- 'spanned'

  # we simply add the keys before joining
  if (!is.null(group)) {
    stopifnot(is.data.frame(group))
    # cbind gives a warning when row names are unequal with base df's
    join_frame <- suppressWarnings( cbind(join_frame,
                                          as.data.frame(group)) )
    cols_to_join_on <- c(cols_to_join_on, colnames(group))
  }

  colnames(original_data_frame)[colnames(original_data_frame) ==
                                dt_var_name] <- 'spanned'


  return_frame  <- merge(join_frame, original_data_frame, by = cols_to_join_on,
                         all.x = TRUE)
  colnames(return_frame)[colnames(return_frame) == 'spanned'] <- dt_var_name

  return_frame <- set_to_original_type(return_frame, original_data_frame)
  return(return_frame)
}

# This is the wrapper around pad_single

pad_multiple <- function(x,
                         interval  = NULL,
                         start_val = NULL,
                         end_val   = NULL,
                         by        = NULL,
                         group     = group){
  stopifnot(is.data.frame(x))
  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.')
  }

  groupings <- unique(x[, colnames(x) %in% group, drop = FALSE])
  padded_groups <- vector("list", nrow(groupings))


  for (i in 1:nrow(groupings)){

    indic_mat <- matrix(0, nrow(x), ncol(groupings))
    for (j in 1:ncol(groupings)){
      indic_mat[, j] <-
        unlist(x[, colnames(x) == group[j]]) == unlist(groupings[i, j])
    }

    x_iter <- x[rowSums(indic_mat) == ncol(groupings), ]

    # because we span a data frame with the grouping vars included, we want to
    # remove them from the data frame going into pad

    pad_args <- list(x         = x_iter,
                     interval  = interval,
                     start_val = start_val,
                     end_val   = end_val,
                     by        = by,
                     group     = groupings[i, , drop = FALSE]) # nolint

    padded_groups[[i]] <- do.call(pad_single, pad_args)
  }
  return(do.call("rbind", padded_groups))
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

# This is a generic function that:
# a) checks if the given interval is valid, if the interval is given.
# b) returns the required interval for padding.
check_interval <- function(dt_var,
                           start_val,
                           end_val,
                           interval){
  int_hierarchy <- get_int_hierarchy()
  all_elements <- rbind(data.frame(total_pad = start_val),
                        data.frame(total_pad = dt_var),
                        data.frame(total_pad = end_val))
  necesarry_interval <- get_interval(all_elements$total_pad)

  if (!is.null(interval)) {
    if (int_hierarchy[necesarry_interval] > int_hierarchy[interval]) {
      stop (
'The interval of the datetime variable  is higher than the desired interval,
possibly in combination with the start_val and / or end _val.
Pad only works with intervals that are equal or lower.
If you wish to pad at this interval you should thicken and aggregate first.')
    }
    necesarry_interval <- interval
  }
  return(necesarry_interval)
}



# small helper to make an int_hierarchy
get_int_hierarchy <- function(x) {
  int_hierarchy <- 1:8
  names(int_hierarchy) <- c('year', 'quarter', 'month', 'week', 'day', 'hour',
                            'min', 'sec')
  return(int_hierarchy)
}
