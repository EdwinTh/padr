#' Pad the datetime column of a data frame.
#'
#' \code{pad} will fill the gaps in incomplete datetime variables, by figuring out
#' what the interval of the data is and what instances are missing. It will insert
#' a record for each of the missing time points. For all
#' other variables in the data frame a missing value will be insterted at the padded rows.
#'
#' @param x A data frame containing at least one variable of class \code{Date},
#' class \code{POSIXct} or class \code{POSIXlt}.
#' @param interval The interval of the returned datetime variable.
#' Any character string that would be accepted by \code{seq.Date()} or
#' \code{seq.POSIXt}. When NULL the
#' the interval will be equal to the interval of the datetime variable. When
#' specified it can only be lower than the interval and step size of the input data.
#' See Details.
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
#' variable(s). Padding will take place within the different group values. Note
#' that when the interval is not specified, `get_interval` will be applied on
#' the datetime variable within each group separately. When the incoming intervals
#' differ, the output intervals will differ as well.
#' @details The interval of a datetime variable is the time unit at which the
#' observations occur. The eight intervals in \code{padr} are from high to low
#' \code{year}, \code{quarter}, \code{month}, \code{week}, \code{day},
#' \code{hour}, \code{min}, and \code{sec}. Since \code{padr} v.0.3.0 the
#' interval is no longer limited to be of a single unit.
#' (Intervals like 5 minutes, 6 hours, 10 days are possible). \code{pad} will figure out
#' the interval of the input variable and the step size, and will fill the gaps for the instances that
#' would be expected from the interval and step size, but are missing in the input data.
#' See \code{vignette("padr")} for more information on \code{pad}.
#' See \code{vignette("padr_implementation")} for detailed information on
#' daylight savings time, different timezones, and the implementation of
#' \code{thicken}.
#' @return The data frame \code{x} with the datetime variable padded. All
#' non-grouping variables in the data frame will have missing values at the rows
#' that are padded.
#' @examples
#' simple_df <- data.frame(day = as.Date(c('2016-04-01', '2016-04-03')),
#'                         some_value = c(3,4))
#' pad(simple_df)
#' pad(simple_df, interval = "day")
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
#' x_df_grp %>% pad(group = c('grp1', 'grp2'), interval = "month")
#' @export
pad <- function(x,
                interval  = NULL,
                start_val = NULL,
                end_val   = NULL,
                by        = NULL,
                group     = NULL){
  is_df(x)

  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
  }

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
    dt_var_name <- by
  } else {
    dt_var <- check_data_frame(x)
    dt_var_name <- get_date_variables(x)
  }

  ### Make sure start_val, end_val and dt_var are same data type ####
  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
  }

  if (inherits(end_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    end_val <- enforce_time_zone(end_val, dt_var)
  }

  if (! is.null(start_val )) {
    dt_var <- to_posix(dt_var, start_val)$a
    start_val <- to_posix(dt_var, start_val)$b
  }

  if (! is.null(end_val )) {
    dt_var <- to_posix(dt_var, end_val)$a
    end_val <- to_posix(dt_var, end_val)$b
  }

  # get the interval of the datetime variable, either single value or a
  # vector
  interval_dt_var <- get_interval_list(unique(dt_var))

  if (!is.null(interval)) {
    interval_converted <- convert_interval(interval)
    interval_converted$interval <- uniform_interval_name(interval_converted$interval)
  } else {
    interval_converted <- NULL
  }

  # if we want to pad a lower level than the dt_interval, we need to make it
  # a posix first to do proper padding
  if (!is.null(interval_converted)) {
    if (inherits(dt_var, 'Date') &
        interval_converted$interval %in% c("hour", "min", "sec")) {
      dt_var <- as.POSIXct(as.character(dt_var))
      x[colnames(x) == dt_var_name] <- dt_var
    }
  }

  if (is.null(interval_converted)) {
    interval <- interval_list_to_string(interval_dt_var)
  } else {
    interval <- interval_list_to_string(interval_converted)
  }

  # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(x) == dt_var_name)
  x[, pos] <- dt_var

  # do the spanning, either with or without the individual groups
  min_max_frame <- get_min_max(x, dt_var_name, group, start_val, end_val)
  warning_no_padding(min_max_frame)

  spanned <- span_all_groups(min_max_frame, interval)

  if (!is.null(interval)) {
    check_interval_validity(spanned$span, dt_var)
  }

  colnames(x)[colnames(x) ==
                                  dt_var_name] <- 'span'

  return_frame <- suppressMessages(
    dplyr::left_join(spanned, x)
  )

  return_frame <- set_to_original_type(return_frame, original_data_frame)

  colnames(return_frame)[colnames(return_frame) == 'span'] <- dt_var_name

  return_frame <- to_original_format(return_frame,
                                     group,
                                     dt_var_name,
                                     original_data_frame)


  interval_message(interval)
  return(return_frame)
}


get_min_max <- function(x,
                        dt_var,
                        group_vars,
                        start_val,
                        end_val) {
  grpd <- dplyr::group_by_(x, .dots = group_vars)

  funcs <- list(sprintf("min(%s)", dt_var),
                sprintf("max(%s)", dt_var))

  ret <- dplyr::summarise_(grpd, .dots = setNames(funcs, c("mn", "mx")))
  if (!is.null(start_val)) ret$mn <- start_val
  if (!is.null(end_val)) ret$mx <- end_val
  ret <- dplyr::ungroup(ret)
  return(ret)
}

warning_no_padding <- function(x) {
  start_equal_to_end <- x$mn == x$mx
  if (any(start_equal_to_end)){
    if (length(start_equal_to_end) == 1) {
      warning("date time variable does not vary, returning x without padding",
              call. = FALSE)
    } else {
      not_varying <- sum(start_equal_to_end)
      warning(sprintf("date time variable does not vary for %d of the groups, no padding for these groups",
                      not_varying), call. = FALSE)
    }
  }
}

# id_vars is a data frame with one row containing the single value,
# the column names are returned in the result
# Worker of span_all_groups function
span_from_min_max_single <- function(start,
                                     end,
                                     interval,
                                     id_vars) {
  ret <- data.frame(span = seq(start, end, by = interval))
  return(as.data.frame(cbind(ret, id_vars)))
}

# x is the output of get_min_max
span_all_groups <- function(x, interval) {
  id_vars <- split( select(x, -mn, -mx), seq(nrow(x)))
  list_span <- mapply(span_from_min_max_single,
                      start = x$mn,
                      end   = x$mx,
                      interval = interval,
                      id_vars = id_vars,
                      SIMPLIFY = FALSE)
  return(do.call("rbind", list_span))
}

get_individual_interval <- function(x, dt_var_name, group_vars) {
  grpd <- dplyr::group_by_(x, .dots = group_vars)
  colnames(grpd)[colnames(grpd) == dt_var_name] <- "dt_var"
  ret <- summarise(grpd, interval = get_interval(dt_var))
  return(ungroup(ret))
}

interval_list_to_string <- function(int) {
  if (int$step == 1) {
    step <- ""
  } else {
    step <- paste(int$step, "")
  }
  return(paste(step, int$interval, sep = ""))
}


# after joining are the rows sorted on dt_var first. This needs to be on the
# keys first. Also the columns should be in the same order as the original
to_original_format <- function(ret, group_vars, dt_var_name, original_data_frame){
  sorting_fields <- c(group_vars, dt_var_name)
  ret <- dplyr::arrange_(ret, .dots = sorting_fields)
  return( dplyr::select_(ret, .dots = colnames(original_data_frame)) )
}

interval_message <- function(int) {
  message(paste("pad applied on the interval:", int))
  Sys.sleep(0.5)
}

check_interval_validity <- function(spanned, dt_var) {
  # take the unique of both so we don't do redundant stuff
  spanned_un <- unique(spanned)
  dt_var_un  <- unique(dt_var)
  if (!all(dt_var_un %in% spanned_un)) {
    stop("The specified interval is unvalid for the datetime variable, because not all original observation are in the padding.
         If you want to pad at this interval, aggregate the data first with thicken.", call. = FALSE)
  }
}
