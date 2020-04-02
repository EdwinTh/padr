#' Pad the datetime column of a data frame
#'
#' \code{pad} will fill the gaps in incomplete datetime variables, by figuring out
#' what the interval of the data is and what instances are missing. It will insert
#' a record for each of the missing time points. For all
#' other variables in the data frame a missing value will be inserted at the padded rows.
#'
#' @param x A data frame containing at least one variable of class \code{Date},
#' \code{POSIXct} or \code{POSIXlt}.
#' @param interval The interval of the returned datetime variable.
#' Any character string that would be accepted by \code{seq.Date()} or
#' \code{seq.POSIXt}. When NULL the
#' the interval will be equal to the interval of the datetime variable. When
#' specified it can only be lower than the interval and step size of the input data.
#' See Details.
#' @param start_val An object of class \code{Date}, \code{POSIXct} or
#' \code{POSIXlt} that specifies the start of the returned datetime variable.
#' If NULL it will use the lowest value of the input variable.
#' @param end_val An object of class \code{Date}, \code{POSIXct} or
#' \code{POSIXlt} that specifies the end of returned datetime variable.
#' If NULL it will use the highest value of the input variable.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' Indicates which variable to use for padding.
#' @param group Optional character vector that specifies the grouping
#' variable(s). Padding will take place within the different groups. When
#' interval is not specified, it will be determined applying \code{get_interval}
#' on the datetime variable as a whole, ignoring groups (see last example).
#' @param break_above Numeric value that indicates the number of rows in millions
#' above which the function will break. Safety net for situations where the
#' interval is different than expected and padding yields a very large
#' dataframe, possibly overflowing memory.
#' @details The interval of a datetime variable is the time unit at which the
#' observations occur. The eight intervals in \code{padr} are from high to low
#' \code{year}, \code{quarter}, \code{month}, \code{week}, \code{day},
#' \code{hour}, \code{min}, and \code{sec}. Since \code{padr} v.0.3.0 the
#' interval is no longer limited to be of a single unit.
#' (Intervals like 5 minutes, 6 hours, 10 days are possible). \code{pad} will
#' figure out the interval of the input variable and the step size, and will
#' fill the gaps for the instances that would be expected from the interval and
#' step size, but are missing in the input data.
#' Note that when \code{start_val} and/or \code{end_val} are specified, they are
#' concatenated with the datetime variable before the interval is determined.
#'
#' Rows with missing values in the datetime variables will be retained.
#' However, they will be moved to the end of the returned data frame.
#' @return The data frame \code{x} with the datetime variable padded. All
#' non-grouping variables in the data frame will have missing values at the rows
#' that are padded. The result will always be sorted on the datetime variable.
#' If \code{group} is not \code{NULL} result is sorted on grouping variable(s)
#' first, then on the datetime variable.
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
#'
#' # Using group argument the interval is determined over all the observations,
#' # ignoring the groups.
#' x <- data.frame(dt_var = as.Date(c("2017-01-01", "2017-03-01", "2017-05-01",
#' "2017-01-01", "2017-02-01", "2017-04-01")),
#' id = rep(1:2, each = 3), val = round(rnorm(6)))
#' pad(x, group = "id")
#' # applying pad with do, interval is determined individualle for each group
#' x %>% group_by(id) %>% do(pad(.))
#' @export
pad <- function(x,
                interval  = NULL,
                start_val = NULL,
                end_val   = NULL,
                by        = NULL,
                group     = NULL,
                break_above = 1){
  is_df(x)
  has_rows(x)

  check_start_and_end(start_val, end_val)
  group <- get_dplyr_groups(x, group)

  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
  }

  original_data_frame <- x
  x <- as.data.frame(x)
  original_interval <- interval

  # we have to get the dt_var twice, first on the original data. If there are
  # NA values in it, we have to get it again on x with NA values filtered out.
  dt_var_info_original <- get_dt_var_and_name(x, by)
  dt_var_name <- dt_var_info_original$dt_var_name
  x_NA_list <- check_for_NA_pad(x, dt_var_info_original$dt_var,
                                dt_var_name)
  x <- x_NA_list$x
  x_NA <- x_NA_list$x_NA

  dt_var_info <- get_dt_var_and_name(x, by)
  dt_var      <- dt_var_info$dt_var

  check_dt_var_in_group(dt_var_name, group)

  ### Make sure start_val, end_val and dt_var are same data type ####
  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
  }

  if (inherits(end_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    end_val <- enforce_time_zone(end_val, dt_var)
  }

  if (! is.null(start_val)) {
    dt_var <- to_posix(dt_var, start_val)$a
    start_val <- to_posix(dt_var, start_val)$b
  }

  if (! is.null(end_val)) {
    dt_var <- to_posix(dt_var, end_val)$a
    end_val <- to_posix(dt_var, end_val)$b
  }

  # get the interval over all the groups, this way it is assured all groups
  # are the same. Only get the interval when interval is NULL
  if (is.null(interval)) {
    interval_dt_var <- get_interval_start_end(dt_var, start_val, end_val)
    if (interval_dt_var[[1]] == "return x here") {
      return(x)
    }
  }

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
    interval <- flatten_interval(interval_dt_var)
  } else {
    interval <- flatten_interval(interval_converted)
  }

  # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(x) == dt_var_name)
  x[, pos] <- dt_var

  # do the spanning, either with or without the individual groups
  min_max_frame <- get_min_max(x, dt_var_name, group, start_val, end_val)
  min_max_frame <- check_invalid_start_and_end(min_max_frame)

  return_rows <- get_return_rows(min_max_frame, interval)
  break_above_func(return_rows, break_above)

  warning_no_padding(min_max_frame)

  spanned <- span_all_groups(min_max_frame, interval)

  if (!is.null(interval)) {
    if (!is.null(start_val)) dt_var <- dt_var[dt_var >= start_val]
    if (!is.null(end_val)) dt_var <- dt_var[dt_var <= end_val]
    check_interval_validity(spanned$span, dt_var)
  }

  colnames(x)[colnames(x) == dt_var_name] <- 'span'

  return_frame <- suppressMessages(
    dplyr::left_join(spanned, x)
  )

  colnames(return_frame)[colnames(return_frame) == 'span'] <- dt_var_name

  return_frame <- to_original_format(return_frame,
                                     group,
                                     dt_var_name,
                                     original_data_frame)

  return_frame <- set_to_original_type(return_frame, original_data_frame)

  if (is.null(original_interval)) {
    interval_message(interval)
  }

  return_frame <- dplyr::bind_rows(return_frame, x_NA)

  return(return_frame)
}


get_min_max <- function(x,
                        dt_var_name,
                        group_vars,
                        start_val,
                        end_val) {
  dt_var_enq     <- rlang::sym(dt_var_name)
  group_vars_enq <- rlang::syms(group_vars)
  grpd           <- dplyr::group_by(x, !!!group_vars_enq)

  ret <- dplyr::summarise(grpd, mn = min(!!dt_var_enq), mx = max(!!dt_var_enq))
  if (!is.null(start_val)) ret$mn <- start_val
  if (!is.null(end_val))   ret$mx <- end_val
  ret <- dplyr::ungroup(ret)
  return(ret)
}

warning_no_vary <- function(msg_second_part) {
  function(x) {
    start_equal_to_end <- x$mn == x$mx
    if (any(start_equal_to_end)){
      not_varying <- sum(start_equal_to_end)
    warning(sprintf("datetime variable does not vary for %d of the groups, %s", #nolint
                    not_varying, msg_second_part), call. = FALSE)
    }
  }
}

warning_no_padding <- warning_no_vary("no padding applied on this / these group(s)")

# if start_val or end_val are specified, we want omit the cases where the
# start_val is larger than max(x) and end_val is smaller than min(x)
# x is the output of get_min_max
check_invalid_start_and_end <- function(x) {
  x$invalid <- x$mn > x$mx
  total_invalid <- sum(x$invalid)

  if (total_invalid == nrow(x)) {
    if (nrow(x) == 1) {
      stop("start value is larger than the end value.", call. = FALSE)
    } else {
      stop("start value is larger than the end value for all groups.", call. = FALSE)
    }
  }

  if (total_invalid > 0) {
    warning(sprintf("%d group(s) for which the start value is larger than the end value, removed from return.", #nolint
                    total_invalid), call. = FALSE)
  }
  x <- x[!x$invalid, ]
  x <- x[, colnames(x) != "invalid"]
  return(x)
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
  select_index <- which(!colnames(x) %in% c("mn", "mx"))
  id_vars <- split( dplyr::select(x, select_index), seq(nrow(x)))
  stop_int64(id_vars)

  list_span <- mapply(span_from_min_max_single,
                      start = x$mn,
                      end   = x$mx,
                      interval = interval,
                      id_vars = id_vars,
                      SIMPLIFY = FALSE)

  dplyr::bind_rows(list_span)
}

# currently int64 gives so much trouble, I chose to just break for now.
stop_int64 <- function(id_var_df) {
  classes_id <- unlist(sapply(id_var_df, class))
  if ("integer64" %in% classes_id) {
    stop("pad currently does not support group variables of class integer64",
         call. = FALSE)
  }
}

flatten_interval <- function(int) {
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
  ret <- dplyr::arrange(ret, !!!rlang::syms(sorting_fields))
  ret <- dplyr::select(ret, !!!rlang::syms(colnames(original_data_frame)))
  return(as.data.frame(ret))
}

interval_message <- function(int) {
  message(paste("pad applied on the interval:", int))
}

check_interval_validity <- function(spanned, dt_var, date_time = TRUE) {
  # take the unique of both so we don't do redundant stuff
  spanned_un <- unique(spanned)
  dt_var_un  <- unique(dt_var)

  if (date_time) {
    message <- "The specified interval is invalid for the datetime variable.
  Not all original observation are in the padding.
  If you want to pad at this interval, aggregate the data first with thicken."
  } else {
    message <- "Invalid step size for the given integer variable.
  Not all original observation are in the padding."
  }

  if (!all(dt_var_un %in% spanned_un)) {
    stop(message, call. = FALSE)
  }
}

get_interval_start_end <- function(dt_var, start_val, end_val) {
  dt_var <- c(dt_var, start_val, end_val)
  if (length(unique(dt_var)) == 1) {
    warning("Datetime variable does not vary, returning x without padding",
            call. = FALSE)
    return("return x here")
  } else {
    return(get_interval_list(dt_var))
  }
}

get_return_rows <- function(min_max_frame, interval) {
  diffs <- min_max_frame$mx - min_max_frame$mn
  if (attributes(diffs)$units %in% c("day", "days")) {
    diffs <- diffs * 24 * 60 * 60
  }
  seconds_to_pad <- sum(diffs)
  interval_in_hours <-
    convert_int_to_hours(make_interval_list_from_string(interval))
  interval_in_seconds <- interval_in_hours * 3600
  return(as.numeric(seconds_to_pad / interval_in_seconds))
}

get_dplyr_groups <- function(x, group) {

  dplyr_groups <- dplyr::groups(x)

  if (length(dplyr_groups) == 0) {
    return(group)
  } else {
    dplyr_groups <- as.character(dplyr_groups)

    if (!is.null(group) & (!all(dplyr_groups %in% group)) ) {
      warning("group argument and dplyr::groups are both present and differ, dplyr::groups are ignored", #nolint
              call. = FALSE) # nolint
      return(group)
    }
  }
  return(dplyr_groups)
}

break_above_func <- function(n,
                             threshold) {
  if (n > (threshold * 10 ^ 6)) {
    stop(sprintf("Estimated %s returned rows, larger than %s million in break_above",
                 n, threshold), call. = FALSE)
  }

}

get_dt_var_and_name <- function(x, by) {
  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
    dt_var_name <- by
  } else {
    dt_var <- check_data_frame(x)
    dt_var_name <- get_date_variables(x)
  }
  list(dt_var = dt_var, dt_var_name = dt_var_name)
}

check_for_NA_pad <- function(x, dt_var, dt_var_name) {
  x_no_NA <- x
  x_NA <- NULL
  if (anyNA(dt_var)) {
    x_no_NA <- x[!is.na(dt_var), ]
    x_NA <- x[is.na(dt_var), ]
    warn_mess <- sprintf(
"There are NA values in the column %s. The records with NA values are returned
in the final rows of the dataframe.",
      dt_var_name
    )
    warning(warn_mess, call. = FALSE)
  }
  list(x = x_no_NA, x_NA = x_NA)
}


check_dt_var_in_group <- function(dt_var_name, group) {
  if (dt_var_name %in% group) {
    stop(sprintf("%s cannot be in de grouping variables", dt_var_name))
  }
}
