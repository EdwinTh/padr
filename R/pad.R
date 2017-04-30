
pad_old <- function(x,
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
                        group     = NULL,
                        give_message = TRUE){
  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
    dt_var_name <- by
  } else {
    dt_var <- check_data_frame(x)
    dt_var_name <- get_date_variables(x)
  }

  # When start_val or end_val are of a different time zone, coerce to tz of dt_var
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

  # If we have just one value specified it depends on start_val / end_val what to d
  return_x_here <- pad_warnings(dt_var, start_val, end_val)
  if (return_x_here) return(x)

  if (!is.null(interval)) {
    interval_converted <- convert_interval(interval)
    interval_converted$interval <- uniform_interval_name(interval_converted$interval)
  } else {
    interval_converted <- NULL
  }

  interval <- check_interval(dt_var, start_val, end_val, interval_converted)

  # if we want to pad a lower level than the dt_interval, we need to make it
  # a posix first to do proper padding
  if (inherits(dt_var, 'Date') & interval$interval %in% c("hour", "min", "sec")) {
    dt_var <- as.POSIXct(as.character(dt_var))
  }

  # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(original_data_frame) == dt_var_name)
  original_data_frame[, pos] <- dt_var

  spanned <- span_pad(dt_var, interval, start_val, end_val)

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

  if (give_message) interval_message(interval)
  return(return_frame)
}

# This is the wrapper around pad_single

pad_multiple <- function(x,
                         interval  = NULL,
                         start_val = NULL,
                         end_val   = NULL,
                         by        = NULL,
                         group     = group){
  is_df(x)

  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
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
                     group     = groupings[i, , drop = FALSE], # nolint
                     give_message = FALSE)

    padded_groups[[i]] <- do.call(pad_single, pad_args)
  }

  if (is.null(interval)) {
    warning("Interval was not supplied when padding groups. Not guaranteed all groups are of the same interval.") #nolint
  }
  return(do.call("rbind", padded_groups))
}

# when spanning for pad we want to allow for an end_val that is (far) after
# max(x), when spanning for thicken this is not sensible. Since spanning for
# pad is simple, rather make a simple span_pad function than adjusting the
# main span function for it.
span_pad <- function(x,
                     interval,
                     start_val = NULL,
                     end_val   = NULL) {

  if (is.null(start_val)) start_val <- min(x)
  if (is.null(end_val))   end_val   <- max(x)

  interval_str <- paste(interval$step, interval$interval)

  span <- seq(start_val, end_val, interval_str)
  return(span)
}

# This is a generic function that:
# a) checks if the given interval is valid, if the interval is given.
# b) returns the required interval for padding.
check_interval <- function(dt_var,
                           start_val,
                           end_val,
                           interval){

  all_elements <- rbind(data.frame(total_pad = start_val),
                        data.frame(total_pad = dt_var),
                        data.frame(total_pad = end_val))
  necesarry_interval <- get_interval_list(all_elements$total_pad)


  if (!is.null(interval)) {
    interval_higher <- convert_int_to_hours(interval) >
      convert_int_to_hours(necesarry_interval)

    if (interval_higher) {
      stop ('The interval of the datetime variable is higher than the desired interval,
  possibly in combination with the start_val and / or end _val.
  Pad only works when the desired interval is equal or lower than the current interval.
  If you wish to pad at this interval you should thicken and aggregate first.', call. = FALSE)
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


pad_warnings <- function(dt_var, start_val, end_val) {
  if (length(unique(dt_var)) == 1 ) {

    if (is.null(start_val) && is.null(end_val) ) {
      warning ('datetime variable contains one value only and start_val and end_val are not specified.\nReturning x without padding.', call. = FALSE) #nolint
      return(TRUE)
    }

  } else {

    if ( !all(dt_var[1:(length(dt_var) - 1)] <= dt_var[2:length(dt_var)] ) ) {
      dt_var <- sort(dt_var)
      warning('Datetime variable was unsorted, pad result is sorted.', call. = FALSE)
    }
  }
  return(FALSE)
}


interval_message <- function(int) {
  if (int$step == 1) {
    step <- ""
  } else {
    step <- paste(int$step, "")
  }
  interval <- paste(step, int$interval, sep = "")
  message(paste("pad applied on the interval", interval))
}
