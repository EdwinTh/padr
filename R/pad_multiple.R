# TODO: interval assurance before joining,
# - check if all the spanned is in the original data.

pad_multiple <- function(x,
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
  interval_dt_var <- get_interval(unique(dt_var))

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
    x[colnames(x) == dt_var_name] <- dt_var
  }

  # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(original_data_frame) == dt_var_name)
  original_data_frame[, pos] <- dt_var

  # do the spanning, either with or without the individual groups
  min_max_frame <- get_min_max(x, dt_var_name, group, start_val, end_val)
  warning_no_padding(min_max_frame)

  interval_string <- interval_list_to_string(interval)

  spanned <- span_all_groups(min_max_frame, interval_string)

  colnames(original_data_frame)[colnames(original_data_frame) ==
                                  dt_var_name] <- 'span'

  return_frame  <- merge(spanned, original_data_frame,
                         all.x = TRUE)
  return_frame <- set_to_original_type(return_frame, original_data_frame)

  return_frame <- to_original_format(return_frame,
                                     group,
                                     dt_var,
                                     original_data_frame)

  colnames(return_frame)[colnames(return_frame) == 'span'] <- dt_var_name

  interval_message(interval)
  return(return_frame)
}

get_min_max <- function(x,
                        dt_var,
                        group_vars,
                        start_val,
                        end_val) {
  grpd <- group_by_(x, .dots = group_vars)

  funcs <- list(sprintf("min(%s)", dt_var),
                sprintf("max(%s)", dt_var))

  ret <- summarise_(grpd, .dots = setNames(funcs, c("mn", "mx")))
  if (!is.null(start_val)) ret$mn <- start_val
  if (!is.null(end_val)) ret$mx <- end_val
  ret <- ungroup(ret)
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
  ret <- data_frame(span = seq(start, end, by = interval))
  as_data_frame(cbind(ret, id_vars))
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
  bind_rows(list_span)
}

get_individual_interval <- function(x, dt_var_name, group_vars) {
  grpd <- group_by_(x, .dots = group_vars)
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

# after joining are the rows sorted on day first. This needs to be on the
# keys first. Also the columns should be in the same orderas the original
to_original_format <- function(ret, group_vars, dt_var, original_data_frame){
  if (!is.null(group_vars)) {
    ret <- arrange_(ret, .dots = group_vars)
  }
  return( select_(ret, .dots = colnames(original_data_frame)) )
}

interval_message <- function(int) {
  interval <- interval_list_to_string(int)
  message(paste("pad applied on the interval:", interval))
}
