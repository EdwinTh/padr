# TODO: apply the two ways of determining the interval

# a) over the distinct of all dt_var values
# b) for each of the groups individual

# TODO: find a uniform way of testing the validity of the interval for each
# of the groups

pad_multiple <- function(x,
                         interval  = NULL,
                         start_val = NULL,
                         end_val   = NULL,
                         by        = NULL,
                         group     = NULL,
                         individual_interval = FALSE){
  is_df(x)

  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
  }

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

  min_max_frame <- get_min_max(x, dt_var_name, group, start_val, end_val)
  warning_no_padding(min_max_frame)

  spanned <- span_all_groups(min_max_frame, interval)

  colnames(original_data_frame)[colnames(original_data_frame) ==
                                  dt_var_name] <- 'spanned'

  return_frame  <- merge(join_frame, original_data_frame, by = cols_to_join_on,
                         all.x = TRUE)
  colnames(return_frame)[colnames(return_frame) == 'spanned'] <- dt_var_name

  return_frame <- set_to_original_type(return_frame, original_data_frame)
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
  as_data_frame(cbind(id_vars, ret))
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
