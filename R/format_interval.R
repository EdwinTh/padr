#' From Datetime to Interval character
#'
#' Format a date variable to a character string of the interval in which the
#' events toke place.

# four functions: date, quarter, week,  posix
format_interval <- function(x,
                            start_format = "%Y-%m-%d",
                            end_format   = start_format,
                            sep          = " ",
                            end_is_start = TRUE,
                            colname      = NULL,
                            by           = NULL) {
  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by)
  } else {
    dt_var <- check_data_frame(x)
  }

  interval <- get_interval(dt_var)

  end_vals   <- find_ends_dt_var(dt_var, interval) - (1 - end_is_start)
  start_char <- strftime(dt_var, start_format)
  end_char   <- strftime(end_vals, end_format)
  interval_chars <- data_frame(new = paste(start_char, end_char, sep = sep))
  return_frame   <- dplyr::bind_cols(x, interval_chars)

  if (is.null(by)) {
    x_name <- paste(get_date_variables(x), "fmt", sep = "_")
  } else {
    x_name <- by
  }

  colnames(return_frame)[ncol(return_frame)] <- x_name
  return(return_frame)
}


# x is a datetime variable of which we need to find the next value of each instance
find_ends_dt_var <- function(x,
                             interval) {
  map_func <- function(x, i) seq(x, length.out = 2, by = i)[2]
  x_df <- data.frame(x = x)
  x_df_with_end <- dplyr::mutate(dplyr::rowwise(x_df),
                                 end = map_func(x, interval))
  x_df_with_end$end
}
