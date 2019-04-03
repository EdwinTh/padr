#' Pad the integer column of a data frame
#'
#' \code{pad_int} fills the gaps in incomplete integer variables. It will insert
#' a record for each of the missing value. For all
#' other variables in the data frame a missing value will be inserted at the
#' padded rows.
#'
#' @param x A data frame.
#' @param by The column to be padded.
#' @param start_val The first value of the returned variable. If NULL it will
#' use the lowest value of the input variable.
#' @param end_val The last value of the returned variable. If NULL it will use
#' the highest value of the input variable.
#' @param group Optional character vector that specifies the grouping
#' variable(s). Padding will take place within the different group values.
#' @param step The step size of the returned variable.
#' @return The data frame \code{x} with the specified variable padded. All
#' non-grouping variables in the data frame will have missing values at the rows
#' that are padded.
#' @examples
#' int_df <- data.frame(x = c(2005, 2007, 2008, 2011),
#'                      val = c(3, 2, 6, 3))
#' pad_int(int_df, 'x')
#' pad_int(int_df, 'x', start_val = 2006, end_val = 2013)
#'
#' int_df2 <- data.frame(x = c(2005, 2015), val = c(3, 4))
#' pad_int(int_df2, 'x', step = 2)
#' pad_int(int_df2, 'x', step = 5)
#'
#' int_df3 <- data.frame(x = c(2005, 2006, 2008, 2006, 2007, 2009),
#'                       g = rep(LETTERS[1:2], each = 3),
#'                       val = c(6, 6, 3, 5, 4, 3))
#' pad_int(int_df3, 'x', group = 'g')
#' pad_int(int_df3, 'x', group = 'g', start_val = 2005, end_val = 2009)
#' @export

pad_int <- function(x,
                    by,
                    start_val = NULL,
                    end_val   = NULL,
                    group     = NULL,
                    step      = 1){
  is_df(x)

  group <- get_dplyr_groups(x, group)
  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
  }

  original_data_frame <- x
  x <- as.data.frame(x)

  int_var <- x[, colnames(x) == by]
  is_valid_int(int_var)

  min_max_frame <- get_min_max(x, by, group, start_val, end_val)
  warning_no_padding(min_max_frame)

  spanned <- span_all_groups(min_max_frame, step)

  if (!is.null(step)) {
    if (!is.null(start_val)) int_var <- int_var[int_var >= start_val]
    if (!is.null(end_val)) int_var <- int_var[int_var <= end_val]
    check_interval_validity(spanned$span, int_var)
  }

  colnames(x)[colnames(x) == by] <- 'span'
  return_frame <- suppressMessages(
    dplyr::left_join(spanned, x)
  )
  colnames(return_frame)[colnames(return_frame) == 'span'] <- by

  return_frame <- set_to_original_type(return_frame, original_data_frame)
  return(return_frame)
}

check_step <- function(min_max_frame,
                             step) {
  if (any( (min_max_frame$mx - min_max_frame$mn) %% step != 0)){
    stop("step is unvalid for this integer variable", call. = FALSE)
  }
}

is_valid_int <- function(int_var) {
  stopifnot(is.numeric(int_var))
  stopifnot(all(int_var %% 1 == 0))
}
