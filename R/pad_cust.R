#' Apply pad with a custom spanning
#'
#' Pad the datetime variable after `thicken_cust` is applied, using the same
#' spanning.
#' @param x A data frame containing at least one datetime variable of
#' class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' @param spanned A datetime vector to which the the datetime variable in `x`
#' should be mapped. See `subset_span` (TODO link) for quickly spanning unequally
#' spaced variables.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' \code{by} indicates which to use for thickening.
#' @param group Optional character vector that specifies the grouping
#' variable(s). Padding will take place within the different group values.
#' @return The data frame \code{x} with
#' @examples
#' @export
pad_cust <- function(x,
                     spanned,
                     by        = NULL,
                     group     = NULL){
  is_df(x)
  stop_not_datetime(spanned)
  group <- get_dplyr_groups(x, group)

  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
  }

  original_data_frame <- x
  x <- as.data.frame(x)

  # we have to get the dt_var twice, first on the original data. If there are
  # NA values in it, we have to get it again on x with NA values filtered out.

  ## This part should be refactored with pad and pad_int
  dt_var_info_original <- get_dt_var_and_name(x, by)
  dt_var_name <- dt_var_info_original$dt_var_name
  x_NA_list <- check_for_NA_pad(x, dt_var_info_original$dt_var,
                                dt_var_name)
  x <- x_NA_list$x
  x_NA <- x_NA_list$x_NA

  dt_var_info <- get_dt_var_and_name(x, by)
  dt_var      <- dt_var_info$dt_var

  check_dt_var_in_group(dt_var_name, group)
  check_same_data_type(dt_var, spanned)
  check_dt_in_spanned(dt_var, spanned)

  if (inherits(spanned, "POSIXt")) {
    spanned <- enforce_time_zone(spanned, dt_var)
  }

  group_vars_un  <- group_unique_vars(x, group)
  spanned_groups <- pad_cust_group_span(spanned, group_vars_un)

  colnames(x)[colnames(x) == dt_var_name] <- 'span'

  return_frame <- suppressMessages(
    dplyr::left_join(spanned_groups, x)
  )

  colnames(return_frame)[colnames(return_frame) == 'span'] <- dt_var_name

  return_frame <- to_original_format(return_frame,
                                     group,
                                     dt_var_name,
                                     original_data_frame)

  return_frame <- set_to_original_type(return_frame, original_data_frame)

  dplyr::bind_rows(return_frame, x_NA)
}

check_same_data_type <- function(dt_var, spanned) {
  dt_var_time <- inherits(dt_var, "POSIXt")
  spanned_time <- inherits(spanned, "POSIXt")
  if ((dt_var_time + spanned_time) == 1) {
    stop("spanned and the datetime variables of different data types",
         call. = FALSE)
  }
}


check_dt_in_spanned <- function(dt_var, spanned) {
  if (any (!dt_var %in% spanned)) {
    stop(
"Observations in the datetime variable, that are not in spanned.
       Run thicken_cust in combination with aggregation first.", call. = FALSE)
  }
}

group_unique_vars <- function(x, group) {
  if (is.null(group)) {
    NULL
  } else {
    unique(x[ ,group])
  }
}

pad_cust_group_span <- function(spanned, group_vars_un) {
  if (is.null(group_vars_un)) {
    data_frame(span = spanned)
  } else {
    spanned_df <- data.frame(span = rep(spanned, nrow(group_vars_un)))
    ind <- rep(1:nrow(group_vars_un), each = length(spanned))
    dplyr::bind_cols(spanned_df, group_vars_un[ind, ])
  }
}


