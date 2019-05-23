#' Pad with a custom spanning
#'
#' Pad the datetime variable after \code{thicken_cust} is applied, using the same
#' spanning.
#' @param x A data frame containing at least one datetime variable of
#' class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' @param spanned A datetime vector to which the the datetime variable in
#' \code{x} should be mapped. See \code{subset_span} for quickly spanning
#' unequally spaced variables.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' @param group Optional character vector that specifies the grouping
#' variable(s). Padding will take place within the different group values.
#' @param drop_last_spanned Logical, indicating whether to drop the last value
#' from \code{spanned}. The spanned is typically around the datetime variable.
#' This would create an empty last record when padding. Setting to \code{TRUE}
#' will drop the last value in \code{spanned} and will not create an empty
#' last record in this situation.
#' @return The data frame \code{x} with the datetime column padded.
#' @examples
#' library(dplyr)
#' # analysis of traffic accidents in traffic jam hours and other hours.
#' accidents <- emergency %>% filter(title == "Traffic: VEHICLE ACCIDENT -")
#' spanning <- span_time("20151210 16", "20161017 17", tz = "EST") %>%
#'   subset_span(list(hour = c(6, 9, 16, 19)))
#' thicken_cust(accidents, spanning, "period") %>%
#'   count(period) %>%
#'   pad_cust(spanning)
#' @export
pad_cust <- function(x,
                     spanned,
                     by        = NULL,
                     group     = NULL,
                     drop_last_spanned = TRUE){
  is_df(x)
  has_rows(x)

  stop_not_datetime(spanned)
  group <- get_dplyr_groups(x, group)

  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
  }

  original_data_frame <- x
  x <- as.data.frame(x)

  stopifnot(is.logical(drop_last_spanned))
  spanned <- spanned[1:(length(spanned) - drop_last_spanned)]

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
  if ( (dt_var_time + spanned_time) == 1) {
    stop("spanned and the datetime variables of different data types",
         call. = FALSE)
  }
}


check_dt_in_spanned <- function(dt_var, spanned) {
  if (any (!dt_var %in% spanned)) {
    stop(
"Observations in the datetime variable, that are not in spanned.
  Either run thicken_cust in combination with aggregation first, or rerun this function
  with drop_last_spanned = FALSE.", call. = FALSE)
  }
}

group_unique_vars <- function(x, group) {
  if (is.null(group)) {
    NULL
  } else {
    unique(x[, group, drop = FALSE])
  }
}

pad_cust_group_span <- function(spanned, group_vars_un) {
  if (is.null(group_vars_un)) {
    data.frame(span = spanned)
  } else {
    spanned_df <- data.frame(span = rep(spanned, nrow(group_vars_un)))
    ind <- rep(1:nrow(group_vars_un), each = length(spanned))
    dplyr::bind_cols(spanned_df, group_vars_un[ind, , drop = FALSE]) #nolint
  }
}
