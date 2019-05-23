#' Thicken with a custom spanning
#'
#' Like \code{thicken}, it will find the datetime variable in \code{x}
#' and add a variable of a higher periodicity to it. However, the variable to
#' which to map the observation is provided by the user. This enables mapping to
#' time points that are unequally spaced.
#'
#' @param x A data frame containing at least one datetime variable of
#' class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' @param spanned A datetime vector to which the the datetime variable in
#' \code{x} should be mapped.
#' @param colname Character, the column name of the added variable.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, \code{POSIXct} or \code{POSIXlt}.
#' Indicates which to use for thickening.
#' @param drop Should the original datetime variable be dropped from the
#' returned data frame? Defaults to \code{FALSE}.
#' @details
#' Only rounding down is available for custom thickening.
#' @return The data frame \code{x} with the variable added to it.
#' @examples
#' library(dplyr)
#' # analysis of traffic accidents in traffic jam hours and other hours.
#' accidents <- emergency %>% filter(title == "Traffic: VEHICLE ACCIDENT -")
#' spanning <- span_time("20151210 16", "20161017 17", tz = "EST") %>%
#'   subset_span(list(hour = c(6, 9, 16, 19)))
#' thicken_cust(accidents, spanning, "period") %>%
#'   count(period) %>%
#'   pad_cust(spanning)
#'@export
thicken_cust <- function(x,
                         spanned,
                         colname,
                         by   = NULL,
                         drop = FALSE) {

  is_df(x)
  has_rows(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  dt_var_info <- get_dt_var_and_name(x, by)
  dt_var      <- dt_var_info$dt_var
  dt_var_name <- dt_var_info$dt_var_name

  is_datetime(spanned)
  if (inherits(spanned, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    spanned   <- enforce_time_zone(spanned, dt_var)
  }

  warning_when_filtering(dt_var, spanned)

  ind_to_keep <- start_val_after_min_dt(min(spanned), dt_var)

  x <- x[ind_to_keep, , drop = FALSE] #nolint
  dt_var <- dt_var[ind_to_keep]

  if (is.null(by)) by <- dt_var_name

  dt_var <- check_for_NA_thicken(dt_var, dt_var_name, colname)

  thickened <- round_thicken(dt_var, spanned, "down", ties_to_earlier = FALSE)

  thickened_frame <- data.frame(thickened)

  return_frame <- dplyr::bind_cols(x, thickened_frame)
  colnames(return_frame)[ncol(return_frame)] <- colname

  if (drop) return_frame <- remove_original_var(return_frame, dt_var_name)

  set_to_original_type(return_frame, original_data_frame)
}


warning_when_filtering <- function(dt_var, spanned) {
  if (min(dt_var) < min(spanned)) {
    warning("Dropping all values in the datetime var that are smaller than smallest spanned",
            call. = FALSE)
  }
}

end_val_before_max_dt <- function(end_val, dt_var) {
  end_val <- to_posix(end_val, dt_var)$a
  dt_var  <- to_posix(end_val, dt_var)$b
  dt_var < end_val
}
