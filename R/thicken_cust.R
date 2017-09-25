#' Apply thicken with a custom spanning
#'
#' Like `thicken` this function will find the datetime variable in `x`, and add
#' a variable of a higher periodicity to it. However, the variable to which to
#' map the observation is provided by the user. This enables mapping to
#' time points that are unequally spaced.
#'
#' @param x A data frame containing at least one datetime variable of
#' class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' @param spanned A datetime vector to which the the datetime variable in `x`
#' should be mapped. See `subset_span` (TODO link) for quickly spanning unequally
#' spaced variables.
#' @param colname The column name of the added variable.
#' @param rounding Should a value in the input datetime variable be mapped to
#' the closest value that is lower (\code{down}) or that is higher (\code{up})
#' than itself.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or class \code{POSIXlt}.
#' \code{by} indicates which to use for thickening.
#' @return The data frame \code{x} with the variable added to it.
#' @examples
#' library(dplyr)
#' # analysis of traffic accidents in traffic jam hours and other hours.
#' accidents <- emergency %>% filter(title == "Traffic: VEHICLE ACCIDENT -")
#' spanning <- span_time("20151210 16", "20161017 17", tz = "EST") %>%
#'   subset_span(list(hour = c(6, 9, 16, 19)))
#' thicken_cust(accidents, spanning, "period") %>%
#'   count(period)
thicken_cust <- function(x,
                         spanned,
                         colname,
                         rounding = c('down',
                                     'up'),
                         by        = NULL) {

  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  dt_var_info <- get_dt_var_and_name(x, by)
  dt_var      <- dt_var_info$dt_var
  dt_var_name <- dt_var_info$dt_var_name

  rounding <- match.arg(rounding)

  if (check_for_sorting(dt_var)){
    warning('Datetime variable was unsorted, result will be unsorted as well.',
            call. = FALSE)
  }

  is_datetime(spanned)
  start_val <- min(spanned)
  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
    spanned   <- enforce_time_zone(spanned, dt_var)
  }

  ind_to_keep <- start_val_after_min_dt(start_val, dt_var)
  x <- x[ind_to_keep, , drop = FALSE] #nolint
  dt_var <- dt_var[ind_to_keep]

  if (is.null(by)) by <- dt_var_name

  dt_var <- check_for_NA_thicken(dt_var, dt_var_name, colname)

  check_ecapsulate(dt_var, spanned)

  thickened <- round_thicken(dt_var, spanned, rounding)

  thickened_frame <- data.frame(thickened)

  return_frame <- dplyr::bind_cols(x, thickened_frame)
  colnames(return_frame)[ncol(return_frame)] <- colname

  set_to_original_type(return_frame, original_data_frame)
}

