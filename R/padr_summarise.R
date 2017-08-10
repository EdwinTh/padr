#' Summarise a Datetime Variable
#'
#' For each group reports the first and the last observations, the interval, and
#' wether the time series is complete (if not it needs padding).
#' @param x A data frame containing at least one variable of class \code{Date},
#' class \code{POSIXct} or class \code{POSIXlt}.
#' @param group Optional character vector that specifies the grouping
#' variable(s).
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt}. \code{by} indicates which variable to summarise.

padr_summarise <- function(x,
                           group = NULL,
                           by    = NULL) {
  is_df(x)
  group <- get_dplyr_groups(x, group)
  x_grouped <- dplyr::group_by(x, !!!rlang::syms(group))
  dt_var_name <- get_dt_var_and_name(x, by)$dt_var_name
  var_enq <- rlang::sym(dt_var_name)
  with_interval <- dplyr::mutate(x_grouped,
                                 interval = get_interval_try(!!var_enq))
  dplyr::summarise(with_interval,
                   first = min(!!var_enq),
                   last  = max(!!var_enq),
                   interval = min(interval),
                   complete = check_pad_complete(!!var_enq, interval))
}


check_pad_complete <- function(dt_var,
                               interval) {
  if (is.na(interval)) return(NA)
  nr_un_obs_dt_var <- length(unique(dt_var))

  length_complete  <- length(seq(min(dt_var), max(dt_var), by = interval))
  nr_un_obs_dt_var == length_complete
}
