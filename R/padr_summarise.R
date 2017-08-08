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
#' @param invariant What to do when datetime variable does not vary for one or
#' more groups.

padr_summarise <- function(x,
                           group = NULL,
                           by    = NULL,
                           invariant = c("warning", "silent", "break")) {
  is_df(x)
  group <- get_dplyr_groups(x, group)
  x_grouped <- dplyr::group_by(x, !!!rlang::syms(group))
  dt_var_name <- get_dt_var_and_name(x, by)$dt_var_name
  var_enq <- rlang::sym(dt_var_name)
  return_set <- dplyr::summarise(x_grouped,
                                 first = min(!!var_enq),
                                 last  = max(!!var_enq),
                                 interval = get_interval(!!var_enq))
}

