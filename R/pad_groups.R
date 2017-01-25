#' Date padding within groups
#'
#' This is a wrapper around \code{pad} that does grouping on one or more
#' variables. Within each of the groups, the \code{pad} function is applied.
#' @param x A data frame containing at least one variable of class \code{Date},
#' class \code{POSIXct} or class \code{POSIXlt}.
#' @param ... The unquoted column name(s) of indicating by which variable(s) to
#' group.
#' @param interval The interval of the returned datetime variable. When NULL the
#' the interval will be equal to the interval of the datetime variable. When
#' specified it can only be lower than the interval of the input data. See Details.
#' @param start_val An object of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt} that specifies the start of the returned datetime variable.
#' If NULL it will use the lowest value of the input variable.
#' @param end_val An object of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt} that specifies the end of returned datetime variable.
#' If NULL it will use the highest value of the input variable.
#' @param by Only needs to be specified when \code{x} contains multiple
#' variables of class \code{Date}, class \code{POSIXct} or
#' class \code{POSIXlt}. \code{by} indicates which variable to use for padding.
#' @details The interval of a datetime variable is the time unit at which the
#' observations occur. The eight intervals in \code{padr} are from high to low
#' \code{year}, \code{quarter}, \code{month}, \code{week}, \code{day},
#' \code{hour}, \code{min}, and \code{sec}. \code{pad} will figure out
#' the interval of the input variable and will fill the gaps for the instances that
#' would be expected from the interval, but are missing in the input data.
#' See \code{vignette("padr")} for more information on \code{pad}.
#' See \code{vignette("padr_implementation")} for detailed information on
#' daylight savings time, different timezones, and the implementation of
#' \code{thicken}.
#' @return The data frame \code{x} with the datetime variable padded within the
#' groups. The grouping variable(s) are filled for every inserted row.
#' All other variables in the data frame will have missing values at the rows
#' that are padded.
pad_groups <- function(x,
                       ...,
                       interval = NULL,
                       start_val= NULL,
                       end_val  = NULL,
                       by       = NULL) {
  if (!is.data.frame(x)) {
    stop('x should be a data frame.')
  }

  grouping_vars <- get_the_group_vars( as.list(match.call()) )
  return(grouping_vars)
}


# little helper get the variable which we use for grouping before padding
get_the_group_vars <- function(fun_args) {
  without_fun_name <- fun_args[-1]
  as.character( fun_args[names(without_fun_name) == ""] )
}

