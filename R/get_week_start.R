#' Retrieve the closest given Weekday
#'
#' When applying \code{thicken} with the interval "week", use this function to
#' automatically start at the first weekday before \code{min(datetime_var)} at
#' `start_val` argument.
#' @param x A vector of class \code{Date}, \code{POSIXct}, or \code{POSIXlt}.
#' @param wday Integer in the range 0-6 specifying the desired weekday start
#' (0 = Sun, 1 = Mon, 2 = Tue, 3 = Wed, 4 = Thu, 5 = Fri, 6 = Sat).
#' @param direction Look \code{down} or \code{up}.
#' @return The closest desired weekday to \code{x}.
#' @examples
#' closest_weekday(coffee$time_stamp)
#' closest_weekday(coffee$time_stamp, 4)
#' closest_weekday(coffee$time_stamp, 4, direction = "up")
#' @export
closest_weekday <- function(x,
                            wday = 1,
                            direction = c("down", "up")) {
  stopifnot(wday %in% 0:6)
  stopifnot(length(wday) == 1)
  stopifnot(is_datetime(x))
  direction <- match.arg(direction)

  dt_var_start <- min(as.Date(dt_var))
  wday_lookup  <- make_weekdays_lookup()
  current <- wday_lookup[weekdays(dt_var_start)]

  if (direction == "down") {
    shift <- current - wday
    if (shift < 0) shift <- shift + 7
    ret_value <- dt_var_start - as.numeric(shift)
  } else {
    shift <- wday - current
    if (shift < 0) shift <- shift + 7
    ret_value <- dt_var_start + as.numeric(shift)
  }
  ret_value
}

make_weekdays_lookup <- function() {
  lookup <- 0:6
  names(lookup) <- weekdays( seq(as.Date("2017-05-21"), length.out = 7,
                                 by = "day"))
  lookup
}
