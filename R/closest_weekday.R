#' Retrieve the closest given weekday
#'
#' Find the closest instance of the requested weekday to \code{min(x)}.
#' Helper function for \code{thicken} with the interval "week", when the user
#' desires the start day of the weeks to be different from Sundays.
#' @param x A vector of class \code{Date}, \code{POSIXct}, or \code{POSIXlt}.
#' @param wday Integer in the range 0-6 specifying the desired weekday start
#' (0 = Sun, 1 = Mon, 2 = Tue, 3 = Wed, 4 = Thu, 5 = Fri, 6 = Sat).
#' @param direction The first desired weekday before ("down") or after ("up")
#' the first day in \code{x}.
#' @return Object of class \code{Date}, reflecting the closest desired weekday
#' to \code{x}.
#' @examples
#' closest_weekday(coffee$time_stamp)
#' closest_weekday(coffee$time_stamp, 5)
#' closest_weekday(coffee$time_stamp, 1, direction = "up")
#' closest_weekday(coffee$time_stamp, 5, direction = "up")
#' @export
closest_weekday <- function(x,
                            wday = 1,
                            direction = c("down", "up")) {
  stopifnot(wday %in% 0:6)
  stopifnot(length(wday) == 1)
  stopifnot(is_datetime(x))
  direction <- match.arg(direction)

  x_start <- min(as.Date(x))
  wday_lookup  <- make_weekdays_lookup()
  current <- wday_lookup[weekdays(x_start)]

  if (direction == "down") {
    shift <- current - wday
    if (shift < 0) shift <- 7 + shift
    ret_value <- x_start - shift
  } else {
    shift <- wday - current
    if (shift < 0) shift <- 7 + shift
    ret_value <- x_start + as.numeric(shift)
  }
  names(ret_value) <- NULL
  ret_value
}

make_weekdays_lookup <- function() {
  lookup <- 0:6
  names(lookup) <- weekdays( seq(as.Date("2017-05-21"), length.out = 7,
                                 by = "day"))
  lookup
}
