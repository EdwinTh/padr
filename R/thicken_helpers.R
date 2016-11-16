#' Find closest value of a in b
#'
#' For two vectors of class \code{Date}, \code{POSIXct}, or \code{POSIXlt} find
#' the closest value of \code{a} in \code{b} under the restriction that the
#' returned value is either smaller than the value in \code{a}, when using
#' \code{round_down}, or larger than the value in \code{b}.
#' @param a A vector of class \code{Date}, \code{POSIXct}, or \code{POSIXlt}.
#' @param b A vector of the same class as \code{a}.
#' @return A vector of the same class and the same length as \code{a}, with
#' for each value of \code{a} the closest value in \code{b}, either rounded up
#' or rounded down.
#' @examples
#' x <- seq(as.Date('2016-01-01'), as.Date('2016-12-31'), by = 'day')
#' x <- sample(x, 200)
#' month_span <- span_month(x)
#' round_down(x, month_span)
#' round_up(x, month_span)
#'
#' x2 <- seq(as.POSIXct('2016-01-01'), as.POSIXct('2016-01-02'), by = 'min')
#' x2 <- sample(x2, 300)
#' spanx2 <- span_hour(x2)
#' round_down(x2, spanx2)
#' round_up(x2, spanx2)

round_thicken <- function(a,
                          b,
                          direction = c('down', 'up')) {

  direction <- match.arg(direction)

  order_a <- order(a)
  a <- sort(a)
  b <- sort(b)

  a_same_level <- to_posix(a, b)$a
  b_same_level <- to_posix(a, b)$b

  rounded <- apply_rounding(a_same_level, b_same_level, direction)

  if('Date' %in% class(a_same_level)){
    thickened <- as.Date(rounded, origin = '1970-01-01')
  } else {
    thickened <- as.POSIXct(rounded, origin = '1970-01-01',
                            tz = attr(a_same_level, 'tz'))
  }
  thickened <- posix_to_date(thickened)

  return(thickened[order_a])
}

# If either of the two variables is of class posix, the other should be posix
# as well
to_posix <- function(a, b) {
  if( 'POSIXt' %in% class(a) & 'Date' %in% class(b) ) {
    b <- as.POSIXct(strftime(b), tz = attr(a, 'tzone'))
  } else if ( 'Date' %in% class(a) & 'POSIXt' %in% class(b) ) {
    a <- as.POSIXct(as.character(a), tz = attr(b, 'tz'))
  }
  return(list(a = a, b = b))
}

# apply the correct rounding function, based on the direction
apply_rounding <- function(a, b,  direction = c('up', 'down')) {
  if(direction == 'up') {
    round_up_core(a, b)
  } else {
    round_down_core(a, b)
  }
}

# If the thickened variabel is of class POSIXt this function checks if it as
# well can be of class Date
posix_to_date <- function(x) {
  if('POSIXt' %in% class(x)) {
  check_var <- as.POSIXlt(x)
  to_date <- all( c(check_var$hour, check_var$min, check_var$sec ) == 0 )
  if(to_date) x <- as.Date(strptime(x, format = '%Y-%m-%d'))
  }
  return(x)
}
