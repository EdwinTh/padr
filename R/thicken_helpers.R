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

round_down <- function(a,
                       b) {

  if( !all(class(a) == class(b)) ){
    stop('a and b should be of the same class')
  }

  order_a <- order(a)
  a <- sort(a)
  b <- sort(b)

  if(class(a)[1] == 'Date'){
    thickened <- as.Date(round_down_core(a,b), origin = '1970-01-01')
  } else if (class(a)[1] == 'POSIXct') {
    thickened <- as.POSIXct(round_down_core(as.numeric(a), as.numeric(b)),
                            origin = '1970-01-01', tz = attr(a, 'tz'))
  } else {
    thickened <- as.POSIXlt(round_down_core(as.numeric(a), as.numeric(b)),
                            origin = '1970-01-01', tz = attr(a, 'tz'))
  }

  return(thickened[order_a])
}


#' @rdname span_year
round_up <- function(a,
                     b) {
  if( !all(class(a) == class(b)) ){
    stop('a and b should be of the same class')
  }

  order_a <- order(a)
  a <- sort(a)
  b <- sort(b)

  if(class(a)[1] == 'Date'){
    thickened <- as.Date(round_up_core(a,b), origin = '1970-01-01')
  } else if (class(a)[1] == 'POSIXct') {
    thickened <- as.POSIXct(round_up_core(as.numeric(a), as.numeric(b)),
                            origin = '1970-01-01', tz = attr(a, 'tz'))
  } else {
    thickened <- as.POSIXlt(round_up_core(as.numeric(a), as.numeric(b)),
                            origin = '1970-01-01', tz = attr(a, 'tz'))
  }

  return(thickened[order_a])
}


