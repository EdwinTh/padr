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

  if(class(a)[1] == 'POSIXlt') a <- a %>% as.POSIXct
  current_b_index <- 1
  current_b_val   <- b[1]
  next_b_val      <- b[2]
  thickened       <- numeric(length(a))
  class(thickened) <- class(a)

  for(i in seq_along(a)){
    if(next_b_val > a[i]) {
      thickened[i] <- current_b_val
    } else {
      while(next_b_val <= a[i]) {
        current_b_index <- current_b_index + 1
        current_b_val   <- b[current_b_index]
        next_b_val      <- b[current_b_index + 1]
      }
      thickened[i]    <- current_b_val
    }
  }

  thickened <- lubridate::with_tz(thickened, attr(a, "tz"))
  if(class(b)[1] == 'POSIXlt') thickened <- thickened %>% as.POSIXlt
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

  if(class(a)[1] == 'POSIXlt') a <- a %>% as.POSIXct
  current_b_index <- 1
  current_b_val   <- b[1]
  thickened       <- numeric(length(a))
  class(thickened) <- class(a)

  for(i in seq_along(a)){
    if(current_b_val > a[i]) {
      thickened[i] <- current_b_val
    } else {
      while(current_b_val <= a[i]){
        current_b_index <- current_b_index + 1
        current_b_val   <- b[current_b_index]
      }
      thickened[i]    <- current_b_val
    }
  }

  thickened <- lubridate::with_tz(thickened, attr(a, "tz"))
  if(class(b)[1] == 'POSIXlt') thickened <- thickened %>% as.POSIXlt
  return(thickened[order_a])
}


