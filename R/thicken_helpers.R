
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
  if(to_date) x <- as.Date(x)
  }
  return(x)
}

