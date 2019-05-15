round_thicken <- function(a,
                          b,
                          direction = c('down', 'up'),
                          ties_to_earlier) {

  direction <- match.arg(direction)

  a_same_level <- to_posix(a, b)$a
  b_same_level <- to_posix(a, b)$b

  a_df <- data.frame(a_same_level = a_same_level,
                     sorting_var = seq_along(a))
  a_df <- dplyr::arrange(a_df, a)
  b_same_level <- sort(b_same_level)

  a_df$rounded <- apply_rounding(a_df$a_same_level,
                                 b_same_level,
                                 direction,
                                 ties_to_earlier)
  sorting_var  <- NULL # appeases CRAN check note
  a_df <- dplyr::arrange(a_df, sorting_var)
  rounded <- a_df$rounded

  if (inherits(a_same_level, 'Date')){
    thickened <- as.Date(rounded, origin = '1970-01-01')
  } else {
    thickened <- as.POSIXct(rounded, origin = '1970-01-01',
                            tz = attr(a_same_level, 'tz'))
  }
  thickened <- posix_to_date(thickened)
  return(thickened)
}

# If either of the two variables is of class posix, the other should be posix
# as well. Apparantly it is possible to have tzone that is NULL, therefore
# we need the extra, not too, elegant code.
to_posix <- function(a, b) {

  if ( inherits(a, 'POSIXt') &  inherits(b, 'Date') ) {

    if (is.null(attr(a, 'tzone'))) {
      b <- as.POSIXct(strftime(b))
      attr(b, "tzone") <- NULL
    } else {
      b <- as.POSIXct(strftime(b), tz = attr(a, 'tzone'))
    }

  } else if ( inherits(a, 'Date') & inherits(b, 'POSIXt') ) {

    if (is.null(attr(b, 'tzone'))) {
      a <- as.POSIXct(strftime(a))
      attr(a, "tzone") <- NULL
    } else {
      a <- as.POSIXct(as.character(a), tz = attr(b, 'tz'))
    }
  }

  return(list(a = a, b = b))
}

# apply the correct rounding function, based on the direction
apply_rounding <- function(a, b,  direction = c('up', 'down'), ties_to_earlier = FALSE) {
  if (direction == 'up') {
    if (ties_to_earlier) {
      round_up_core_prev(a, b)
    } else {
      round_up_core(a, b)
    }
  } else {
    if (ties_to_earlier) {
      round_down_core_prev(a, b)
    } else {
      round_down_core(a, b)
    }
  }
}

# If the thickened variable is of class POSIXt this function checks if it as
# well can be of class Date
posix_to_date <- function(x) {
  if ( inherits(x, 'POSIXt')) {
    check_var <- as.POSIXlt(x)
    to_date <- all( c(check_var$hour, check_var$min, check_var$sec ) == 0 )
    if (to_date) {
      x <- as.Date(x, tz = lubridate::tz(x))
    }
  }
  return(x)
}
