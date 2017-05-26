
whole_unit_thickening <- function(dt_var,
                                  interval,
                                  start_val,
                                  rounding) {
  # step 1: make both dt_var and start_val posix
  dt_var    <- as.POSIXlt(dt_var)
  if (!is.null(start_val)) {
    start_val <- as.POSIXlt(start_val)
  }

  # step 2: round to the new interval
  if (rounding == "down") {
    thickened <- lubridate::floor_date(dt_var, interval)
  } else {
    thickened <- lubridate::ceiling_date(dt_var, interval)
  }

  # step 2: determine the offset with start_val
  offset_val <- get_offset(thickened, start_val)

  # step 4: adjust by the offset
  thickened <- thickened + offset_val

  # step 5: back to date if possible
  posix_to_date(thickened)
}

# the offset calculations are dependent on the interval, this is not as simple
# I thought it was, get off_set should be elaborated

get_offset <- function(dt_var,
                       start_val) {
  if (!is.null(start_val)) {
    offset_val <- min(dt_var) - start_val
  } else {
    offset_val <- 0
  }
  return(offset_val)
}
