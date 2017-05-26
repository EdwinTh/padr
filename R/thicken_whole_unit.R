
whole_unit_thickening <- function(dt_var,
                                  interval_converted,
                                  start_val,
                                  rounding) {
  # step 1: make both dt_var and start_val posix
  dt_var    <- as.POSIXlt(dt_var)
  if (!is.null(start_val)) {
    start_val <- as.POSIXlt(start_val)
  }

  # step 2: determine the offset with start_val
  offset_val <- get_offset(dt_var, start_val)

  # step 3: round to the new interval
  if (rounding == "down") {
    thickened <- lubridate::floor_date(dt_var, interval_converted$interval)
  } else {
    thickened <- lubridate::ceiling_date(dt_var, interval_converted$interval)
  }

  # step 4: adjust by the offset
  thickened <- thickened + offset_val

  # step 5: back to date if possible
  posix_to_date(thickened)
}

get_offset <- function(dt_var,
                       start_val) {
  if (!is.null(start_val)) {
    offset_val <- dt_var - start_val
  } else {
    offset_val <- 0
  }
  return(offset_val)
}
