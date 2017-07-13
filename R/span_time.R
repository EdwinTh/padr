




char_to_datetime <- function(x, tz = "UTC") {
  x_string <- substr(paste0(x, "0101"), 1, 15)
  x_dt <- paste(substr(x_string, 1, 4),
                substr(x_string, 5, 6),
                substr(x_string, 7, 11),
                substr(x_string, 12, 13),
                substr(x_string, 14, 15), sep = "-")
  as.POSIXct(x_dt, tz = tz)
}

interval_from_char <- function(x) {
  char_string <- c("hour", "min", "sec")
  char_string[(x-9) / 2]
}
