library(dplyr)

# pad and get_inteval are currently broken (fixed in a eversion)



# this function assumes that the interval is no higher than hour
# for day and higher it should be converted to date
shift_to_middle_posix <- function(x) {
  stopifnot(inherits(x, "POSIXt"))
  # interval_x <- get_interval_list(x)
  interval_x <- list(interval = "hour", step = 1)
  interval_x_secs <- int_to_secs(interval_x)
  x + interval_x_secs / 2
}

int_to_secs <- function(x) {
  secs_string <- c(hour = 3600, min = 60, sec = 1)
  secs_string[x$interval] * x$step
}


emergency %>% thicken("h", "h") %>% count(h) %>% mutate(h = shift_to_middle_posix(h)) %>%
  head(48) %>%
  ggplot(aes(h, n)) + geom_col()
