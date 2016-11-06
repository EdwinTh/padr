# this file contains functions that span a full grid of start and end
# points for all of the eight intervals

library(lubridate)
library(dplyr)

create_grid <- function(start_val = ymd_hms('20140101 010101'),
                        end_val   = ymd_hms('20150101 010101')) {
  start_day <- start_val %>% as.Date; end_day <- end_val %>% as.Date

  grid_list <- list(
    year    = data_frame(datetime = seq(start_day, end_day, by = 'year')),
    year_p  = data_frame(datetime = seq(start_val, end_val, by = 'year')),
    quarter = data_frame(datetime = seq(start_day, end_day, by = 'quarter')),
    quarter_p = data_frame(datetime = seq(start_val, end_val, by = 'quarter')),
    month   = data_frame(datetime = seq(start_day, end_day, by = 'month')),
    month_p = data_frame(datetime = seq(start_val, end_val, by = 'month')),
    week    = data_frame(datetime = seq(start_day, end_day, by = 'week')),
    week_p  = data_frame(datetime = seq(start_val, end_val, by = 'week')),
    day     = data_frame(datetime = seq(start_day, end_day, by = 'day')),
    day_p  = data_frame(datetime = seq(start_val, end_val, by = 'day')),
    hour    = data_frame(datetime = seq(start_val, end_val, by = 'hour')),
    min     = data_frame(datetime = seq(start_val, end_val, by = 'min')),
    sec     = data_frame(datetime = seq(start_val, end_val, by = 'sec'))
  )
  return(grid_list)
}

# x is an element of the create_grid output
make_spaces <- function(x) {
  if(length(x) < 5) return(x)
  nr <- length(x)
  ind <- sample(1:nr, floor(nr / 2)) %>% c(1,nr) %>% unique %>% sort
  return(x[ind])
}
