pkgname <- "padr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('padr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("fill_by_function")
### * fill_by_function

flush(stderr()); flush(stdout())

### Name: fill_by_function
### Title: Fill missing values by a function of the nonmissings.
### Aliases: fill_by_function

### ** Examples

library(dplyr) # for the pipe operator
x <- seq(as.Date('2016-01-01'), by = 'day', length.out = 366)
x <- x[sample(1:366, 200)] %>% sort
x_df <- data_frame(x  = x,
                   y1 = runif(200, 10, 20) %>% round,
                   y2 = runif(200, 1, 50) %>% round)
x_df %>% pad %>% fill_by_function(y1, y2)
x_df %>% pad %>% fill_by_function(y1, y2, fun = median)



cleanEx()
nameEx("fill_by_prevalent")
### * fill_by_prevalent

flush(stderr()); flush(stdout())

### Name: fill_by_prevalent
### Title: Fill missing values by the most prevalent nonnmissing value.
### Aliases: fill_by_prevalent

### ** Examples

library(dplyr) # for the pipe operator
x <- seq(as.Date('2016-01-01'), by = 'day', length.out = 366)
x <- x[sample(1:366, 200)] %>% sort
x_df <- data_frame(x  = x,
                  y1 = rep(letters[1:3], c(80, 70, 50)) %>% sample,
                  y2 = rep(letters[2:5], c(60, 80, 40, 20)) %>% sample)
x_df %>% pad %>% fill_by_prevalent(y1, y2)



cleanEx()
nameEx("fill_by_value")
### * fill_by_value

flush(stderr()); flush(stdout())

### Name: fill_by_value
### Title: Fill missing values by a single value.
### Aliases: fill_by_value

### ** Examples

library(dplyr) # for the pipe operator
x <- seq(as.Date('2016-01-01'), by = 'day', length.out = 366)
x <- x[sample(1:366, 200)] %>% sort
x_df <- data_frame(x  = x,
                   y1 = runif(200, 10, 20) %>% round,
                   y2 = runif(200, 1, 50) %>% round,
                   y3 = runif(200, 20, 40) %>% round,
                   y4 = sample(letters[1:5], 200, replace = TRUE))
x_padded <- x_df %>% pad
x_padded %>% fill_by_value(y1)
x_df %>% pad %>% fill_by_value(y1, y2, value = 42)



cleanEx()
nameEx("get_interval")
### * get_interval

flush(stderr()); flush(stdout())

### Name: get_interval
### Title: Get the interval of a datetime variable.
### Aliases: get_interval

### ** Examples

x_month <- seq(as.Date('2016-01-01'), as.Date('2016-05-01'), by = 'month')
get_interval(x_month)

x_sec <- seq(as.POSIXct('2016-01-01 00:00:00'), length.out = 100, by = 'sec')
get_interval(x_sec)



cleanEx()
nameEx("pad")
### * pad

flush(stderr()); flush(stdout())

### Name: pad
### Title: Pad the datetime column of a data frame.
### Aliases: pad

### ** Examples

simple_df <- data.frame(day = as.Date(c('2016-04-01', '2016-04-03')),
                        some_value = c(3,4))
pad(simple_df)

library(dplyr) # for the pipe operator
month <- seq(as.Date('2016-04-01'), as.Date('2017-04-01'),
              by = 'month')[c(1, 4, 5, 7, 9, 10, 13)]
month_df <- data.frame(month = month,
                       y = runif(length(month), 10, 20) %>% round)
# forward fill the padded values with tidyr's fill
month_df %>% pad %>% tidyr::fill(y)

# or fill all y with 0
month_df %>% pad %>% fill_by_value(y)

# padding a data.frame on group level
day_var <- seq(as.Date('2016-01-01'), length.out = 12, by = 'month')
x_df_grp <- data.frame(grp  = rep(LETTERS[1:3], each =4),
                       y    = runif(12, 10, 20) %>% round(0),
                       date = sample(day_var, 12, TRUE)) %>%
 arrange(grp, date)

x_df_grp %>% group_by(grp) %>% do(pad(.)) %>% ungroup %>%
tidyr::fill(grp)



cleanEx()
nameEx("thicken")
### * thicken

flush(stderr()); flush(stdout())

### Name: thicken
### Title: Add a variable of a higher interval to a data frame.
### Aliases: thicken

### ** Examples

x_hour <- seq(lubridate::ymd_hms('20160302 000000'), by = 'hour',
              length.out = 200)
some_df <- data.frame(x_hour = x_hour)
thicken(some_df)
thicken(some_df, 'month')
thicken(some_df, start_val = lubridate::ymd_hms('20160301 120000'))

library(dplyr)
x_df <- data.frame(
  x = seq(lubridate::ymd(20130101), by = 'day', length.out = 1000) %>%
    sample(500),
  y = runif(500, 10, 50) %>% round) %>%
  arrange(x)

# get the max per month
x_df %>% thicken('month') %>% group_by(x_month) %>%
  summarise(y_max = max(y))

# get the average per week, but you want your week to start on Mondays
# instead of Sundays
min_x <- x_df$x %>% min
weekdays(min_x)
x_df %>% thicken(start_val = min_x - 1) %>%
  group_by(x_week) %>% summarise(y_avg = mean(y))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
