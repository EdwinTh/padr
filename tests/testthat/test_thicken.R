
date_seq <- function(interval){
  # only use a wide interval to test year, all others less wide for performance
  if(interval == 'year') {
    start_date <- as.POSIXlt(strftime('2005-01-01'))
  } else {
    start_date <- as.POSIXlt(strftime('2015-01-01'))
  }

  sequence <- seq(start_date,
                  as.POSIXlt(strftime('2017-01-01')),
                  by = interval)

  set.seed(12345)
  if(length(sequence) > 100) {
    sampled_dates <- sample(sequence, 100)
  } else {
    sampled_dates <- sample(sequence, length(sequence) / 2)
  }
  return(sampled_dates)
}

x_month <- date_seq('month')
x_day   <- date_seq('DSTday')
x_hour  <- date_seq('hour')
x_min   <- date_seq('min')
x_sec   <- date_seq('sec')
equal_dist <- c(as.POSIXct('2014-01-01 23:00:00'),
                as.POSIXct('2014-01-02 01:00:00'))

df_with_one_date  <- data.frame(dt_var1 = date_seq('month'),
                                y = 1:6)
df_with_one_date_sorted <- df_with_one_date %>% arrange(dt_var1)
df_with_two_dates <- data.frame(dt_var1  = date_seq('month'),
                                dt_var2 = date_seq('month'),
                                y = 1:6)

context("Test the thicken function")

test_that("Section 1, correct error handling", {
  expect_error(thicken(x_month %>% as.character))
  expect_error(thicken(x_month %>% as.numeric))
  expect_error(thicken(mtcars))
  expect_error(suppressWarnings(thicken(df_with_one_date)), NA)
  expect_error(thicken(df_with_two_dates))
  expect_error(suppressWarnings(thicken(df_with_two_dates, by = dt_var1)), NA)
})

test_that("Section 2, correct error handling", {
  expect_error(thicken(x_month %>% sort, interval = 'month'))
  expect_error(suppressWarnings(thicken(x_hour, interval = 'month')), NA)
  expect_warning(thicken(x_month))
})

test_that("thicken gives correct interval", {
  expect_equal(suppressWarnings(thicken(x_sec, interval = 'year'))$x_sec_year %>% get_interval, 'year')
  expect_equal(suppressWarnings(thicken(x_sec, interval = 'month'))$x_sec_month %>% get_interval, 'month')
  expect_equal(suppressWarnings(thicken(x_sec, interval = 'day'))$x_sec_day %>% get_interval, 'day')
  expect_equal(suppressWarnings(thicken(x_sec, interval = 'hour'))$x_sec_hour %>% get_interval, 'hour')
  expect_equal(suppressWarnings(thicken(x_sec, interval = 'min'))$x_sec_min %>% get_interval, 'min')
})

test_that("thicken gives correct output when x is a vector", {
  day_sorted <- sort(x_day)
  day_to_year <- thicken(day_sorted, 'x', interval = 'year')$x
  day_to_year2 <- thicken(day_sorted, 'x', interval = 'year', rounding = 'up')$x

  expect_equal(day_to_year %>% length, 100)
  expect_equal(lubridate::year(day_to_year[1]), 2015)
  expect_equal(lubridate::year(day_to_year[100]), 2016)
  expect_equal(lubridate::year(day_to_year2[1]), 2016)
  expect_equal(lubridate::year(day_to_year2[100]), 2017)
})

test_that("thicken gives correct ouput when x is a df",{
  X <- data.frame(day_var = seq(as.Date('2016-01-01'), as.Date('2016-12-31'), by = 'day'),
                  value   = runif(366, 50, 100))

  expect_equal(thicken(X, interval = 'month') %>% nrow, 366)
  expect_equal( lubridate::month(thicken(X, interval = 'month')$day_var_month) %>% max, 12)
  expect_error( (thicken(dplyr::as_data_frame(X), interval = 'month')), NA)
  expect_error( thicken(data.table::as.data.table(X), interval = 'month') , NA)
})


test_that('column naming works properly', {
  a <- sort(x_day)
  a_df <- data.frame(a = a, b = 42)
  expect_equal(colnames(thicken(a))[2], 'a_week')
  expect_equal(colnames(thicken(a, colname = 'jos'))[2], 'jos')
  expect_equal(colnames(thicken(a_df))[3], 'a_week')
  expect_equal(colnames(thicken(a_df, colname = 'jos'))[3], 'jos')
})


