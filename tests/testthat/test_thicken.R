
date_seq <- function(interval){
  # only use a wide interval to test year, all others less wide for performance
  if(interval == 'year') {
    start_date <- as.POSIXlt(strftime('2005-01-01'))
  } else {
    start_date <- as.POSIXlt(strftime('2015-01-01'))
  }

  sequence <- seq(start_date,
                  as.POSIXlt(strftime('2016-01-01')),
                  by = interval)
  # as.Date function is used for interval = 'day' so we are sure to stay out of
  # timezone and daylight savings issues
  if(interval == 'day') {
    sequence <- seq(as.Date(strftime('2014-01-01')),
                    as.Date(strftime('2017-01-01')),
                    by = interval)
  }
  set.seed(12345)
  if(length(sequence) > 100) {
    sampled_dates <- sample(sequence, 100)
  } else {
    sampled_dates <- sample(sequence, length(sequence) / 2)
  }
  return(sampled_dates)
}

x_month <- date_seq('month')
x_day   <- date_seq('day')
x_hour  <- date_seq('hour')
x_min   <- date_seq('min')
x_sec   <- date_seq('sec')
equal_dist <- c(as.POSIXct('2014-01-01 23:00:00'),
                as.POSIXct('2014-01-02 01:00:00'))

df_with_one_date  <- data.frame(dt_var1 = date_seq('month'),
                                y = 1:6)
df_with_two_dates <- data.frame(dt_var1  = date_seq('month'),
                                dt_var2 = date_seq('month'),
                                y = 1:6)

context("Test the thicken function")

test_that("Section 1, correct error handling", {
  expect_error(thicken(x_month %>% as.character))
  expect_error(thicken(x_month %>% as.numeric))
  expect_error(thicken(mtcars))
  expect_error(thicken(df_with_one_date), NA)
  expect_error(thicken(df_with_two_dates))
  expect_error(thicken(df_with_two_dates, by = dt_var1), NA)
})

test_that("Section 2, correct error handling", {
  expect_error(thicken(x_month, 'month'))
  expect_error(thicken(x_hour, 'month'), NA)
})

test_that("thicken gives correct interval", {
  expect_equal(thicken(x_sec, 'year')$thickened %>% get_interval, 'year')
  expect_equal(thicken(x_sec, 'month')$thickened %>% get_interval, 'month')
  expect_equal(thicken(x_sec, 'day')$thickened %>% get_interval, 'day')
  expect_equal(thicken(x_sec, 'hour')$thickened %>% get_interval, 'hour')
  expect_equal(thicken(x_sec, 'minute')$thickened %>% get_interval, 'minute')
})

test_that("rounding works properly in thicken",{
  expect_equal(thicken(x_sec, 'year', 'closest', FALSE) %>% nrow, 2)
  expect_equal(thicken(x_sec, 'year', 'down', FALSE) %>% nrow, 1)
  expect_equal(thicken(x_sec, 'year', 'up', FALSE) %>% nrow, 1)
})

