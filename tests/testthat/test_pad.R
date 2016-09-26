
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

x_year  <- c(as.Date('2015-01-01'), as.Date('2018-01-01'))
x_month <- seq(as.Date('2015-01-01'), as.Date('2015-06-01'), by ='month')[c(1,3,4,6)]
x_day   <- seq(as.Date('2015-01-01'), as.Date('2015-02-01'), by = 'day') %>% sample(15) %>%
  c(as.Date('2015-01-01'), as.Date('2015-02-01')) %>% unique
x_hour  <- seq(as.POSIXct('2015-01-01 01:00:00'), as.POSIXct('2015-01-02 01:00:00'), by = 'hour')[c(1,25)]
x_min   <- seq(lubridate::ymd_hm('2015-01-01 00:00'),
               lubridate::ymd_hm('2015-01-01 00:59'), by = 'min') %>% sample(15)

df_with_one_date  <- data.frame(dt_var1 = date_seq('month'),
                                y = 1:6)
df_with_two_dates <- data.frame(dt_var1  = date_seq('month'),
                                dt_var2 = date_seq('month'),
                                y = 1:6)

context("Test the pad function")

test_that("Correct error handling", {
  expect_error(pad(x_month %>% as.character))
  expect_error(pad(x_month %>% as.numeric))
  expect_error(pad(mtcars))
  expect_error(pad(df_with_one_date), NA)
  expect_error(pad(df_with_two_dates))
  expect_error(pad(df_with_two_dates, by = dt_var1), NA)
})

test_that("Gives warning when unordered", {
  expect_warning(pad(x_day))
})

test_that("Pad gives correct results on vectors", {
  expect_equal(pad(x_year) %>% length, 4)
  expect_equal(pad(x_month) %>% length, 6)
  expect_equal(pad(x_day) %>% length, 32)
  expect_equal(pad(x_hour) %>% length, 25)
})
