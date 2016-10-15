
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

x_year  <- seq(as.Date('2015-01-01'), as.Date('2016-01-01'), by = 'year')
x_month <- seq(as.Date('2015-01-01'), as.Date('2015-06-01'), by = 'month')
x_day   <- seq(as.Date('2015-01-01'), as.Date('2015-02-01'), by = 'day')
x_hour  <- seq(as.POSIXct('2015-01-01'), as.POSIXct('2015-01-02'), by = 'hour')
x_min   <- seq(lubridate::ymd_hm('2015-01-01 00:00'),
               lubridate::ymd_hm('2015-01-01 00:59'), by = 'min')

df_with_one_date  <- data.frame(dt_var1 = date_seq('month'),
                                y = 1:6)
df_with_two_dates <- data.frame(dt_var1  = date_seq('month'),
                                dt_var2 = date_seq('month'),
                                y = 1:6)

context("Test the smear function")

test_that("Section 1, correct error handling", {
  expect_error(smear(x_month %>% as.character))
  expect_error(smear(x_month %>% as.numeric))
  expect_error(smear(mtcars))
  expect_error(suppressWarnings(smear(df_with_one_date, interval = 'day')), NA)
  expect_error(smear(df_with_two_dates, interval = 'day'))
  expect_error(suppressWarnings(smear(df_with_two_dates, by = dt_var1, interval = 'day')), NA)
})

test_that("Section 2, correct error handling", {
  expect_error(smear(x_month, 'month'))
  expect_error(smear(x_hour, 'month'))
  expect_error(smear(x_month, 'day'), NA)
  expect_warning(smear(date_seq('month'), 'day'))
})

test_that("Smear gives the correct output", {
  expect_equal(smear(x_year, 'month') %>% nrow, 24)
  expect_equal(smear(x_year, 'day')   %>% nrow, 731)
  expect_equal(smear(x_month, 'day')  %>% nrow,  181)
  # we lose one hour when moving to DST
  expect_equal(smear(x_month, 'hour')  %>% nrow,  181*24-1)
})

