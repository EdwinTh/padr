
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
               lubridate::ymd_hm('2015-01-01 00:59'), by = 'min') %>% sample(15) %>%
  c(lubridate::ymd_hm('2015-01-01 00:00'), lubridate::ymd_hm('2015-01-01 00:59')) %>% unique

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
  expect_error(suppressWarnings(pad(df_with_one_date)), NA)
  expect_error(pad(df_with_two_dates))
  expect_error(suppressWarnings(pad(df_with_two_dates, by = dt_var1)), NA)
})

test_that("Gives warning when unordered", {
  expect_warning(pad(x_day))
})

test_that("Pad gives correct results on vectors", {
  expect_equal(pad(x_year) %>% length, 4)
  expect_equal(pad(x_year, end_val = as.Date('2021-01-01')) %>% length, 7)
  expect_equal(pad(x_year, start_val = as.Date('2012-01-01')) %>% length, 7)
  expect_equal(pad(x_year, interval = 'month') %>% length, 37)
  expect_equal(pad(x_month) %>% length, 6)
  expect_equal(suppressWarnings(pad(x_day)) %>% length, 32)
  expect_equal(pad(x_hour) %>% length, 2)
  expect_equal(pad(x_hour, interval = 'hour') %>% length, 25)
  expect_equal(suppressWarnings(pad(x_min)) %>% length, 60)
})

test_that("Pad gives correct results on data.frames", {
  expect_equal(pad(data.frame(x_year, 1)) %>% nrow, 4)
  expect_equal(pad(data.frame(x_year, 1), end_val = as.Date('2021-01-01')) %>% nrow, 7)
  expect_equal(pad(data.frame(x_year, 1), start_val = as.Date('2012-01-01')) %>% nrow, 7)
  expect_equal(pad(data.frame(x_year, 1), interval = 'month') %>% nrow, 37)
  expect_equal(pad(data.frame(x_month, 1)) %>% nrow, 6)
  expect_equal(suppressWarnings(pad(data.frame(x_day, 1))) %>% nrow, 32)
  expect_equal(pad(data.frame(x_hour, 1)) %>% nrow, 2)
  expect_equal(pad(data.frame(x_hour, 1), interval = 'hour') %>% nrow, 25)
  expect_equal(suppressWarnings(pad(data.frame(x_min, 1))) %>% nrow, 60)
})

test_that('Pad works properly on data.table and tbl', {
  expect_equal(class(pad(data.table::data.table(x_year, 1)))[1], 'data.table')
  expect_equal(class(pad(dplyr::data_frame(x_year, 1)))[1], 'tbl_df')
})


