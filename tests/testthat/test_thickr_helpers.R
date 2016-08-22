context('Test the span_ functions on errors')

x_posix <- as.POSIXct(strftime(c('2013-12-31 10:43:16',
                                 '2014-01-01 08:22:12')))
x_date  <- as.Date(x_posix)
x_num   <- as.numeric(x_posix)
x_char  <- as.character(x_posix)
x_fac   <- as.character(x_posix)

test_that('span_year throws errors at wrong types', {
  expect_error(span_year(x_posix), NA)
  expect_error(span_year(x_date), NA)
  expect_error(span_year(x_num))
  expect_error(span_year(x_char))
  expect_error(span_year(x_fac))
})

test_that('span_month throws errors at wrong types', {
  expect_error(span_month(x_posix), NA)
  expect_error(span_month(x_date), NA)
  expect_error(span_month(x_num))
  expect_error(span_month(x_char))
  expect_error(span_month(x_fac))
})

test_that('span_day throws errors at wrong types', {
  expect_error(span_day(x_posix), NA)
  expect_error(span_day(x_date))
  expect_error(span_day(x_num))
  expect_error(span_day(x_char))
  expect_error(span_day(x_fac))
})

test_that('span_hour throws errors at wrong types', {
  expect_error(span_hour(x_posix), NA)
  expect_error(span_hour(x_date))
  expect_error(span_hour(x_num))
  expect_error(span_hour(x_char))
  expect_error(span_hour(x_fac))
})

test_that('span_minute throws errors at wrong types', {
  expect_error(span_minute(x_posix), NA)
  expect_error(span_minute(x_date))
  expect_error(span_minute(x_num))
  expect_error(span_minute(x_char))
  expect_error(span_minute(x_fac))
})


context('Test the span_ functions on output')

test_that('span_year produces correct output', {
  out_year <- span_year(x_posix)
  expect_equal(out_year %>% length, 3)
  expect_equal(out_year %>% min, '2013-01-01' %>% strftime %>% as.POSIXct)
  expect_equal(out_year %>% max, '2015-01-01' %>% strftime %>% as.POSIXct)
})

test_that('span_month produces correct output', {
  out_month <- span_month(x_posix)
  expect_equal(out_month %>% length, 3)
  expect_equal(out_month %>% min, '2013-12-01' %>% strftime %>% as.POSIXct)
  expect_equal(out_month %>% max, '2014-02-01' %>% strftime %>% as.POSIXct)
})

test_that('span_day produces correct output', {
  out_day <- span_day(x_posix)
  expect_equal(out_day %>% length, 3)
  expect_equal(out_day %>% min, '2013-12-31' %>% strftime %>% as.POSIXct)
  expect_equal(out_day %>% max, '2014-01-02' %>% strftime %>% as.POSIXct)
})

test_that('span_hour produces correct output', {
  out_hour <- span_hour(x_posix)
  expect_equal(out_hour %>% length, 24)
  expect_equal(out_hour %>% min, '2013-12-31 10:00:00' %>% strftime %>% as.POSIXct)
  expect_equal(out_hour %>% max, '2014-01-01 09:00:00' %>% strftime %>% as.POSIXct)
})

test_that('span_minute produces correct output', {
  out_min <- span_minute(x_posix)
  expect_equal(out_min %>% length, 1301)
  expect_equal(out_min %>% min, '2013-12-31 10:43:00' %>% strftime %>% as.POSIXct)
  expect_equal(out_min %>% max, '2014-01-01 08:23:00' %>% strftime %>% as.POSIXct)
})


