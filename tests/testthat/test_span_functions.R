x_posix <- as.POSIXct(strftime(c('2013-12-31 10:43:16',
                                 '2014-01-01 08:22:12')))
x_date  <- as.Date(x_posix)
x_num   <- as.numeric(x_posix)
x_char  <- as.character(x_posix)
x_fac   <- as.character(x_posix)

context('span_ functions throw error on wrong data types')

# NOTE expect_error(object, NA) = expect no error
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

context('span_ functions throw error on wrong interval start and end')
x          <- x_posix
start      <- as.POSIXct(strftime('2014-01-01 00:00:00'))
end_year   <- as.POSIXct(strftime('2015-01-01 00:00:00'))
end_month  <- as.POSIXct(strftime('2015-02-01 00:00:00'))
end_day    <- as.POSIXct(strftime('2015-02-03 00:00:00'))
end_hour   <- as.POSIXct(strftime('2015-02-03 04:00:00'))
end_minute <- as.POSIXct(strftime('2015-02-03 04:05:00'))

test_that('span_year throws errors at wrong start and end', {
  expect_error(span_year(x, start, end_year), NA)
  expect_error(span_year(x, start, end_month))
  expect_error(span_year(x, start, end_day))
  expect_error(span_year(x, start, end_hour))
  expect_error(span_year(x, start, end_minute))
})

test_that('span_month throws errors at wrong start and end', {
  expect_error(span_month(x, start, end_year), NA)
  expect_error(span_month(x, start, end_month), NA)
  expect_error(span_month(x, start, end_day))
  expect_error(span_month(x, start, end_hour))
  expect_error(span_month(x, start, end_minute))
})

test_that('span_day throws errors at wrong start and end', {
  expect_error(span_day(x, start, end_year), NA)
  expect_error(span_day(x, start, end_month), NA)
  expect_error(span_day(x, start, end_day), NA)
  expect_error(span_day(x, start, end_hour))
  expect_error(span_day(x, start, end_minute))
})

test_that('span_hour throws errors at wrong start and end', {
  expect_error(span_hour(x, start, end_year), NA)
  expect_error(span_hour(x, start, end_month), NA)
  expect_error(span_hour(x, start, end_day), NA)
  expect_error(span_hour(x, start, end_hour), NA)
  expect_error(span_hour(x, start, end_minute))
})

test_that('span_minute throws errors at wrong start and end', {
  expect_error(span_minute(x, start, end_year), NA)
  expect_error(span_minute(x, start, end_month), NA)
  expect_error(span_minute(x, start, end_day), NA)
  expect_error(span_minute(x, start, end_hour), NA)
  expect_error(span_minute(x, start, end_minute), NA)
})


context('Test the span_ functions on output')

test_that('span_year produces correct output', {
  out_year <- span_year(x_posix)
  out_year_with_start <- span_year(x_posix, start = as.POSIXct('2012-12-05 16:56:14'))
  out_year_with_end <- span_year(x_posix, end = as.POSIXct('2015-03-02 09:12:11'))
  expect_equal(length(out_year), 3)
  expect_equal(min(out_year), as.POSIXct('2013-01-01'))
  expect_equal(max(out_year), as.POSIXct('2015-01-01'))
  expect_equal(max(out_year_with_start), as.POSIXct("2014-12-05 16:56:14"))
  expect_equal(min(out_year_with_end), as.POSIXct("2013-03-02 09:12:11"))
})

test_that('span_month produces correct output', {
  out_month <- span_month(x_posix)
  out_month_with_start <- span_month(x_posix, start = as.POSIXct('2013-11-17 16:56:14'))
  out_month_with_end   <- span_month(x_posix, end = as.POSIXct('2014-02-04 03:11:05'))
  expect_equal(out_month %>% length, 3)
  expect_equal(out_month %>% min, '2013-12-01' %>% strftime %>% as.POSIXct)
  expect_equal(out_month %>% max, '2014-02-01' %>% strftime %>% as.POSIXct)
  expect_equal(max(out_month_with_start), as.POSIXct("2014-01-17 16:56:14"))
  expect_equal(min(out_month_with_end), as.POSIXct("2013-12-04 03:11:05"))
})

test_that('span_day produces correct output', {
  out_day <- span_day(x_posix)
  out_day_with_start <- span_day(x_posix, start = as.POSIXct('2013-12-30 16:56:14'))
  out_day_with_end   <- span_day(x_posix, end = as.POSIXct('2014-01-03 03:11:05'))
  expect_equal(out_day %>% length, 3)
  expect_equal(out_day %>% min, '2013-12-31' %>% strftime %>% as.POSIXct)
  expect_equal(out_day %>% max, '2014-01-02' %>% strftime %>% as.POSIXct)
  expect_equal(max(out_day_with_start), as.POSIXct("2014-01-01 16:56:14"))
  expect_equal(min(out_day_with_end), as.POSIXct("2014-01-01 03:11:05"))
})

test_that('span_hour produces correct output', {
  out_hour <- span_hour(x_posix)
  out_hour_with_start <- span_hour(x_posix, start = as.POSIXct('2013-12-31 08:45:13'))
  out_hour_with_end   <- span_hour(x_posix, end = as.POSIXct('2014-01-01 11:45:13'))
  expect_equal(out_hour %>% length, 24)
  expect_equal(out_hour %>% min, '2013-12-31 10:00:00' %>% strftime %>% as.POSIXct)
  expect_equal(out_hour %>% max, '2014-01-01 09:00:00' %>% strftime %>% as.POSIXct)
  expect_equal(max(out_hour_with_start), as.POSIXct("2014-01-01 07:45:13"))
  expect_equal(min(out_hour_with_end), as.POSIXct("2013-12-31 12:45:13"))
})

test_that('span_minute produces correct output', {
  out_min <- span_minute(x_posix)
  out_min_with_start <- span_minute(x_posix, start = as.POSIXct('2013-12-31 10:41:12'))
  out_min_with_end <- span_minute(x_posix, end = as.POSIXct('2014-01-01 08:24:15'))
  expect_equal(out_min %>% length, 1301)
  expect_equal(out_min %>% min, '2013-12-31 10:43:00' %>% strftime %>% as.POSIXct)
  expect_equal(out_min %>% max, '2014-01-01 08:23:00' %>% strftime %>% as.POSIXct)
  expect_equal(max(out_min_with_start), as.POSIXct("2014-01-01 08:21:12"))
  expect_equal(min(out_min_with_end), as.POSIXct("2013-12-31 10:44:15"))
})


