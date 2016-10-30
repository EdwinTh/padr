x_posix <- as.POSIXct(strftime(c('2013-12-31 10:43:16',
                                 '2014-01-01 08:22:12')))
x_date  <- as.Date(x_posix)
x_num   <- as.numeric(x_posix)
x_char  <- as.character(x_posix)
x_fac   <- as.character(x_posix)

context('span_ functions throw error on wrong data types')

# NOTE expect_error(object, NA) = expect no error
test_that('span throws errors at wrong types', {
  expect_error(span(x_posix), NA)
  expect_error(span(x_date), NA)
  expect_error(span(x_num))
  expect_error(span(x_char))
  expect_error(span_(x_fac))
})


context('Test the span_ functions on output')

test_that('span produces correct output for year', {
  out_year <- span(x_posix, interval = 'year')
  out_year_with_start <- span(x_posix, interval = 'year',
                              start_val = as.POSIXct('2012-12-05 16:56:14'))
  expect_equal(length(out_year), 3)
  expect_equal(min(out_year), as.Date('2013-01-01'))
  expect_equal(max(out_year), as.Date('2015-01-01'))
  expect_equal(max(out_year_with_start), as.POSIXct("2014-12-05 16:56:14"))
})

test_that('span produces correct output for month', {
  out_month <- span(x_posix, 'month')
  out_month_with_start <- span(x_posix, 'month', start_val = as.POSIXct('2013-11-17 16:56:14'))

  expect_equal(out_month %>% length, 3)
  expect_equal(out_month %>% min, as.Date('2013-12-01'))
  expect_equal(out_month %>% max, as.Date('2014-02-01'))
  expect_equal(max(out_month_with_start), as.POSIXct("2014-01-17 16:56:14"))
})

test_that('span_day produces correct output', {
  out_day <- span(x_posix, 'day')
  out_day_with_start <- span(x_posix, 'day' ,start_val = as.POSIXct('2013-12-30 16:56:14'))
  expect_equal(out_day %>% length, 3)
  expect_equal(out_day %>% min, as.Date('2013-12-31'))
  expect_equal(out_day %>% max, as.Date('2014-01-02'))
  expect_equal(max(out_day_with_start), as.POSIXct("2014-01-01 16:56:14"))
})

test_that('span_hour produces correct output', {
  out_hour <- span(x_posix, 'hour')
  out_hour_with_start <- span(x_posix, 'hour', start_val = as.POSIXct('2013-12-31 08:45:13'))
  expect_equal(out_hour %>% length, 24)
  expect_equal(out_hour %>% min, '2013-12-31 10:00:00' %>% strftime %>% as.POSIXct)
  expect_equal(out_hour %>% max, '2014-01-01 09:00:00' %>% strftime %>% as.POSIXct)
  expect_equal(max(out_hour_with_start), as.POSIXct("2014-01-01 08:45:13"))
})

test_that('span_minute produces correct output', {
  out_min <- span(x_posix, 'min')
  out_min_with_start <- span(x_posix, 'min', start_val = as.POSIXct('2013-12-31 10:41:12'))
  expect_equal(out_min %>% length, 1301)
  expect_equal(out_min %>% min, '2013-12-31 10:43:00' %>% strftime %>% as.POSIXct)
  expect_equal(out_min %>% max, '2014-01-01 08:23:00' %>% strftime %>% as.POSIXct)
  expect_equal(max(out_min_with_start), as.POSIXct("2014-01-01 08:22:12"))
})

test_that('span_second produces correct output', {
  out_sec <- span(x_posix, 'sec')
  out_sec_with_start <- span(x_posix, 'sec', start_val = as.POSIXct('2013-12-31 10:41:12'))
  expect_equal(out_sec %>% length, 77938)
  expect_equal(out_sec %>% min, '2013-12-31 10:43:16' %>% strftime %>% as.POSIXct)
  expect_equal(out_sec %>% max, '2014-01-01 08:22:13' %>% strftime %>% as.POSIXct)
  expect_equal(max(out_sec_with_start), as.POSIXct("2014-01-01 08:22:13"))
})

context('span output is of the right class')

test_that('span_year returns the same class', {
  expect_true(span(x_posix, 'year') %>% is.Date)
  expect_true(span(x_posix, 'quarter') %>% is.Date)
  expect_true(span(x_posix, 'month') %>% is.Date)
  expect_true(span(x_posix, 'week') %>% is.Date)
  expect_true(span(x_posix, 'day') %>% is.Date)
  expect_true( 'POSIXct' %in% (span(x_posix, 'hour') %>% class) )
  expect_true( 'POSIXct' %in% (span(x_posix, 'min') %>% class) )
  expect_true( 'POSIXct' %in% (span(x_posix, 'sec') %>% class) )
})


context('test get_start_and_end')
test_that('get_start_and_end output correct values', {
  expect_equal(get_start_and_end(x_posix, 'sec')$start_val %>% as.character,
               '2013-12-31 10:43:16')
  expect_equal(get_start_and_end(x_posix, 'sec')$end_val %>% as.character,
               '2014-01-01 08:22:13')
  expect_equal(get_start_and_end(x_posix, 'min')$start_val %>% as.character,
               '2013-12-31 10:43:00')
  expect_equal(get_start_and_end(x_posix, 'min')$end_val %>% as.character,
               '2014-01-01 08:23:00')
  expect_equal(get_start_and_end(x_posix, 'hour')$start_val %>% as.character,
               '2013-12-31 10:00:00')
  expect_equal(get_start_and_end(x_posix, 'hour')$end_val %>% as.character,
               '2014-01-01 09:00:00')
  expect_equal(get_start_and_end(x_posix, 'day')$start_val %>% as.character,
               '2013-12-31')
  expect_equal(get_start_and_end(x_posix, 'day')$end_val %>% as.character,
               '2014-01-02')
  expect_equal(get_start_and_end(x_posix, 'week')$start_val %>% as.character,
               '2013-12-29')
  expect_equal(get_start_and_end(x_posix, 'week')$end_val %>% as.character,
               '2014-01-05')
  expect_equal(get_start_and_end(x_posix, 'month')$start_val %>% as.character,
               '2013-12-01')
  expect_equal(get_start_and_end(x_posix, 'month')$end_val %>% as.character,
               '2014-02-01')
  expect_equal(get_start_and_end(x_posix, 'quarter')$start_val %>% as.character,
               '2013-10-01')
  expect_equal(get_start_and_end(x_posix, 'quarter')$end_val %>% as.character,
               '2014-04-01')
  expect_equal(get_start_and_end(x_posix, 'year')$start_val %>% as.character,
               '2013-01-01')
  expect_equal(get_start_and_end(x_posix, 'year')$end_val %>% as.character,
               '2015-01-01')
})

