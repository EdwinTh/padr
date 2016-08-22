context('Test the get_interval function')

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

  if(length(sequence) > 1000) {
    sampled_dates <- sample(sequence, 1000)
  } else {
    sampled_dates <- sample(sequence, length(sequence) / 2)
  }
  return(sampled_dates)
}

test_errors_data <- date_seq('day')

test_that('test_errors_data only works on right data types',{
  expect_error(as.integer(test_errors_data) %>% get_interval)
  expect_error(as.numeric(test_errors_data) %>% get_interval)
  expect_error(as.character(test_errors_data) %>% get_interval)
  expect_error(as.factor(test_errors_data) %>% get_interval)
  expect_error(test_errors_data %>% get_interval, NA)
  expect_error(as.POSIXct(test_errors_data) %>% get_interval, NA)
  expect_error(as.POSIXlt(test_errors_data) %>% get_interval, NA)
})



test_that('test_errors_data gives the correct output',{
  expect_equal(date_seq('year') %>% get_interval, 'year')
  expect_equal(date_seq('month') %>% get_interval, 'month')
  expect_equal(date_seq('day') %>% get_interval, 'day')
  expect_equal(date_seq('hour') %>% get_interval, 'hour')
  expect_equal(date_seq('min') %>% get_interval, 'minute')
  expect_equal(date_seq('sec') %>% get_interval, 'second')
})


