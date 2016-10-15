date_seq <- function(pulse){
  # only use a wide pulse to test year, all others less wide for performance
  if(pulse == 'year') {
    start_date <- as.POSIXlt(strftime('2005-01-01'))
  } else {
    start_date <- as.POSIXlt(strftime('2015-01-01'))
  }

  sequence <- seq(start_date,
                  as.POSIXlt(strftime('2016-01-01')),
                  by = pulse)
  # as.Date function is used for pulse = 'day' so we are sure to stay out of
  # timezone and daylight savings issues
  if(pulse == 'day') {
    sequence <- seq(as.Date(strftime('2014-01-01')),
                    as.Date(strftime('2017-01-01')),
                    by = pulse)
  }

  if(length(sequence) > 1000) {
    sampled_dates <- sample(sequence, 1000)
  } else {
    sampled_dates <- sample(sequence, length(sequence) / 2)
  }
  return(sampled_dates)
}

test_errors_data <- date_seq('day')

context('Test the get_pulse function')

test_that('get_pulse only works on right data types',{
  expect_error(as.integer(test_errors_data) %>% get_pulse)
  expect_error(as.numeric(test_errors_data) %>% get_pulse)
  expect_error(as.character(test_errors_data) %>% get_pulse)
  expect_error(as.factor(test_errors_data) %>% get_pulse)
  expect_error(test_errors_data %>% get_pulse, NA)
  expect_error(as.POSIXct(test_errors_data) %>% get_pulse, NA)
  expect_error(as.POSIXlt(test_errors_data) %>% get_pulse, NA)
})

test_that('get_pulse gives the correct output',{
  expect_equal(date_seq('year') %>% get_pulse, 'year')
  expect_equal(date_seq('month') %>% get_pulse, 'month')
  expect_equal(date_seq('day') %>% get_pulse, 'day')
  expect_equal(date_seq('hour') %>% get_pulse, 'hour')
  expect_equal(date_seq('min') %>% get_pulse, 'min')
  expect_equal(date_seq('sec') %>% get_pulse, 'sec')
})



context('Test the get_date_variable function')

test_get_date_variable_data <-
  data.frame(x = date_seq('month'),
             y1 = runif(6),
             y2 = letters[1:6],
             y3 = factor(letters[7:12]),
             stringsAsFactors = FALSE)
test_get_date_variable_data2 <- test_get_date_variable_data
test_get_date_variable_data2$x2 <- date_seq('month')

test_that('get_date_variable only works on the right data types', {
  expect_error(get_date_variables(test_get_date_variable_data %>% as.matrix))
  expect_error(get_date_variables(test_get_date_variable_data$x))
})

test_that('get_date_variable gives the correct output', {
  expect_equal(get_date_variables(test_get_date_variable_data), 'x')
  expect_equal(get_date_variables(test_get_date_variable_data2), c('x', 'x2'))
  expect_equal(get_date_variables(mtcars), character())
})

