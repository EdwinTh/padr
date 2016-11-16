#-----------------------------------------------------------------------------#
# get_date_variables

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

#-----------------------------------------------------------------------------#

# enforce_time_zone
context('Test the enforce_time_zone')

a_ct <- ymd_h(c('20151201 03', '20160201 03'))
b_ct <- ymd_hms(c('2015-01-01 00:00:00', '2016-01-01 00:00:00'))
c_ct <- ymd_hms(c('2015-01-01 00:00:00', '2016-01-01 00:00:00'), tz = 'CET')

equal     <- enforce_time_zone(a_ct, b_ct)
different <- enforce_time_zone(a_ct, c_ct)

test_that('enforce_timee zone works as expected', {
  expect_warning( enforce_time_zone(a_ct, c_ct))
  expect_equal( attr(equal, 'tz'), 'UTC')
  expect_equal( as.character(equal), c("2015-12-01 03:00:00", "2016-02-01 03:00:00"))
  expect_equal( attr(different, 'tz'), 'CET')
  expect_equal( as.character(different), c("2015-12-01 03:00:00", "2016-02-01 03:00:00"))
})

