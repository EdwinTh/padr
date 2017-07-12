context("check span_time input functions")

test_that("function only works on the appropriate classes", {
  expect_error(integer_to_date("20160101 01"), NA)
  expect_error(integer_to_date(TRUE))
  expect_error(integer_to_date(2014), NA)
  expect_error(integer_to_date(2014.0), NA)
  expect_error(integer_to_date(2014.1))
  expect_error(integer_to_date(as.Date("2014-01-01")))
})

test_that("match_date_pattern only matches the right patters", {
  expect_true(match_date_pattern("20160101 01"))
  expect_true(match_date_pattern("20160101 0101"))
  expect_true(match_date_pattern("20160101 010101"))
  expect_false(match_date_pattern("20160101"))
  expect_false(match_date_pattern("20160101 0"))
  expect_false(match_date_pattern("20160101 010"))
  expect_false(match_date_pattern("20160101 01010"))
})
