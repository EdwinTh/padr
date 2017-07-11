context("check span_time input functions")

test_that("match_date_pattern only matches the right patters", {
  expect_true(match_date_pattern("20160101 01"))
  expect_true(match_date_pattern("20160101 0101"))
  expect_true(match_date_pattern("20160101 010101"))
  expect_false(match_date_pattern("20160101"))
  expect_false(match_date_pattern("20160101 0"))
  expect_false(match_date_pattern("20160101 010"))
  expect_false(match_date_pattern("20160101 01010"))
})
