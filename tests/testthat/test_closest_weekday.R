source("library.R")

x <- span_date(20170104, len_out = 7, by = "day")

context("closest_weekday function")
test_that("closest_weekday down", {
  expect_equal(closest_weekday(x), ymd(20170102))
  expect_equal(closest_weekday(x, 3), ymd(20170104))
  expect_equal(closest_weekday(x, 5), ymd(20161230))
})

test_that("closest_weekday up", {
  expect_equal(closest_weekday(x, direction = "up"), ymd(20170109))
  expect_equal(closest_weekday(x, 3, direction = "up"), ymd(20170104))
  expect_equal(closest_weekday(x, 5, direction = "up"), ymd(20170106))
})
