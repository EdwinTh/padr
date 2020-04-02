date_span <- span_date(20161226, len_out = 10)
time_span <- span_time("20170101 00", len_out = 10)
min_span  <- span_time("20170101 0000", len_out = 10)
sec_span  <- span_time("20170101 000000", len_out = 10)
context("Check proper error handling wrong inputs subset_span")

test_that("function breaks on wrong names in span_list", {
  expect_error(subset_span(date_span, list(yr = 1:10)))
  expect_error(subset_span(date_span, list(day = 1:10)))
  expect_error(subset_span(date_span, list(year = 1:10)), NA)
  expect_error(subset_span(date_span, list(mon = 1:10)), NA)
  expect_error(subset_span(date_span, list(wday = 1:3)), NA)
  expect_error(subset_span(date_span, list(mday = 1:10)), NA)
  expect_error(subset_span(time_span, list(hour = 1:10)), NA)
  expect_error(subset_span(time_span, list(min = 1:10)), NA)
  expect_error(subset_span(time_span, list(sec = 1:10)), NA)
})

test_that("function breaks on wrong input", {
  expect_error(subset_span(1:10, list(year = 1:10)),
               "x is not of class POSIXct, POSIXlt, or Date")
  expect_error(subset_span(TRUE, list(year = 1:10)),
               "x is not of class POSIXct, POSIXlt, or Date")
  expect_error(subset_span("2016-01-01", list(year = 1:10)),
               "x is not of class POSIXct, POSIXlt, or Date")
  expect_error(subset_span(20160101, list(year = 1:10)),
               "x is not of class POSIXct, POSIXlt, or Date")
  expect_error(subset_span(date_span, list(year = 1:10)), NA)
  expect_error(subset_span(time_span, list(year = 1:10)), NA)
})

context("Check subset_span on expected output")
test_that("subset_span gives the correct output", {
  expect_equal(subset_span(date_span, list(year = 2016)),
               date_span[1:6])
  expect_equal(subset_span(date_span, list(mon = 12)),
               date_span[1:6])
  expect_equal(subset_span(date_span, list(wday = 1)),
               date_span[c(1, 8)])
  expect_equal(subset_span(date_span, list(mday = 26:31)),
               date_span[1:6])
  expect_equal(subset_span(time_span, list(hour = 0:4)),
               time_span[1:5])
  expect_equal(subset_span(min_span, list(min = 0:4)),
               min_span[1:5])
  expect_equal(subset_span(sec_span, list(sec = 0:4)),
               sec_span[1:5])
})

