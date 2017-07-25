date_span <- span_date(20170701, len_out = 10)
time_span <- span_time("20170101 00", len_out = 10)

context("Check proper error handling wrong inputs subset_span")

test_that("function breaks on wrong names in span_list", {
  msg <- "invalid names in the span_list"
  expect_error(subset_span(date_span, list(yr = 1:10)), msg)
  expect_error(subset_span(date_span, list(day = 1:10)), msg)
  expect_error(subset_span(date_span, list(year = 1:10)), NA)
  expect_error(subset_span(date_span, list(mon = 1:10)), NA)
  expect_error(subset_span(date_span, list(wday = 1:3)), NA)
  expect_error(subset_span(date_span, list(mday = 1:10)), NA)
  expect_error(subset_span(time_span, list(hour = 1:10)), NA)
  expect_error(subset_span(time_span, list(min = 1:10)), NA)
  expect_error(subset_span(time_span, list(sec = 1:10)), NA)
})

test_that("function breaks on wrong input", {
  expect_error(subset_span(1:10, list(year = 1:10)))
})
