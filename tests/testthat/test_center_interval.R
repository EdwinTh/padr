context("Helper functions of center_interval")
test_that("int_to_secs gives the correct result", {
  sec_day <- 60 * 60 * 24
  expect_equal(int_to_secs(list(interval = "year", step = 2)),
               sec_day * 365 * 2)
  expect_equal(int_to_secs(list(interval = "quarter", step = 1)),
               sec_day * 365/4)
  expect_equal(int_to_secs(list(interval = "month", step = 3)),
               sec_day * 365/12 * 3)
  expect_equal(int_to_secs(list(interval = "week", step = 8)),
               sec_day * 7  * 8)
  expect_equal(int_to_secs(list(interval = "day", step = 1)),
               sec_day)
  expect_equal(int_to_secs(list(interval = "hour", step = 100)),
               360000)
  expect_equal(int_to_secs(list(interval = "min", step = 10)),
               600)
  expect_equal(int_to_secs(list(interval = "sec", step = 2)),
               2)
})

test_that("int_to_days gives the correct result", {
  expect_equal(int_to_days(list(interval = "year", step = 10)), 3650)
  expect_equal(int_to_days(list(interval = "month", step = 12)), 365)
  expect_equal(int_to_days(list(interval = "day", step = 2)), 2)
})

test_that("center_interval integration tests", {
  year_span  <- span_date(2016, len_out = 3)
  quarter_span <- span_date(201601, len_out = 3, interval = "quarter")
  month_span <- span_date(201601, len_out = 3)
  week_span  <- span_date(2016, len_out = 3, interval = "week")
  day_span   <- span_time(20160101, len_out = 3)
  hour_span  <- span_time("20160101 00", len_out = 3)
  min_span   <- span_time("20160101 0000", len_out = 3)
  sec_span    <- span_time("20160101 000000", len_out = 3, interval = "5 sec")
  expect_equal(center_interval(),
               span_date(2016,, ))
})
