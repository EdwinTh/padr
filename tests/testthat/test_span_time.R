context("check valid_char_dt input functions")

test_that("function only works on the appropriate classes", {
  expect_error(valid_char_dt("20160101 01", "jos"), NA)
  expect_error(valid_char_dt(TRUE, "jos"))
  expect_error(valid_char_dt(201401010101, "jos"))
  expect_error(valid_char_dt(2014.0, "jos"))
  expect_error(valid_char_dt(2014.1, "jos"))
  expect_error(valid_char_dt(as.Date("2014-01-01"), "jos"))
})

test_that("match_date_pattern only matches the right patterns", {
  expect_true(match_date_pattern("20160101 01"))
  expect_true(match_date_pattern("20160101 0101"))
  expect_true(match_date_pattern("20160101 010101"))
  expect_false(match_date_pattern("20160101"))
  expect_false(match_date_pattern("20160101 0"))
  expect_false(match_date_pattern("20160101 010"))
  expect_false(match_date_pattern("20160101 01010"))
})


context("span_time integration tests")
test_that("span_time gives the desired outputs", {
  year_span <- seq.POSIXt(as.POSIXct("2011-01-01 00-00-00", tz = "UTC"),
                          as.POSIXct("2015-01-01", tz = "UTC"), by = "year")
  hour_span <- seq.POSIXt(as.POSIXct("2011-01-01 00-00-00", tz = "UTC"),
                          as.POSIXct("2011-01-02 00-00-00", tz = "UTC"), by = "hour")
  min_span <- seq.POSIXt(as.POSIXct("2011-01-01 00-00-00"),
                         as.POSIXct("2011-01-02 00-00-00"), by = "min")
  sec_span <- seq.POSIXt(as.POSIXct("2011-01-01 00-00-00"),
                         as.POSIXct("2011-01-02 00-00-00"), by = "sec")

  expect_equal(span_time("20110101 00", "20110102 00"), hour_span)
  expect_equal(span_time(201101, 201501), month_span)
  expect_equal(span_time(2011, 2015, interval = "month"), month_span)
  expect_equal(span_time(20110101, 20110201), day_span)
  expect_equal(span_time(20110101, 20150101, interval = "month"), month_span)
  expect_equal(span_time(2011, len_out = 5), year_span)
  expect_equal(span_time(201101, len_out = 49), month_span)
  expect_equal(span_time(20110101, len_out = 32), day_span)
  expect_equal(span_time(20110101, len_out = 49, interval = "month"), month_span)
})
