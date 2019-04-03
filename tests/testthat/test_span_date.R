context("building blocks span_date")
sw <- suppressWarnings
test_that("check_to_len_out give right error", {
  jos <- 40
  expect_error(sw(check_to_len_out(jos, jos)), NA)
  expect_warning(check_to_len_out(jos, jos),
                 "both to and len_out are specified, len_out is ignored")
  expect_error(check_to_len_out(jos, NULL), NA)
  expect_error(check_to_len_out(NULL, jos), NA)
  expect_error(check_to_len_out(NULL, NULL),
               "either to or len_out must be specified")
})

test_that("check_valid_input_span errors on wrong datatype", {
  expect_error(check_valid_input_span(2011), NA)
  expect_error(check_valid_input_span("2011"), NA)
  expect_error(check_valid_input_span(TRUE),
               "from is not a character or numeric")
  expect_error(check_valid_input_span(as.Date("2011-01-01")),
               "from is not a character or numeric")
})

test_that("valid_numeric_dt errors on wrong format", {
  expect_error(valid_numeric_dt(2011.1, "jos"), "jos is not a valid integer")
  expect_error(valid_numeric_dt(2011.0, "jos"), NA)
  expect_error(valid_numeric_dt(2011L, "jos"), NA)
  expect_error(valid_numeric_dt(201, "jos"), "jos is not of a valid length")
  expect_error(valid_numeric_dt(2011), NA)
  expect_error(valid_numeric_dt(20110, "jos"), "jos is not of a valid length")
  expect_error(valid_numeric_dt(201101), NA)
  expect_error(valid_numeric_dt(2011010, "jos"), "jos is not of a valid length")
  expect_error(valid_numeric_dt(20110101), NA)
})

test_that("match_date_pattern only matches the right patterns", {
  expect_true(match_date_pattern("2016"))
  expect_true(match_date_pattern("201601"))
  expect_true(match_date_pattern("20160101"))
  expect_false(match_date_pattern("20160"))
  expect_false(match_date_pattern("2016010"))
  expect_false(match_date_pattern("2016010 0"))
})

test_that("match_date_time_pattern only matches the right patterns", {
  expect_true(match_date_time_pattern("20160101 01"))
  expect_true(match_date_time_pattern("20160101 0101"))
  expect_true(match_date_time_pattern("20160101 010101"))
  expect_false(match_date_time_pattern("20160101"))
  expect_false(match_date_time_pattern("20160101 0"))
  expect_false(match_date_time_pattern("2016010 010"))
  expect_false(match_date_time_pattern("2016010 01010"))
})

test_that("convert_to_date gives the correct outputs", {
  expect_equal(convert_to_date(2011), as.Date("2011-01-01"))
  expect_equal(convert_to_date(201102), as.Date("2011-02-01"))
  expect_equal(convert_to_date(20110203), as.Date("2011-02-03"))
  expect_equal(convert_to_date("2011"), as.Date("2011-01-01"))
  expect_equal(convert_to_date("201102"), as.Date("2011-02-01"))
  expect_equal(convert_to_date("20110203"), as.Date("2011-02-03"))
})

test_that("interval_from_short gives correct outputs", {
  expect_equal(interval_from_short(4), "year")
  expect_equal(interval_from_short(6), "month")
  expect_equal(interval_from_short(8), "day")
})

test_that("convert_to_datetime gives the correct outputs", {
  p <- function(x) as.POSIXct(x, tz = "UTC")
  expect_equal(convert_to_datetime(2011), p("2011-01-01"))
  expect_equal(convert_to_datetime(201102), p("2011-02-01"))
  expect_equal(convert_to_datetime(20110203), p("2011-02-03"))
  expect_equal(convert_to_datetime("2011"), p("2011-01-01"))
  expect_equal(convert_to_datetime("201102"), p("2011-02-01"))
  expect_equal(convert_to_datetime("20110203"), p("2011-02-03"))
  expect_equal(convert_to_datetime("20110101 01"), p("2011-01-01 01:00:00"))
  expect_equal(convert_to_datetime("201102"), p("2011-02-01"))
  expect_equal(convert_to_datetime("20110203"), p("2011-02-03"))
})

test_that("interval_from_long gives correct outputs", {
  expect_equal(interval_from_long(4), "year")
  expect_equal(interval_from_long(6), "month")
  expect_equal(interval_from_long(8), "day")
  expect_equal(interval_from_long(11), "hour")
  expect_equal(interval_from_long(13), "min")
  expect_equal(interval_from_long(15), "sec")
})

context("span_date integration tests")
test_that("span_date gives the desired outputs", {
  year_span <- seq.Date(as.Date("2011-01-01"), as.Date("2015-01-01"), by = "year")
  month_span <- seq.Date(as.Date("2011-01-01"), as.Date("2015-01-01"), by = "month")
  day_span <- seq.Date(as.Date("2011-01-01"), as.Date("2011-02-01"), by = "day")
  expect_equal(span_date(2011, 2015), year_span)
  expect_equal(span_date("2011", 2015), year_span)
  expect_equal(span_date("2011", "2015"), year_span)
  expect_equal(span_date(201101, 201501), month_span)
  expect_equal(span_date(2011, 2015, by = "month"), month_span)
  expect_equal(span_date(20110101, 20110201), day_span)
  expect_equal(span_date(20110101, 20150101, by = "month"), month_span)
  expect_equal(span_date(2011, len_out = 5), year_span)
  expect_equal(span_date(201101, len_out = 49), month_span)
  expect_equal(span_date(20110101, len_out = 32), day_span)
  expect_equal(span_date(20110101, len_out = 49, by = "month"), month_span)
})

context("span_time integration tests")
test_that("span_time gives the desired outputs", {
  p <- function(x) as.POSIXct(x, tz = "UTC")
  day_span <- seq.POSIXt(p("2011-01-01"), p("2011-02-01"), by = "day")
  hour_span <- seq.POSIXt(p("2011-01-01 00:00:00"), p("2011-01-01 23:00:00"), by = "hour")
  min_span <- seq.POSIXt(p("2011-01-01 00:00:00"), p("2011-01-01 00:25:00"), by = "min")
  sec_span <- seq.POSIXt(p("2011-01-01 00:00:00"),
                         p("2011-01-01 00:00:25"), by = "sec")

  expect_equal(span_time(20110101, 20110201), day_span)
  expect_equal(span_time("20110101", "20110201"), day_span)
  expect_equal(span_time("2011", "20110201"), day_span)
  expect_equal(span_time("20110101 00", "20110101 23"), hour_span)
  expect_equal(span_time("2011", "20110101 23"), hour_span)
  expect_equal(span_time("20110101 00", len_out = 24), hour_span)
  expect_equal(span_time("20110101 0000", "20110101 0025"), min_span)
  expect_equal(span_time("20110101 0000", len_out = 26), min_span)
  expect_equal(span_time("20110101 000000", "20110101 000025"), sec_span)
  expect_equal(span_time("20110101 000000", len_out = 26), sec_span)
})
