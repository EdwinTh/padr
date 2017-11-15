context("Parts of center_interval")
test_that("int_to_secs gives the correct result", {
  sec_day <- 60 * 60 * 24
  expect_equal(int_to_secs(list(interval = "year", step = 2)),
               sec_day * 365 * 2)
  expect_equal(int_to_secs(list(interval = "quarter", step = 1)),
               sec_day * 365 / 4)
  expect_equal(int_to_secs(list(interval = "month", step = 3)),
               sec_day * 365 / 12 * 3)
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
  quarter_span <- span_date(201601, len_out = 3, by = "quarter")
  month_span <- span_date(201601, len_out = 3)
  week_span  <- span_date(2016, len_out = 3, by = "week")
  day_span   <- span_time(20160101, len_out = 3)
  hour_span  <- span_time("20160101 00", len_out = 3)
  min_span   <- span_time("20160101 0000", len_out = 3)
  sec_span    <- span_time("20160101 000000", len_out = 3, by = "5 sec")
  expect_equal(center_interval(year_span), year_span + 182.5)
  expect_equal(center_interval(year_span, "down"), year_span - 182.5)
  expect_equal(center_interval(year_span, interval = "10 days"), year_span + 5)
  expect_equal(center_interval(quarter_span), quarter_span + 365 / 8)
  expect_equal(center_interval(quarter_span, "down"), quarter_span - 365 / 8)
  expect_equal(center_interval(month_span), month_span + 365 / 24)
  expect_equal(center_interval(month_span, "down"), month_span - 365 / 24)
  expect_equal(center_interval(week_span), week_span + 7 / 2)
  expect_equal(center_interval(week_span, "down"), week_span - 7 / 2)
  expect_equal(center_interval(day_span), day_span + 3600 * 12)
  expect_equal(center_interval(day_span, "down"), day_span - 3600 * 12)
  expect_equal(center_interval(hour_span), hour_span + 1800)
  expect_equal(center_interval(hour_span, "down"), hour_span - 1800)
  expect_equal(center_interval(min_span), min_span + 30)
  expect_equal(center_interval(min_span, "down"), min_span - 30)
  expect_equal(center_interval(sec_span), sec_span + 2.5)
  expect_equal(center_interval(sec_span, "down"), sec_span - 2.5)
})


context("Parts of format_interval")

test_that("find_next_val helper function", {
  expect_equal(find_next_val(span_date(2016, 2018), 365),
               span_date(2017, 2019))
  expect_equal(find_next_val(span_time(2016, 2018), 365 * 24 * 3600),
               span_time(2017, 2019))
  expect_equal(find_next_val(span_date(201601, 201603), 31),
               span_date(201602, 201604))
  expect_equal(find_next_val(span_time(201601, 201603), 31 * 24 * 3600),
               span_time(201602, 201604))
  expect_equal(find_next_val(span_date(201601, len_out = 3, by = "week"), 7),
               span_date(20160108, len_out = 3, by = "week"))
  expect_equal(find_next_val(span_time(201601, len_out = 3, by = "week"), 7 * 24 * 3600),
               span_time(20160108, len_out = 3, by = "week"))
  non_eq_days <- span_date(20160101, 20160104)[c(1, 2, 4)]
  expect_equal(find_next_val(non_eq_days, 1),
               c(non_eq_days[c(2, 3)], as.Date("2016-01-05")))
})


test_that("get_units_to_last helper function", {
  gutl <- get_units_to_last
  expect_equal(gutl(span_date(2016, len_out = 2)), 365)
  expect_equal(gutl(span_time(2016, len_out = 2)), 365 * 24 * 3600)
  expect_equal(gutl(span_date(201601, len_out = 2)), 365 / 12)
  expect_equal(gutl(span_time(201601, len_out = 2)), round(30.41667 * 24 * 3600))
  expect_equal(gutl(span_date(20160101, len_out = 2)), 1)
  expect_equal(gutl(span_date(20160101, len_out = 5)[c(1, 3, 5)]), 2)
  expect_equal(gutl(span_time(20160101, len_out = 2)), 1 * 24 * 3600)
  expect_equal(gutl(span_time(20160101, len_out = 5)[c(1, 3, 5)]), 2 * 24 * 3600)
  expect_equal(gutl(span_time("20160101 00", len_out = 2)), 3600)
  expect_equal(gutl(span_time("20160101 00", len_out = 10)[c(2, 5, 8)]), 3600 * 3)
  expect_equal(gutl(span_time("20160101 00", len_out = 2)), 3600)
  expect_equal(gutl(span_time("20160101 00", len_out = 10)[c(2, 5, 8)]), 3600 * 3)
  expect_equal(gutl(span_time("20160101 0000", len_out = 2)), 60)
  expect_equal(gutl(span_time("20160101 000000", len_out = 10)[c(2, 5, 8)]), 3)
})


context("format_interval integration tests")
fse <- format_interval
x1 <- span_date(2016, 2017)
x2 <- span_time("20160101 00", len_out = 2, tz = "CET")
x3 <- lubridate::ymd(c(20160101, 20160103, 20160104))

test_that("format_interval throws errors", {
  expect_error(fse(1:10))
  expect_error(fse(c("20160101", "20160101")))
  expect_error(fse(span_date(2016, 2018)), NA)
  expect_error(fse(span_time(2016, 2018)), NA)
  expect_error(fse(rep(span_date(2016, 2017)), 2))
  expect_error(fse(lubridate::ymd(20160101)))
})

test_that("format_interval formatting", {
  expect_equal(fse(x1), c("2016-01-01 2017-01-01", "2017-01-01 2018-01-01"))
  expect_equal(fse(x1, start_format = "%y", sep = "-"), c("16-17", "17-18"))
  expect_equal(fse(x2, start_format = "%F %H", end_format = "%H", sep = "-"),
               c("2016-01-01 00-01", "2016-01-01 01-02"))
})

test_that("format_interval end _offset works", {
  expect_equal(fse(x1, end_offset = 1),
               c("2016-01-01 2016-12-31", "2017-01-01 2017-12-31"))
  expect_equal(fse(x2, start_format = "%F %T", end_offset = 60),
               c("2016-01-01 00:00:00 2016-01-01 00:59:00",
                 "2016-01-01 01:00:00 2016-01-01 01:59:00"))
})

test_that("format_interval works for non symmetrics", {
  expect_equal(fse(x3),
               c("2016-01-01 2016-01-03", "2016-01-03 2016-01-04", "2016-01-04 2016-01-05"))
  expect_equal(fse(x3, units_to_last = 2),
               c("2016-01-01 2016-01-03", "2016-01-03 2016-01-04", "2016-01-04 2016-01-06"))
})

test_that("format_interval works with different timezones", {
  dt <- span_time("20170801 00", by = "3 hour", len_out = 3,
                  tz = "EST")
  expect_equal(fse(dt, start_format = "%H"),
               c("00 03", "03 06", "06 09"))
})
