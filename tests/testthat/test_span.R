source("library.R")

# standard inputs for class testing
day_vec <- as.Date(c("2016-01-01", "2016-02-29"))
posix_ct <- as.POSIXct(c("2016-01-01 21:19:53", "2016-02-29 10:16:11"))
posix_lt <- as.POSIXlt(c("2016-01-01 21:19:53", "2016-02-29 10:16:11"))
one_posix <- as.POSIXlt("2016-02-17 21:19:53")

generate_interval <- function(unit, int) {
  list(interval = int, step = unit)
}

context("span fails on wrong input")

test_that("span fails on non-datetime vectors", {
  expect_error(as.integer(day_vec) %>% span)
  expect_error(as.numeric(day_vec) %>% span)
  expect_error(as.character(day_vec) %>% span)
  expect_error(as.factor(day_vec) %>% span)
})

test_that("span fails on data.frames", {
  expect_error(mtcars %>% span)
})

test_that("span does not fail on Date, POSIXct and POSIXlt", {
  expect_error(day_vec %>% span(generate_interval(1, "day")), NA)
  expect_error(posix_ct %>% span(generate_interval(1, "sec")), NA)
  expect_error(posix_lt %>% span(generate_interval(1, "sec")), NA)
})


context("units of the get_interval funcition")

test_that("next_ gives correct ouput", {
  expect_equal(next_year(one_posix)$year, 117)
  expect_equal(next_month(one_posix)$mon, 2)
  expect_equal(next_day(one_posix)$mday, 18)
  expect_equal(next_hour(one_posix)$hour, 22)
  expect_equal(next_min(one_posix)$min, 20)
  expect_equal(next_sec(one_posix)$sec, 54)
})

test_that("set to 0/1 gives correct output", {
  expect_equal(month_to_1(one_posix)$mon, 0)
  expect_equal(day_to_1(one_posix)$mday, 1)
  expect_equal(hour_to_0(one_posix)$hour, 0)
  expect_equal(min_to_0(one_posix)$min, 0)
  expect_equal(sec_to_0(one_posix)$sec, 0)
})


test_that("quarter functions give correct output", {
  expect_equal(this_quarter_month(one_posix)$mon, 0)
  expect_equal(next_quarter_month(one_posix)$mon, 3)
})

test_that("week functions give correct output", {
  expect_equal(this_week(one_posix)$mday, 14)
  expect_equal(next_week(one_posix)$mday, 21)
})


context("end_val when start_val is specified")

test_that("shift_end_from_start gives correct output", {
  gsae_year <- get_start_and_end(posix_ct, convert_interval("year"))
  gsae_week <- get_start_and_end(posix_ct, convert_interval("week"))
  expect_equal(shift_end_from_start(gsae_year, as.Date("2016-02-01")),
               as.Date("2017-02-01"))
  expect_equal(shift_end_from_start(gsae_year, as.POSIXct("2016-02-01 00:04:01")), #nolint
               as.POSIXct("2017-02-01 00:04:01"))
  expect_equal(shift_end_from_start(gsae_week, as.Date("2015-12-25")),
               as.Date("2016-03-04"))
  expect_equal(shift_end_from_start(gsae_week, as.POSIXct("2015-12-25 00:04:01")), #nolint
               as.POSIXct("2016-03-04 00:04:01"))
})

context("assure_greater_than_max_x makes sure that end_val is greater than max_x")  #nolint

test_that("assure_greater_than_max_x works properly", {
  max_x_date <- as.Date("2016-10-21")
  max_x_posix <- as.POSIXct("2016-10-21 12:31:43")
  end_val_date <- as.Date("2016-08-14")
  end_val_posix <- as.POSIXct("2016-08-14 14:39:04")

  expect_equal( assure_greater_than_max_x(max_x_date, end_val_date, "month"),
                as.Date("2016-11-14"))
  expect_equal( assure_greater_than_max_x(max_x_date, end_val_posix, "month"),
                as.POSIXct("2016-11-14 14:39:04"))
  expect_equal( assure_greater_than_max_x(max_x_posix, end_val_posix, "month"),
                as.POSIXct("2016-11-14 14:39:04"))

  expect_equal( assure_greater_than_max_x(max_x_date, end_val_date, "day"),
                as.Date("2016-10-22"))
  expect_equal( assure_greater_than_max_x(max_x_date, end_val_posix, "day"),
                as.POSIXct("2016-10-21 14:39:04"))
  expect_equal( assure_greater_than_max_x(max_x_posix, end_val_posix, "day"),
                as.POSIXct("2016-10-21 14:39:04"))
})

test_that("span integration tests", {
  x <- coffee$time_stamp[3:4]
  sp <- function(interval) span(x, list(interval = interval, step = 1))
  expect_equal(sp("year"), span_date(2016, 2017))
  expect_equal(sp("quarter"), as.Date(c("2016-07-01", "2016-10-01")))
  expect_equal(sp("month"), span_date(201607, 201608))
  expect_equal(sp("week"), as.Date(c("2016-07-03", "2016-07-10", "2016-07-17")))
  expect_equal(sp("day"), span_date(20160709, 20160711))
})

context("span_around function")

test_that("span_around breaks on the wrong input", {
  expect_error(span_around(coffee, "hour"))
  expect_error(span_around(coffee$time_stamp, "hour"), NA)
  expect_error(span_around(coffee$time_stamp, "hour", start_shift = 12))
  expect_error(span_around(coffee$time_stamp, "hour", start_shift = "12 hour"), NA)
  expect_error(span_around(coffee$time_stamp, "hour", end_shift = 12))
  expect_error(span_around(coffee$time_stamp, "hour", end_shift = "12 hour"), NA)
})


test_that("span_around integration tests", {
  x <- as.POSIXct(c("2016-07-09 13:25:17", "2016-07-10 10:45:11"), tz = "CET")
  start_shift_one_hour <- span_around(x, "hour", start_shift = "1 hour")
  end_shift_one_hour <- span_around(x, "hour", end_shift = "1 hour")
  expect_error(span_around(1:10, "day"))
  expect_error(span_around(x, "day"), NA)
  expect_error(span_around(x, "2 days"), NA)
  expect_equal(span_around(x, "day"), span_date(20160709, 20160711))
  expect_equal(span_around(x, "hour"),
               span_time("20160709 13", "20160710 11", tz = "CET"))
  expect_equal(span_around(x, "min"),
               span_time("20160709 13", "20160710 1046", tz = "CET"))
  expect_equal(start_shift_one_hour[1],
               as.POSIXct("2016-07-09 12:00:00", tz = "CET"))
  expect_equal(start_shift_one_hour[length(start_shift_one_hour)],
               as.POSIXct("2016-07-10 12:00:00", tz = "CET"))
  expect_equal(end_shift_one_hour[1],
               as.POSIXct("2016-07-09 13:00:00", tz = "CET"))
  expect_equal(end_shift_one_hour[length(end_shift_one_hour)],
               as.POSIXct("2016-07-10 12:00:00", tz = "CET"))
  expect_equal(span_around(x, "day", start_shift = "1 day")[1], as.Date("2016-07-08"))
})
