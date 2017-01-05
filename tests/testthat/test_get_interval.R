source("library.R")
# standard inputs for class testing
days <- as.Date(c("2016-01-01", "2016-02-29"))
posix_ct <- as.POSIXct(c("2016-01-01 21:19:53", "2016-02-29 10:16:11"))
posix_lt <- as.POSIXlt(c("2016-01-01 21:19:53", "2016-02-29 10:16:11"))


context("get_interval fails on wrong input")

test_that("get_interval fails on non-datetime vectors", {
  expect_error(as.integer(days) %>% get_interval)
  expect_error(as.numeric(days) %>% get_interval)
  expect_error(as.character(days) %>% get_interval)
  expect_error(as.factor(days) %>% get_interval)
})

test_that("get_interval fails on data.frames", {
  expect_error(mtcars %>% get_interval)
})

test_that("get_interval does not fail on Date, POSIXct and POSIXlt", {
  expect_error(days %>% get_interval, NA)
  expect_error(posix_ct %>% get_interval, NA)
  expect_error(posix_lt %>% get_interval, NA)
})


context("datetime_char makes a full datetime character")

test_that("class is character", {
  expect_equal(days %>% datetime_char %>% class, "character")
  expect_equal(posix_ct %>% datetime_char %>% class, "character")
  expect_equal(posix_lt %>% datetime_char %>% class, "character")
})

test_that("datetime_char adds zeros to Date", {
  expect_equal(days %>% datetime_char,
               c("2016-01-01 00:00:00", "2016-02-29 00:00:00"))
})

test_that("datetime_char does not change POSIXt", {
  expect_equal(posix_ct %>% datetime_char,
               c("2016-01-01 21:19:53", "2016-02-29 10:16:11"))
  expect_equal(posix_lt %>% datetime_char,
               c("2016-01-01 21:19:53", "2016-02-29 10:16:11"))
})


context("lowest_differ returns the correct levels that differ")

test_that("lowest_differ return empty character whe x does not differ", {
  expect_equal( "2016-01-01 00:00:00" %>% lowest_differ, character(0) )
  expect_equal( c("2016-01-01 00:00:00", "2016-01-01 00:00:00") %>%
                  lowest_differ, character(0) )
})

test_that("get_interval breaks when x does not differ", {
  expect_error( "2016-01-01 00:00:00" %>% get_interval )
  expect_error( c("2016-01-01 00:00:00", "2016-01-01 00:00:00") %>%
                  get_interval)
})

test_that("lowest_differ returns the correct interval", {
  expect_equal( c("2016-01-01 00:00:00", "2017-01-01 00:00:00") %>%
                  lowest_differ, "year")
  expect_equal( c("2016-01-01 00:00:00", "2017-02-01 00:00:00") %>%
                  lowest_differ, "month")
  expect_equal( c("2016-01-01 00:00:00", "2017-02-03 00:00:00") %>%
                  lowest_differ, "day")
  expect_equal( c("2016-01-01 00:00:00", "2017-02-03 12:00:00") %>%
                  lowest_differ, "hour")
  expect_equal( c("2016-01-01 00:00:00", "2017-02-03 12:51:00") %>%
                  lowest_differ, "min")
  expect_equal( c("2016-01-01 00:00:00", "2017-02-03 12:51:23") %>%
                  lowest_differ, "sec")
})


context("is_ functions correctly identify quarter and week")

test_that("is_month_quarter gives correct result", {
  expect_true( is_month_quarter(
    c("2016-01-01 00:00:00", "2016-04-01 00:00:00")))
  expect_false( is_month_quarter(
    c("2016-01-01 00:00:00", "2016-03-01 00:00:00")))
  expect_true( is_month_quarter(
    c("2016-01-01 12:51:16", "2016-01-01 12:51:16")))
  expect_false( is_month_quarter(
    c("2016-01-01 12:51:16", "2016-03-01 12:51:16")))
})

test_that("is_day_week gives correct result", {
  expect_true( is_day_week(c("2016-01-01 00:00:00", "2016-01-08 00:00:00")))
  expect_false( is_day_week(c("2016-01-01 00:00:00", "2016-01-09 00:00:00")))
  expect_true( is_day_week(c("2016-01-01 12:51:16", "2016-01-08 12:51:16")))
  expect_false( is_day_week(c("2016-01-01 12:51:16", "2016-01-09 12:51:16")))
})
