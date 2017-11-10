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

context("Testing the step suite, for after the interval is found")
test_that("get_difs helper function", {
  expect_equal(get_difs(c(1, 5, 6, 8)), c(4, 1, 2))
})

test_that("get_max_modulo_zero helper function", {
  expect_equal(get_max_modulo_zero(seq(0, length.out = 10, by = 2)), 2)
  expect_equal(get_max_modulo_zero(seq(0, length.out = 10, by = 3)), 3)
  expect_equal(get_max_modulo_zero(seq(0, length.out = 10, by = 11)), 11)
  expect_equal(get_max_modulo_zero(seq(0, length.out = 10, by = 51)), 51)
})

test_that("convert_month_to_number gives correct output", {
  mnths <- seq(ymd(20000101), length.out = 100, by = "month") %>%
    convert_month_to_number
  names(mnths) <- NULL
  expect_equal(mnths, 0:99)
})


test_that("step_of_year gives correct output", {
  x <- seq(ymd(20100101), length.out = 20, by = "year")
  expect_equal(step_of_year(x), 1)
  expect_equal( step_of_year(x[seq(1, 20, by = 2)]), 2)
  expect_equal( step_of_year(x[seq(1, 20, by = 3)]), 3)
  expect_equal( step_of_year(x[seq(1, 20, by = 5)]), 5)
  expect_equal( step_of_year(x[seq(1, 20, by = 9)]), 9)
})

test_that("step_of_quarter gives correct output", {
  x  <- seq(ymd(20100101), length.out = 20, by = "quarter")
  expect_equal( step_of_quarter(x), 1)
  expect_equal( step_of_quarter(x[seq(1, 20, by = 2)]), 2)
  expect_equal( step_of_quarter(x[seq(1, 20, by = 3)]), 3)
  expect_equal( step_of_quarter(x[seq(1, 20, by = 5)]), 5)
  expect_equal( step_of_quarter(x[seq(1, 20, by = 9)]), 9)
})

test_that("step_of_month gives correct output", {
  x  <- seq(ymd(20100101), length.out = 20, by = "month")
  expect_equal(step_of_month(x), 1)
  expect_equal( step_of_month(x[seq(1, 20, by = 2)]), 2)
  expect_equal( step_of_month(x[seq(1, 20, by = 3)]), 3)
  expect_equal( step_of_month(x[seq(1, 20, by = 5)]), 5)
  expect_equal( step_of_month(x[seq(1, 20, by = 9)]), 9)
})

test_that("step_of_difftime week gives correct output", {
  unit <- "week"
  x  <- seq(ymd(20160201), length.out = 20, by = unit)
  expect_equal(step_with_difftime(x, unit), 1)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 2)], unit), 2)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 3)], unit), 3)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 5)], unit), 5)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 9)], unit), 9)
})

test_that("step_with_difftime day gives correct output", {
  unit <- "day"
  x  <- seq(ymd(20160201), length.out = 20, by = unit)
  expect_equal(step_with_difftime(x, unit), 1)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 2)], unit), 2)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 3)], unit), 3)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 5)], unit), 5)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 9)], unit), 9)
})

test_that("step_with_difftime hour gives correct output", {
  unit <- "hour"
  x  <- seq(ymd_hms("20160201 000000" ), length.out = 20, by = unit)
  expect_equal(step_with_difftime(x, unit), 1)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 2)], unit), 2)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 3)], unit), 3)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 5)], unit), 5)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 9)], unit), 9)
})

test_that("step_with_difftime hour gives correct output", {
  unit <- "min"
  x  <- seq(ymd_hms("20160201 000000" ), length.out = 20, by = unit)
  expect_equal(step_with_difftime(x, unit), 1)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 2)], unit), 2)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 3)], unit), 3)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 5)], unit), 5)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 9)], unit), 9)
})

test_that("step_with_difftime hour gives correct output", {
  unit <- "sec"
  x  <- seq(ymd_hms("20160201 000000" ), length.out = 20, by = unit)
  expect_equal(step_with_difftime(x, unit), 1)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 2)], unit), 2)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 3)], unit), 3)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 5)], unit), 5)
  expect_equal( step_with_difftime(x[seq(1, 20, by = 9)], unit), 9)
})

context("test stop_on_NA")
test_that("stop_on_NA breaks the function when x has NAs", {
  x1 <- coffee$time_stamp
  x2 <- x1
  x2[2] <- NA
  expect_error(stop_on_NA(x1), NA)
  expect_error(stop_on_NA(x2), "interval cannot be determined when x contains NAs")
  expect_error(get_interval(x1), NA)
  expect_error(get_interval(x2), "interval cannot be determined when x contains NAs")
})
