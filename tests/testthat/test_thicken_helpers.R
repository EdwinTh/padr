source("library.R")

a_date <- ymd(c("20151201", "20160201"))
a_ct <- ymd_h(c("20151201 03", "20160201 03"))
b_date <- span(a_date, convert_interval("month"))
b_ct   <- ymd_hms(c("2015-01-01 00:00:00", "2016-01-01 00:00:00",
                    "2017-01-01 00:00:00"))
b_ct_tz_is_NULL <- b_ct
attr(b_ct_tz_is_NULL, 'tzone') <- NULL

context("to_posix creates correct result")
test_that("to_posix sets second to posix if first is", {
  date_date   <- to_posix(a_date, b_date)
  posix_date  <- to_posix(a_ct, b_date)
  date_posix  <- to_posix(a_date, b_ct)
  posix_posix <- to_posix(a_ct, b_ct)
  date_posix_tz_null <- to_posix(a_date, b_ct_tz_is_NULL)
  posix_date_tz_null <- to_posix(b_ct_tz_is_NULL, a_date)
  expect_equal( date_date$a %>% class, "Date")
  expect_equal( date_date$b %>% class, "Date")
  expect_equal( posix_date$a %>% class, c("POSIXct", "POSIXt"))
  expect_equal( posix_date$b %>% class, c("POSIXct", "POSIXt"))
  expect_equal( date_posix$a %>% class, c("POSIXct", "POSIXt"))
  expect_equal( date_posix$b %>% class, c("POSIXct", "POSIXt"))
  expect_equal( posix_posix$a %>% class, c("POSIXct", "POSIXt"))
  expect_equal( posix_posix$b %>% class, c("POSIXct", "POSIXt"))
  expect_equal( date_posix_tz_null$a %>% class, c("POSIXct", "POSIXt"))
  expect_equal( date_posix_tz_null$b %>% class, c("POSIXct", "POSIXt"))
})


context("round_up_core and round_down_core work as expected")
test_that("round_up_core gives correct result", {
  expect_equal( round_up_core(a_date, b_date),
                as.numeric (as.Date ( c("2016-01-01",  "2016-03-01"))))
  expect_equal( round_up_core(a_ct, b_ct),
                as.numeric (ymd_hms ( c("2016-01-01 00:00:00",
                                        "2017-01-01 00:00:00"))))
})

test_that("round_down_core gives correct result", {
  expect_equal( round_down_core(a_date, b_date),
                as.numeric (as.Date ( c("2015-12-01",  "2016-02-01"))))
  expect_equal( round_down_core(a_ct, b_ct),
                as.numeric (ymd_hms ( c("2015-01-01 00:00:00",
                                        "2016-01-01 00:00:00"))))
})

context("posix_to_date works as expected")
test_that("posix_to_date gives date when possible", {
  expect_equal( posix_to_date(a_date) %>% class, "Date" )
  expect_equal( posix_to_date(a_ct) %>% class, c("POSIXct", "POSIXt"))
  expect_equal( posix_to_date(b_ct) %>% class, "Date" )
})
