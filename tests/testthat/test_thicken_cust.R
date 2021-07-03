source('library.R')
dt_var  <- coffee$time_stamp
spanned <- span_date(20160708, 20160710)
full_spanned <- span_date(20160707, 20160712)

context("filtering when spanned is greater or smaller")

test_that("warning is thrown by warn_when_filtering", {
  expect_warning(warning_when_filtering(dt_var, full_spanned), NA)
  expect_warning(warning_when_filtering(dt_var, spanned),
                 "Dropping all values in the datetime var that are smaller than smallest spanned")
})

test_that("start_val_after_min_dt works in this context", {
  expect_equal(start_val_after_min_dt(min(spanned), dt_var), c(F, F, T, T))
  expect_equal(start_val_after_min_dt(min(full_spanned), dt_var), c(T, T, T, T))
})

test_that("end_val_before_max_dt works in this context", {
  expect_equal(end_val_before_max_dt(max(spanned), dt_var), c(T, T, T, F))
  expect_equal(end_val_before_max_dt(max(full_spanned), dt_var), c(T, T, T, T))
})

context("thicken_cust integration tests")

spanned_asym <- ymd_h(c("2016-07-07 08", "2016-07-09 11", "2016-07-10 23"),
                      tz = "UTC")
sw <- suppressWarnings

test_that("thicken_cust works properly", {
  # Manually setting the ts var in coffee, so test passes on systems with
  # a different time zone

  coffee$time_stamp <- as.POSIXct(c(
    "2016-07-07 09:11:21", "2016-07-07 09:46:48", "2016-07-09 13:25:17", "2016-07-10 10:45:11"
  ), tz = 'UTC')
  thicken_cust_1 <- thicken_cust(coffee, spanned_asym, "jos")$jos
  spanned_1 <- spanned_asym[c(1, 1, 2, 2)]
  expect_equal(thicken_cust_1, spanned_1)

  expect_warning(thicken_cust(coffee, spanned_asym[-1], "jos"),
                 "Dropping all values in the datetime var that are smaller than smallest spanned")

  thicken_cust_2 <- sw(thicken_cust(coffee, spanned_asym[-1], "jos")$jos)
  spanned_2 <- spanned_asym[c(2, 2)]
  expect_equal(thicken_cust_2, spanned_2)
})

context("thicken_cust drop argument")
test_that("the drop argument gives the desired result", {
  attr(coffee$time_stamp, "tzone") <- "UTC"
  hourly <- ymd_h(c("20160707 07",
                    "20160707 07",
                    "20160709 11",
                    "20160710 08"), tz = "UTC")
  coffee_hour <- coffee %>% mutate(time_stamp_hour = hourly)
  no_drop <- coffee_hour
  with_drop <- coffee_hour %>% select(-time_stamp)

  expect_equal(sw(thicken_cust(coffee,
                               spanned = hourly,
                               colname = "time_stamp_hour")), no_drop)
  expect_equal(sw(thicken_cust(coffee,
                               spanned = hourly,
                               colname = "time_stamp_hour",
                               drop = FALSE)), no_drop)
  expect_equal(sw(thicken_cust(coffee,
                               spanned = hourly,
                               colname = "time_stamp_hour",
                               drop = TRUE)), with_drop)
})


context("informative error for Year 2038 problem - thicken_cust")
x <- data.frame(dt = ymd_h("20381201 01", "20381202 01"))
sp <- as.Date(c("2038-12-01", "2038-12-02", "2038-12-04"))
expect_error(thicken_cust(x, sp, "day"),
             "thicken_cust does not work on POSIX data after 2038, due to Year 2038 problem. https://en.wikipedia.org/wiki/Year_2038_problem")
