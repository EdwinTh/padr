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

spanned_asym <- ymd_h(c("2016-07-07 09", "2016-07-09 12", "2016-07-11 00"),
                      tz = "")
sw <- suppressWarnings

test_that("thicken_cust works properly", {
  # the cumbersome setting of tz's is needed for passing the test on systems
  # with a different local than CET
  thicken_cust_1 <- thicken_cust(coffee, spanned_asym, "jos")$jos
  attr(thicken_cust_1, "tz") <- "CET"
  spanned_1 <- spanned_asym[c(1, 1, 2, 2)]
  attr(spanned_1, "tz") <- "CET"
  expect_equal(thicken_cust_1, spanned_1)

  expect_warning(thicken_cust(coffee, spanned_asym[-1], "jos"),
                 "Dropping all values in the datetime var that are smaller than smallest spanned")

  thicken_cust_2 <- sw(thicken_cust(coffee, spanned_asym[-1], "jos")$jos)
  attr(thicken_cust_2, "tz") <- "CET"
  spanned_2 <- spanned_asym[c(2, 2)]
  attr(spanned_2, "tz") <- "CET"
  expect_equal(thicken_cust_2, spanned_2)
})
