source("library.R")

coffee$time_stamp <- ymd_hms(coffee$time_stamp, tz = "")
coffee_2_dts <- coffee
coffee_2_dts$ts2 <- coffee_2_dts$time_stamp

context("Check all argument combinations in thicken")

test_that("interval: year", {
  coffee_year    <- coffee
  year(coffee_year$time_stamp)[3:4] <- 2017:2018
  coffee_year_2_dts <- coffee_year
  coffee_year_2_dts$ts2 <- coffee_year_2_dts$time_stamp
  year_thickened_down <- ymd(c(20160101, 20160101, 20170101, 20180101))
  year_thickened_up   <- ymd(c(20170101, 20170101, 20180101, 20190101))

  s <- ymd(20151231)

  # plain
  expect_equal(thicken(coffee_year, "year")$time_stamp_year,
               year_thickened_down)
  # specify colname
  expect_equal(thicken(coffee_year, "year", "y")$y,
               year_thickened_down)
  # rounding up
  expect_equal(thicken(coffee_year, "year", rounding = "up")$time_stamp_year,
               year_thickened_up)
  # specify by
  expect_equal(thicken(coffee_year_2_dts, "year", by = "ts2")$ts2_year,
               year_thickened_down)
  # start_val
  expect_equal(thicken(coffee_year, "year", start_val = s)$time_stamp_year,
               year_thickened_down - 1)
  # interval abbreviation
  expect_equal(thicken(coffee_year, "y")$time_stamp_year,
               year_thickened_down)
  # specify by rounding up
  expect_equal(thicken(coffee_year_2_dts, "year", rounding = "up", by = "ts2")$ts2_year,
               year_thickened_up)
  # specify by specify colname
  expect_equal(thicken(coffee_year_2_dts, "year", colname = "t", by = "ts2")$t,
               year_thickened_down)
  # specify by start_val
  expect_equal(thicken(coffee_year_2_dts, "year", by = "ts2", start_val = s)$ts2_year,
               year_thickened_down - 1)
  # specify by interval abbreviation
  expect_equal(thicken(coffee_year_2_dts, "y", by = "ts2", start_val = s)$ts2_year,
               year_thickened_down - 1)
  # rounding up     specify colname
  expect_equal(thicken(coffee_year, "year", "jos", rounding = "up")$jos,
               year_thickened_up)
  # rounding up     start_val
  expect_equal(thicken(coffee_year, "year", rounding = "up", start_val = s)$time_stamp_year,
               year_thickened_up - 1)
  # rounding up     interval abbreviation
  expect_equal(thicken(coffee_year, "y", rounding = "up")$time_stamp_year,
               year_thickened_up)
  # specify colname start_val
  expect_equal(thicken(coffee_year, "year", "jos", start_val = s)$jos,
               year_thickened_down - 1)
  # specify colname interval abbreviation
  expect_equal(thicken(coffee_year, "y", "jos")$jos,
               year_thickened_down)
  # start_val       interval abbreviation
  expect_equal(thicken(coffee_year, "y", start_val = s)$time_stamp_year,
               year_thickened_down - 1)
})

test_that("interval: month", {
  coffee_month    <- coffee
  month(coffee_month$time_stamp)[3:4] <- 8:9
  coffee_month_2_dts <- coffee_month
  coffee_month_2_dts$ts2 <- coffee_month_2_dts$time_stamp
  month_thickened_down <- ymd(c(20160701, 20160701, 20160801, 20160901))
  month_thickened_up   <- ymd(c(20160801, 20160801, 20160901, 20161001))

  s <- ymd(20160630)

  # plain
  expect_equal(thicken(coffee_month, "month")$time_stamp_month,
               month_thickened_down)
  # specify colname
  expect_equal(thicken(coffee_month, "month", "m")$m,
               month_thickened_down)
  # rounding up
  expect_equal(thicken(coffee_month, "month", rounding = "up")$time_stamp_month,
               month_thickened_up)
  # specify by
  expect_equal(thicken(coffee_month_2_dts, "month", by = "ts2")$ts2_month,
               month_thickened_down)
  # start_val
  expect_equal(thicken(coffee_month, "month", start_val = s)$time_stamp_month,
               month_thickened_down - c(1, 1, 2, 2))
  # interval abbreviation
  expect_equal(thicken(coffee_month, "mo")$time_stamp_month,
               month_thickened_down)
  # specify by rounding up
  expect_equal(thicken(coffee_month_2_dts, "month", rounding = "up", by = "ts2")$ts2_month,
               month_thickened_up)
  # specify by specify colname
  expect_equal(thicken(coffee_month_2_dts, "month", colname = "t", by = "ts2")$t,
               month_thickened_down)
  # specify by start_val
  expect_equal(thicken(coffee_month_2_dts, "month", by = "ts2", start_val = s)$ts2_month,
               month_thickened_down - c(1, 1, 2, 2))
  # specify by interval abbreviation
  expect_equal(thicken(coffee_month_2_dts, "mo", by = "ts2")$ts2_month,
               month_thickened_down)
  # rounding up     specify colname
  expect_equal(thicken(coffee_month, "month", "jos", rounding = "up")$jos,
               month_thickened_up)
  # rounding up     start_val
  expect_equal(thicken(coffee_month, "month", rounding = "up", start_val = s)$time_stamp_month,
               month_thickened_up - c(2, 2, 2, 1))
  # rounding up     interval abbreviation
  expect_equal(thicken(coffee_month, "mo", rounding = "up")$time_stamp_month,
               month_thickened_up)
  # specify colname start_val
  expect_equal(thicken(coffee_month, "month", "jos", start_val = s)$jos,
               month_thickened_down - c(1, 1, 2, 2))
  # specify colname interval abbreviation
  expect_equal(thicken(coffee_month, "mo", "jos")$jos,
               month_thickened_down)
  # start_val       interval abbreviation
  expect_equal(thicken(coffee_month, "mo", start_val = s)$time_stamp_month,
               month_thickened_down - c(1, 1, 2, 2))
})

test_that("interval: day", {
  coffee_day       <- coffee
  coffee_day_2_dts <- coffee_day
  coffee_day_2_dts$ts2 <- coffee_day_2_dts$time_stamp
  day_thickened_down <- ymd(c(20160707, 20160707, 20160709, 20160710))
  day_thickened_down_s <- ymd_h(c("20160706 23", "20160706 23",
                                  "20160708 23", "20160709 23"), tz = "")
  day_thickened_up   <- ymd(c(20160708, 20160708, 20160710, 20160711))

  s <- ymd_h("20160706 23")

  # plain
  expect_equal(thicken(coffee_day, "day")$time_stamp_day,
               day_thickened_down)
  # specify colname
  expect_equal(thicken(coffee_day, "day", "m")$m,
               day_thickened_down)
  # rounding up
  expect_equal(thicken(coffee_day, "day", rounding = "up")$time_stamp_day,
               day_thickened_up)
  # specify by
  expect_equal(thicken(coffee_day_2_dts, "day", by = "ts2")$ts2_day,
               day_thickened_down)
  # start_val
  expect_equal(suppressWarnings(thicken(coffee_day, "day", start_val = s))$time_stamp_day,
               day_thickened_down_s)
  # interval abbreviation
  expect_equal(thicken(coffee_day, "d")$time_stamp_day,
               day_thickened_down)
  # specify by rounding up
  expect_equal(thicken(coffee_day_2_dts, "day", rounding = "up", by = "ts2")$ts2_day,
               day_thickened_up)
  # specify by specify colname
  expect_equal(thicken(coffee_day_2_dts, "day", colname = "t", by = "ts2")$t,
               day_thickened_down)
  # specify by start_val
  expect_equal(suppressWarnings(thicken(coffee_day_2_dts, "day",
                                        by = "ts2", start_val = s))$ts2_day,
               day_thickened_down_s)
  # specify by interval abbreviation
  expect_equal(thicken(coffee_day_2_dts, "d", by = "ts2")$ts2_day,
               day_thickened_down)
  # rounding up     specify colname
  expect_equal(thicken(coffee_day, "day", "jos", rounding = "up")$jos,
               day_thickened_up)
  # rounding up     start_val
  expect_equal(suppressWarnings(thicken(coffee_day, "day", rounding = "up",
                                        start_val = s))$time_stamp_day,
               day_thickened_down_s + 3600 * 24)
  # rounding up     interval abbreviation
  expect_equal(thicken(coffee_day, "d", rounding = "up")$time_stamp_day,
               day_thickened_up)
  # specify colname start_val
  expect_equal(suppressWarnings(thicken(coffee_day, "day", "jos",
                                        start_val = s))$jos,
               day_thickened_down_s)
  # specify colname interval abbreviation
  expect_equal(thicken(coffee_day, "d", "jos")$jos,
               day_thickened_down)
  # start_val       interval abbreviation
  expect_equal(suppressWarnings(thicken(coffee_day, "d",
                                        start_val = s))$time_stamp_day,
               day_thickened_down_s)
})

test_that("interval: hour", {
  hour_thickened <- ymd_h(c("2016-07-07 09", "2016-07-07 09",
                            "2016-07-09 13", "2016-07-10 10"), tz = "")
  s <- ymd_hm("2016-07-07 8:59", tz = "")
  # plain
  expect_equal(thicken(coffee, "hour")$time_stamp_hour,
               hour_thickened)
  # specify colname
  expect_equal(thicken(coffee, "hour", "h")$h,
               hour_thickened)
  # rounding up
  expect_equal(thicken(coffee, "hour", rounding = "up")$time_stamp_hour,
               hour_thickened + 3600)
  # specify by
  expect_equal(thicken(coffee_2_dts, "hour", by = "ts2")$ts2_hour,
               hour_thickened)
  # start_val
  s <- ymd_hm("2016-07-07 8:59", tz = "")
  expect_equal(thicken(coffee, "hour", start_val = s)$time_stamp_hour,
               hour_thickened - 60)
  # interval abbreviation
  expect_equal(thicken(coffee, "h")$time_stamp_hour,
               hour_thickened)
  # specify by rounding up
  expect_equal(thicken(coffee_2_dts, "hour", rounding = "up", by = "ts2")$ts2_hour,
               hour_thickened + 3600)
  # specify by specify colname
  expect_equal(thicken(coffee_2_dts, "hour", colname = "t", by = "ts2")$t,
               hour_thickened)
  # specify by start_val
  expect_equal(thicken(coffee_2_dts, "hour", by = "ts2", start_val = s)$ts2_hour,
               hour_thickened - 60)

  # specify by interval abbreviation
  expect_equal(thicken(coffee_2_dts, "h", by = "ts2", start_val = s)$ts2_hour,
               hour_thickened - 60)
  # rounding up     specify colname
  expect_equal(thicken(coffee, "hour", "jos", rounding = "up")$jos,
               hour_thickened + 3600)
  # rounding up     start_val
  expect_equal(thicken(coffee, "hour", rounding = "up", start_val = s)$time_stamp_hour,
               hour_thickened + 3540)
  # rounding up     interval abbreviation
  expect_equal(thicken(coffee, "h", rounding = "up")$time_stamp_hour,
               hour_thickened + 3600)
  # specify colname start_val
  expect_equal(thicken(coffee, "hour", "jos", start_val = s)$jos,
               hour_thickened - 60)
  # specify colname interval abbreviation
  expect_equal(thicken(coffee, "h", "jos")$jos,
               hour_thickened)
  # start_val       interval abbreviation
  expect_equal(thicken(coffee, "h", start_val = s)$time_stamp_hour,
               hour_thickened - 60)
})
