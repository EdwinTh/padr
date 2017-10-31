# source("library.R")
# context("thicken_whole_unit engine of thicken")
#
# test_that("the eight intervals give the correct output down", {
#   x <- coffee$time_stamp
#   x <- ymd_hms(as.character(x))
#   expect_equal(whole_unit_thickening(x, "year", NULL, "down"),
#                rep(ymd(20160101), 4))
#   expect_equal(whole_unit_thickening(x, "quarter", NULL, "down"),
#                rep(ymd(20160701), 4))
#   expect_equal(whole_unit_thickening(x, "month", NULL, "down"),
#                rep(ymd(20160701), 4))
#   expect_equal(whole_unit_thickening(x, "week", NULL, "down"),
#                ymd(c(rep(20160703, 3), 20160710)))
#   expect_equal(whole_unit_thickening(x, "day", NULL, "down"),
#                ymd(c(20160707, 20160707, 20160709, 20160710)))
#   expect_equal(whole_unit_thickening(x, "hour", NULL, "down"),
#                ymd_h(c("2016-07-07 09", "2016-07-07 09", "2016-07-09 13",
#                        "2016-07-10 10")))
#   expect_equal(whole_unit_thickening(x, "minute", NULL, "down"),
#                ymd_hm(c("2016-07-07 09:11", "2016-07-07 09:46",
#                         "2016-07-09 13:25", "2016-07-10 10:45")))
# })
#
# test_that("the eight intervals give the correct output up and start_val", {
#   x <- coffee$time_stamp
#   x <- ymd_hms(as.character(x))
#   s <-
#   expect_equal(whole_unit_thickening(x, "year", ymd(20160201), "up"),
#                rep(ymd(20170201), 4))
#   expect_equal(whole_unit_thickening(x, "quarter", NULL, "down"),
#                rep(ymd(20160701), 4))
#   expect_equal(whole_unit_thickening(x, "month", NULL, "down"),
#                rep(ymd(20160701), 4))
#   expect_equal(whole_unit_thickening(x, "week", NULL, "down"),
#                ymd(c(rep(20160703, 3), 20160710)))
#   expect_equal(whole_unit_thickening(x, "day", NULL, "down"),
#                ymd(c(20160707, 20160707, 20160709, 20160710)))
#   expect_equal(whole_unit_thickening(x, "hour", NULL, "down"),
#                ymd_h(c("2016-07-07 09", "2016-07-07 09", "2016-07-09 13",
#                        "2016-07-10 10")))
#   expect_equal(whole_unit_thickening(x, "minute", NULL, "down"),
#                ymd_hm(c("2016-07-07 09:11", "2016-07-07 09:46",
#                         "2016-07-09 13:25", "2016-07-10 10:45")))
# })
