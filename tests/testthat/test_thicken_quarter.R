# This script tests thicken with x having a quarter interval.

library(dplyr)
library(lubridate)
library(testthat)

start_val <- ymd_hms('20160101 010101'); end_val <- ymd_hms('20170101 010101')
start_day <- as.Date(start_val); end_day <- as.Date(end_val)

df_quarter <- data_frame(datetime = seq(start_day, end_day, by = 'quarter'))
df_quarter_dt <- data_frame(datetime = seq(start_val, end_val, by = 'quarter'))



context("Thicken quarter to year gives correct result, x = date")

test_that('Thicken quarter to year,  as is, no offset', {
  expect_equal(thicken(df_quarter)[,2],
               ymd(c(rep(20160101, 4), 20170101)))
  expect_equal(thicken(df_quarter, rounding = 'up')[,2],
               ymd(c(rep(20170101, 4), 20180101)))
})


test_that('Thicken quarter to year, as is, date offset', {
  expect_equal(thicken(df_quarter)[,2], start_val = ymd(20151231),
               ymd(c(rep(20151231, 4), 20161231)))
  expect_equal(thicken(df_quarter, rounding = 'up', start_val = ymd(20151231))[,2],
               ymd(c(rep(20161231, 4), 20171231)))
})
