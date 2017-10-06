source("library.R")

#-----------------------------------------------------------------------------#
# get_date_variables

context("Test the get_date_variable function")

no_dt_var <- mtcars
one_day_var <- data.frame(x = ymd(20160201, 20160301), y = c(1, 2))
one_dt_var <- data.frame(x = ymd_h("20160201 02", "20160301 03"), y = c(1, 2))
two_dt_var <- data.frame(x = ymd_h("20160201 02", "20160301 03"), y = c(1, 2),
                         x2 = ymd(20160201, 20160301))

test_that("get_date_variable only works on the right data types", {
  expect_error(get_date_variables(one_day_var %>% as.matrix))
  expect_error(get_date_variables(one_day_var$x))
})

test_that("get_date_variable gives the correct output", {
  expect_equal(length(get_date_variables(no_dt_var)), 0)
  expect_equal(get_date_variables(one_day_var), "x")
  expect_equal(get_date_variables(one_dt_var), "x")
  expect_equal(get_date_variables(two_dt_var), c("x", "x2"))
})

#-----------------------------------------------------------------------------#

# enforce_time_zone
context("Test the enforce_time_zone")

test_that("enforce_timee zone works as expected", {

  a_ct <- ymd_h(c("20151201 03", "20160201 03"))
  b_ct <- ymd_hms(c("2015-01-01 00:00:00", "2016-01-01 00:00:00"))
  c_ct <- ymd_hms(c("2015-01-01 00:00:00", "2016-01-01 00:00:00"), tz = "CET")

  equal  <- enforce_time_zone(a_ct, b_ct)
  expect_warning({
    different <- enforce_time_zone(a_ct, c_ct)
  })

  expect_warning( enforce_time_zone(a_ct, c_ct))
  expect_equal( attr(equal, "tz"), "UTC")
  expect_equal( as.character(equal),
                c("2015-12-01 03:00:00", "2016-02-01 03:00:00"))
  expect_equal( attr(different, "tz"), "CET")
  expect_equal( as.character(different),
                c("2015-12-01 03:00:00", "2016-02-01 03:00:00"))
})
