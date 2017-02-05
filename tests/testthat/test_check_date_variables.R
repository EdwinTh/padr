source("library.R")
no_dt <- data.frame(x = 1:2, y = c("A", "B"))
one_dt <- data.frame(x = 1:2, y = c("A", "B"), dt1 = ymd(c(20160401, 20160803)))
two_dt <- data.frame(x = 1:2, y = c("A", "B"), dt1 = ymd(c(20160401, 20160803)),
                     dt2 = ymd_h(c("20160401 01", "20160803 05")))

context("check_date_variables does what is expecterd")
test_that("check_date_variables works correctly when by is specified", {
  expect_error( check_data_frame( no_dt, by = "dt3") )
  expect_error( check_data_frame( one_dt, by = "dt3") )
  expect_error( check_data_frame( two_dt, by = "dt3") )
  expect_equal( check_data_frame( one_dt, by = "dt1"),
                ymd(c(20160401, 20160803)))
  expect_equal( check_data_frame( two_dt, by = "dt1"),
                ymd(c(20160401, 20160803)))
})

test_that("check_date_variables works correctly when by is not specified", {
  expect_equal( check_data_frame( one_dt), ymd(c(20160401, 20160803)))
  expect_error( check_data_frame( no_dt) )
  expect_error( check_data_frame( two_dt) )
})
