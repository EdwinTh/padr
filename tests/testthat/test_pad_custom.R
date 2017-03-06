source("library.R")

a <- data.frame(a = seq(ymd(20160101), ymd(20160131), by = "30 d"),
                v = 1)

# These tests are only pad_custom specific, since this code will later be
# refactored and merged with the ordinary pad function.

context("pad_custom unit tests")

test_that("pad_custom does not work with wrong intervals", {
  expect_error(pad_custom(a, "4 days"))
  expect_error(pad_custom(a, "12 days"))
  expect_error(pad_custom(a, "2 months"))
  expect_error(pad_custom(a, "13 mins"))
  expect_error(pad_custom(a, "5 weeks"))
})

test_that("pad gives desired output", {
  expect_equal(pad_custom(a, interval = "2 days") %>% nrow, 16)
  expect_equal(pad_custom(a, interval = "6 hours") %>% nrow, 121)
  expect_equal(pad_custom(a, interval = "10 mins") %>% nrow, 4321)
})

test_that("check_needs_posix gives correct output", {
  expect_false(check_needs_posix('2 days'))
  expect_false(check_needs_posix('7 weeks'))
  expect_false(check_needs_posix('5 months'))
  expect_true(check_needs_posix('9 hours'))
  expect_true(check_needs_posix('15 mins'))
  expect_true(check_needs_posix('158 secs'))
})
