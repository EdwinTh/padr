context("round_down_core and round_up_core cpp functions")

a <- c(2, 5, 6, 9)
b <- c(1, 5, 9, 13)

test_that("round_down_core will assign ties to current", {
  expect_equal(round_down_core(a, b),
               c(1, 5, 5, 9))
})

test_that("round_down_core will assign ties to previous", {
  expect_equal(round_down_core_prev(a, b),
               c(1, 1, 5, 5))
})

test_that("round_up_core will assign ties to next", {
  expect_equal(round_up_core(a, b),
               c(5, 9, 9, 13))
})

test_that("round_up_core will assign ties to current", {
  expect_equal(round_up_core_prev(a, b),
               c(5, 5, 9, 9))
})


