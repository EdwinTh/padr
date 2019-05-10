context("round_down_core and round_up_core cpp functions")

a <- c(2, 5, 6, 8)
b <- c(1, 5, 9)

test_that("default behavior of round_down_core is to current", {
  expect_equal(round_down_core(a, b, to_current = TRUE),
               c(1, 5, 5, 5))
})

test_that("alternative behavior of round_down_core is to previous", {
  expect_equal(round_down_core(a, b, to_current = FALSE),
               c(1, 1, 5, 5))
})

test_that("default behavior of round_up_core is to next", {
  expect_equal(round_up_core(a, b, to_next = TRUE),
               c(5, 9, 9, 9))
})

test_that("alternative behavior of round_up_core is to current", {
  expect_equal(round_up_core(a, b, to_next = FALSE),
               c(5, 5, 9, 9))
})


