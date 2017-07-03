source("library.R")
x <- coffee %>% thicken("day", "d") %>% count(d) %>% pad() %>% fill_by_value()

context("helper functions of format_date")
test_that("check_completeness_func works", {
  expect_error(check_completeness_func(x$d, "day"), NA)
  expect_error(check_completeness_func(x$d[-2], "day"))
})
