source("library.R")

x_df <- tibble(x = as.Date(c("2019-01-01", "2019-01-04", "2019-01-06")),
               y1 = c(3, 5, 6),
               y2 = c(11, 14, 14),
               y3 = rep(10, 3),
               y4 = c("A", "A", "B")) %>% pad()

context("Test the fill functions")

test_that("fill_ functions break with wrong input", {
  expect_error(x_df %>% as.list %>% fill_by_value(y1))
  expect_error(x_df %>% as.list %>% fill_by_function(y1))
  expect_error(x_df %>% as.list %>% fill_by_prevalent(y1))
  expect_error(x_df$y1 %>% as.list %>% fill_by_value(y1))
  expect_error(x_df$y1 %>% as.list %>% fill_by_function(y1))
  expect_error(x_df$y1 %>% as.list %>% fill_by_prevalent(y1))
  expect_error(x_df %>% fill_by_value(y1), NA)
  expect_error(x_df %>% fill_by_function(y1), NA)
  expect_error(x_df %>% fill_by_prevalent(y4), NA)
})

test_that("fill_by_value gives expected outcomes", {
  expect_equal( fill_by_value(x_df, y1)$y1[2], 0)
  expect_equal( fill_by_value(x_df, y1, y2)$y1[2], 0)
  expect_equal( fill_by_value(x_df, y1, y2)$y2[2], 0)
  expect_equal( fill_by_value(x_df, y1, value = 42)$y1[2], 42)
  expect_equal( fill_by_value(x_df, y1, y2, value = 42)$y1[2], 42)
  expect_equal( fill_by_value(x_df, y1, y2, value = 42)$y2[2], 42)
})

test_that("fill_by_function gives expected outcomes", {
  expect_error( fill_by_function(x_df, y1, fun = y2) )
  expect_equal( fill_by_function(x_df, y1)$y1 %>% median, 4 + 2 / 3)
  expect_equal( fill_by_function(x_df, y1, y2)$y1 %>% median, 4 + 2 / 3)
  expect_equal( fill_by_function(x_df, y1, y2)$y2 %>% median, 13)
  expect_equal( fill_by_function(x_df, y1, fun = median)$y1 %>% median, 5)
  expect_equal( fill_by_function(x_df, y1, y2, fun = median)$y1 %>% median, 5)
})

unname <- function(x) {
  names(x) <- NULL
  x
}

test_that("fill_by_prevalent gives expected outcomes", {
  expect_equal( unname(fill_by_prevalent(x_df, y4)$y4[2]), "A")
  expect_equal( unname(fill_by_prevalent(x_df, y3)$y3[2]), 10)
  expect_equal( unname(fill_by_prevalent(x_df, y3, y4)$y4[2]), "A")
  expect_equal( unname(fill_by_prevalent(x_df, y3, y4)$y3[2]), 10)
  expect_error( unname(fill_by_prevalent(x_df, y1)))
})

test_that("get_the_inds works properly", {
  x <- suppressWarnings(coffee %>% thicken('day') %>% group_by(time_stamp_day) %>%
    summarise(a = sum(amount)) %>% pad)
  x$b <- NA
  no_cols <- 2:3
  names(no_cols) <- c("a", "b")
  expect_equal(get_the_inds(x), no_cols)
  expect_equal(get_the_inds(x, a), 2)
  expect_equal(get_the_inds(x, a, b), 2:3)
})

