source("library.R")

df <- data.frame(y = c(2006, 2008, 2012, 2007, 2008, 2011),
                 g = rep(LETTERS[1:2], each = 3),
                 v = sample(1:10, 6))


context("Testing the pad_int")

test_that("pad_int throws warnings with incorrect input", {
  df$year <- ymd(paste(df$y, '0101', sep = ''))
  expect_error(pad_int(df, 'g'))
  expect_error(pad_int(df,  'year'))
  expect_error(pad_int(df$y,  'year'))
})

