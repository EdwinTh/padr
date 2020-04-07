source("library.R")

df <- data.frame(y = c(2006, 2008, 2012, 2007, 2008, 2011),
                 g = rep(LETTERS[1:2], each = 3),
                 v = sample(1:10, 6))

df2 <- data.frame(y = c(2006, 2010, 2006, 2010),
                  g = rep(LETTERS[1:2], each = 2),
                  v = sample(1:10, 4))

context("Testing the pad_int")

test_that("pad_int throws warnings with incorrect input", {
  df$year <- ymd(paste(df$y, '0101', sep = ''))
  expect_error(pad_int(df, 'g'))
  expect_error(pad_int(df,  'year'))
  expect_error(pad_int(df$y,  'year'))
})

test_that("pad_int without groups", {
  df_single <- df %>% filter(g == 'A')
  expect_equal( pad_int(df_single, 'y') %>% nrow, 7)
  expect_equal( pad_int(df_single, 'y', start_val = 2004) %>% nrow, 9)
  expect_equal( pad_int(df_single, 'y', start_val = 2007) %>% nrow, 6)
  expect_equal( pad_int(df_single, 'y', end_val = 2014) %>% nrow, 9)
  expect_equal( pad_int(df_single, 'y', end_val = 2011) %>% nrow, 6)
  expect_equal( pad_int(df_single, 'y', step = 2) %>% nrow, 4)
  expect_error( pad_int(df_single, 'y', step = 3))
})

test_that("pad_int with groups", {
  expect_equal( pad_int(df, 'y', group = 'g') %>% nrow, 12)
  expect_equal( pad_int(df, 'y', group = 'g', start_val = 2004) %>% nrow, 17)
  expect_equal( pad_int(df, 'y', group = 'g', start_val = 2007) %>% nrow, 11)
  expect_equal( pad_int(df, 'y', group = 'g', end_val = 2014) %>% nrow, 17)
  expect_equal( pad_int(df, 'y', group = 'g', end_val = 2011) %>% nrow, 11)
  expect_equal( pad_int(df2, 'y', group = 'g', step = 2) %>% nrow, 6)
  expect_error( pad_int(df2, 'y', group = 'g', step = 3) )
})

test_that("pad_int works on both tbl and data.table", {
  df_tbl <- dplyr::as_tibble(df)
  df_dt  <- data.table::as.data.table(df)
  expect_error(pad_int(df_tbl, "y"), NA)
  expect_error(pad_int(df_dt, "y"), NA)
  expect_true(inherits(pad_int(df_tbl, "y"), "tbl"))
  expect_true(inherits(pad_int(df_dt, "y"), "data.table"))
})
