source("library.R")

x_year  <- as.Date(c("2015-01-01", "2016-01-01", "2018-01-01"))

x_month <- seq(as.Date("2015-01-01"), as.Date("2015-06-01"),
               by = "month")[c(1, 3, 4, 6)]

x_day   <- seq(as.Date("2015-01-01"), as.Date("2015-02-01"),
               by = "day") %>% sample(15) %>%
  c(as.Date("2015-01-01"), as.Date("2015-02-01")) %>% unique

x_hour  <- seq(
  as.POSIXct("2015-01-01 01:00:00"),
  as.POSIXct("2015-01-03 01:00:00"),
  by = "hour"
)[c(1, 25, 49)]

x_min   <- seq(lubridate::ymd_hms("2015-01-01 00:00:00"),
               lubridate::ymd_hms("2015-01-01 00:59:00"), by = "min") %>%
  sample(15) %>%
  c(lubridate::ymd_hm("2015-01-01 00:00"),
    lubridate::ymd_hm("2015-01-01 00:59")) %>%
  unique

sw <- suppressWarnings

context("Test the pad function")

test_that("Correct error handling", {
  expect_error(pad(x_month %>% as.character))
  expect_error(pad(x_month %>% as.numeric))
  expect_error(pad(mtcars))
})


test_that("Pad works properly on data.table and tbl", {
  expect_equal(class(pad(data.table::data.table(x_year, 1)))[1], "data.table")
  expect_equal(class(pad(dplyr::data_frame(x_year, 1)))[1], "tbl_df")
})

context("pad gives correct output with one datetime value")

test_that('gives warning and same result when start_val and end_val are NULL', {
  x <- data.frame(tm = ymd(20160102))
  expect_warning(pad(x))
  suppressWarnings(expect_equal(pad(x), x))
})

test_that("break_above prevents large output", {
  large_df <- data.frame(x = ymd_h("2000-01-01 01", "2004-01-01 00"), y = 1:2)
  large_df_grp <- rbind(large_df, large_df)
  large_df_grp$grp <-  rep(letters[1:2], each = 2)
  expect_error( pad(large_df, interval = "min") )
  expect_error( pad(large_df_grp, interval = "min", group = "grp") )
  expect_error( nrow(pad(large_df, interval = "hour", break_above = 0.035)))
  expect_error( nrow(pad(large_df_grp, interval = "hour", break_above = 0.07, group = "grp")))
  expect_equal( nrow(pad(large_df, interval = "hour", break_above = 0.0351)), 35064)
  expect_equal( nrow(pad(large_df_grp, interval = "hour", group = "grp",
                         break_above = 0.0702)), 70128)

})

test_that('gives correct output when end_val and/or start_val are specified, date', {
  x <- data.frame(tm = ymd(20160102))
  expect_equal(pad(x, start_val = ymd(20160101))$tm, c(ymd(20160101), x$tm))
  expect_equal(pad(x, end_val = ymd(20160104), interval = "day")$tm,
               c(x$tm, ymd(20160103), ymd(20160104)))
  expect_equal(pad(x, start_val = ymd(20160101), end_val = ymd(20160104))$tm,
               seq(ymd(20160101), by = 'day', length.out = 4))
})

test_that("pad gives informative error when start_val or end_val is of wrong class", {
  x <- data.frame(tm = ymd(20160102, 20160103))
  expect_error(pad(x, start_val = "20160101"),
               "start_val should be of class Date, POSIXlt, or POSIXct")
  expect_error(pad(x, end_val = "20160101"),
               "end_val should be of class Date, POSIXlt, or POSIXct")
})

test_that('gives correct output when end_val and/or start_val are specified, posix', {
  x <- data.frame(tm = ymd_h('20160102 16'))
  s_val <- ymd_h('20160101 16')
  e_val <- ymd_h('20160104 16')
  compare <- seq(s_val, by = 'day', length.out = 4)
  expect_equal(pad(x, start_val = s_val)$tm, compare[1:2])
  expect_equal(pad(x, end_val = e_val, interval = "day")$tm, compare[2:4])
  expect_equal(pad(x, start_val = s_val, end_val = e_val)$tm, compare)
})

test_that("gives correct output when start or end with datetime range", {
  x <- data.frame(dt = ymd(20170101, 20180101), x = 1:2)
  expect_equal(pad(x, start_val = ymd(20170201))$dt,
               seq(ymd(20170201), ymd(20180101), by = "month"))
  expect_equal(pad(x, end_val = ymd(20170601))$dt,
               seq(ymd(20170101), ymd(20170601), by = "month"))

  x_grp <- data.frame(dt = ymd(20170101, 20170601, 20170201, 20170501),
                      id = c(1, 1, 2, 2),
                      x  = 1:4)
  expect_equal(pad(x_grp, group = "id", start_val = ymd(20170401))$dt,
               ymd(20170401, 20170501, 20170601, 20170401, 20170501))
  expect_equal(pad(x_grp, group = "id", end_val = ymd(20170301))$dt,
               ymd(20170101, 20170201, 20170301, 20170201, 20170301))
})

context("pad_single and pad_multiple, addtions to padr")

test_that("pad gives correct output, with no groups", {
  mnths <- seq(ymd(20160101), length.out = 6, by = 'month')
  x <- data.frame(m = mnths[c(2, 4, 5)])
  expect_equal( pad(x, "month")$m, mnths[2:5])
  expect_equal( pad(x, start_val = mnths[1])$m, mnths[1:5])
  expect_equal( pad(x, end_val = mnths[6])$m, mnths[2:6])
})


test_that("pad_multiple pads correctly with one group var", {
  mnths <- seq(ymd(20160101), length.out = 6, by = 'month')
  x <- data.frame(m = rep( mnths[c(2, 4, 5)], 2), g = letters[c(1, 1, 1, 2, 2, 2)])
  expect_equal( pad(x, group = 'g', interval = "month")$m, rep(mnths[2:5], 2) )
  expect_equal( sw(pad(x, group = 'g', start_val = mnths[1]))$m, rep(mnths[1:5], 2) )
  expect_equal( sw(pad(x, group = 'g', end_val = mnths[6]))$m, rep(mnths[2:6], 2) )
})

test_that("pad pads correctly with two group vars", {
  mnths <- seq(ymd(20160101), length.out = 6, by = 'month')
  x <- data.frame(m  = rep( mnths[c(2, 4, 5)], 4),
                  g1 = letters[rep(1:2, each = 6)],
                  g2 = letters[rep(5:8, each = 3)])
  expect_equal( pad(x, group = c('g1', 'g2'), interval = "months")$m, rep(mnths[2:5], 4) )
  expect_equal( sw(pad(x, group = c('g1', 'g2'), start_val = mnths[1]))$m,
                   rep(mnths[1:5], 4) )
  expect_equal( sw(pad(x, group = c('g1', 'g2'), end_val = mnths[6]))$m,
                   rep(mnths[2:6], 4) )
})

test_that("dplyr grouping yields correct results", {
  mnths <- seq(ymd(20160101), length.out = 6, by = 'month')
  x <- data.frame(m  = rep( mnths[c(2, 4, 5)], 4),
                  g1 = letters[rep(1:2, each = 6)],
                  g2 = letters[rep(5:8, each = 3)])
  expect_equal(pad(x, group = "g1"),
               pad(dplyr::group_by(x, g1)) %>% as.data.frame )
  expect_equal(pad(x, group = c("g1", "g2")),
               pad(dplyr::group_by(x, g1, g2)) %>% as.data.frame)
  expect_warning(pad(group_by(x, g2), group = "g1"))
  expect_equal( sw(pad(group_by(x, g2), group = "g1")) %>% as.data.frame,
                pad(x, group = "g1"))
  expect_equal( pad(group_by(x, g1)) %>% groups %>% as.character, "g1")

})

test_that("the by arguments works, both in pad and pad_single", {
  one_var <- data.frame(x_year = x_year, val = 1)
  two_var <- one_var; two_var$x_year2 <- two_var$x_year
  one_var_grps <- rbind(one_var, one_var)
  one_var_grps$grp <- rep(letters[1:2], each = 3)
  two_var_grps <- rbind(two_var, two_var)
  two_var_grps$grp <- rep(letters[1:2], each = 3)
  check_val <- seq( ymd(20150101), length.out = 4, by = 'year')

  expect_equal( pad(one_var, by = "x_year", interval = "year")$x_year, check_val)
  expect_equal( pad(one_var_grps, by = "x_year", group = 'grp',  interval = "year")$x_year,
                rep(check_val, 2) )
  expect_equal( pad(two_var, "year", by = "x_year")$x_year, check_val)
  expect_equal( pad(two_var_grps, "year", by = "x_year", group = 'grp')$x_year,
                rep(check_val, 2) )
})

test_that("pad works correclty when start is after or end is before range", {
  x_grp <- data.frame(x   = as.Date(c("2017-04-01", "2017-07-01",
                                      "2017-09-01", "2017-11-01")),
                      grp = rep(letters[1:2], each = 2),
                      y   = 1:4)
  x_sing_1 <- x_grp[1:2, ]
  x_sing_2 <- x_grp[3:4, ]
  expect_error(pad(x_sing_1, start_val = as.Date("2017-08-01")),
               "start value is larger than the end value.")
  expect_error(pad(x_sing_1, end_val = as.Date("2017-03-01")),
               "start value is larger than the end value.")
  expect_warning(pad(x_grp, group = "grp", start_val = as.Date("2017-08-01")))
  expect_warning(pad(x_grp, group = "grp", end_val = as.Date("2017-08-01")))

  x_sing_1_pad <- pad(x_sing_1, group = "grp", interval = "month")
  x_start_val <- sw(pad(x_grp, group = "grp", end_val = as.Date("2017-07-01"),
                        interval = "month"))
  expect_equal(x_sing_1_pad, x_start_val)

  x_sing_2_pad <- pad(x_sing_2, group = "grp", interval = "month")
  x_end_val <- sw(pad(x_grp, group = "grp", start_val = as.Date("2017-09-01"),
                      interval = "month"))
  expect_equal(x_sing_2_pad, x_end_val)
})

context("pad integration tests")
test_that("Pad gives correct results", {
  expect_equal(pad(data.frame(x_year, 1), "year") %>% nrow, 4)
  expect_equal(pad(data.frame(x_year, 1), "year", end_val = as.Date("2021-01-01")) %>%
                 nrow, 7)
  expect_equal(pad(data.frame(x_year, 1), "year", start_val = as.Date("2012-01-01")) %>%
                 nrow, 7)
  expect_equal(pad(data.frame(x_year, 1), interval = "month") %>% nrow, 37)
  expect_equal(pad(data.frame(x_month, 1)) %>% nrow, 6)
  expect_equal(suppressWarnings(pad(data.frame(x_day, 1))) %>% nrow, 32)
  expect_equal(pad(data.frame(x_hour, 1)) %>% nrow, 3)
  expect_equal(pad(data.frame(x_hour, 1), interval = "hour") %>% nrow, 49)
  expect_equal(suppressWarnings(pad(data.frame(x_min, 1))) %>% nrow, 60)
})
