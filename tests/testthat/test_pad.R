source("library.R")

x_year  <- c(as.Date("2015-01-01"), as.Date("2018-01-01"))

x_month <- seq(as.Date("2015-01-01"), as.Date("2015-06-01"),
               by = "month")[c(1, 3, 4, 6)]

x_day   <- seq(as.Date("2015-01-01"), as.Date("2015-02-01"),
               by = "day") %>% sample(15) %>%
  c(as.Date("2015-01-01"), as.Date("2015-02-01")) %>% unique

x_hour  <- seq(
  as.POSIXct("2015-01-01 01:00:00"),
  as.POSIXct("2015-01-02 01:00:00"),
  by = "hour"
)[c(1, 25)]

x_min   <- seq(lubridate::ymd_hms("2015-01-01 00:00:00"),
               lubridate::ymd_hms("2015-01-01 00:59:00"), by = "min") %>%
  sample(15) %>%
  c(lubridate::ymd_hm("2015-01-01 00:00"),
    lubridate::ymd_hm("2015-01-01 00:59")) %>%
  unique


context("Test the pad function")

test_that("Correct error handling", {
  expect_error(pad(x_month %>% as.character))
  expect_error(pad(x_month %>% as.numeric))
  expect_error(pad(mtcars))
})

test_that("Gives warning when unordered", {
  expect_warning(pad(x_day %>% as.data.frame))
})

test_that("Pad works properly on data.table and tbl", {
  expect_equal(class(pad(data.table::data.table(x_year, 1)))[1], "data.table")
  expect_equal(class(pad(dplyr::data_frame(x_year, 1)))[1], "tbl_df")
})

test_that("check_start_end throws error", {
  expect_error( check_start_end(x_hour, as.POSIXct("2015-01-01 01:01:00"),
                                NULL, "hour" ))
  expect_error( check_start_end(x_hour, as.POSIXct("2015-01-01 01:01:00"),
                                NULL, "min" ), NA)
})

context("pad integration tests")
test_that("Pad gives correct results", {
  expect_equal(pad(data.frame(x_year, 1)) %>% nrow, 4)
  expect_equal(pad(data.frame(x_year, 1), end_val = as.Date("2021-01-01")) %>%
                 nrow, 7)
  expect_equal(pad(data.frame(x_year, 1), start_val = as.Date("2012-01-01")) %>%
                 nrow, 7)
  expect_equal(pad(data.frame(x_year, 1), interval = "month") %>% nrow, 37)
  expect_equal(pad(data.frame(x_month, 1)) %>% nrow, 6)
  expect_equal(suppressWarnings(pad(data.frame(x_day, 1))) %>% nrow, 32)
  expect_equal(pad(data.frame(x_hour, 1)) %>% nrow, 2)
  expect_equal(pad(data.frame(x_hour, 1), interval = "hour") %>% nrow, 25)
  expect_equal(suppressWarnings(pad(data.frame(x_min, 1))) %>% nrow, 60)
})
