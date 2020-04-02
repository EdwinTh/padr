source("library.R")

date_seq <- function(interval){
  set.seed(1234)
  # only use a wide interval to test year, all others less wide for performance
  if (interval == "year") {
    start_date <- as.POSIXlt(strftime("2005-01-01"))
  } else {
    start_date <- as.POSIXlt(strftime("2015-01-01"))
  }

  sequence <- seq(start_date,
                  as.POSIXlt(strftime("2017-01-01")),
                  by = interval)

  set.seed(12345)
  if (length(sequence) > 100) {
    sampled_dates <- sample(sequence, 100)
  } else {
    sampled_dates <- sample(sequence, length(sequence) / 2)
  }
  return(sampled_dates)
}

x_month <- date_seq("month")
x_day   <- date_seq("DSTday")
x_hour  <- date_seq("hour")
x_min   <- date_seq("min")
x_sec   <- date_seq("sec")
equal_dist <- c(as.POSIXct("2014-01-01 23:00:00"),
                as.POSIXct("2014-01-02 01:00:00"))

df_with_one_date  <- data.frame(dt_var1 = date_seq("month"),
                                y = 1:6)
df_with_one_date_sorted <- df_with_one_date %>% arrange(dt_var1)
df_with_two_dates <- data.frame(dt_var1  = date_seq("month"),
                                dt_var2 = date_seq("month"),
                                y = 1:6)
x_month <- data.frame(x = ymd(c(20160201, 20160301)))
x_month_unordered  <- data.frame(x = ymd(c(20160301, 20160201)))
sw <- suppressWarnings

context("thicken function errors and warnings")

test_that("thicken only accepts data frames", {
  expect_error(thicken(x_month %>% as.character))
  expect_error(thicken(x_month %>% as.numeric))
  expect_error(suppressWarnings(thicken(df_with_one_date, interval = "quarter")), NA)
})

test_that("thicken throws error when asked interval is lower", {
  expect_error( thicken(x_month, interval = "month"))
  expect_error( thicken(x_month, interval = "day"))
  expect_error( thicken(x_month, interval = "year"), NA)
})

test_that("thicken gives informed error when start_val is wrong class", {
  expect_error(thicken(x_month, start_val = "2017-01-01",
               "start_val should be of class Date, POSIXlt, or POSIXct"))
})

test_that("thicken removes when start_val is larger than min(dt)", {
  x <- data.frame(dt = as.Date(c("2016-01-01", "2016-01-03", "2016-01-04")),
                  y = 1:3)
  expect_equal(thicken(x, start_val = as.Date("2016-01-02"), interval = "year")  %>%
                  nrow, 2)
})

context("thicken integration tests")

test_that("thicken gives correct interval", {
  x_df <- data.frame(x_sec = x_sec)
  expect_equal(sw(thicken(x_df, interval = "year"))$x_sec_year %>% get_interval,
               "year")
  expect_equal(sw(thicken(x_df, interval = "month"))$x_sec_month %>% get_interval,
               "month")
  expect_equal(sw(thicken(x_df, interval = "day"))$x_sec_day %>% get_interval,
               "day")
  expect_equal(sw(thicken(x_df, interval = "hour"))$x_sec_hour %>% get_interval,
               "hour")
  expect_equal(sw(thicken(x_df, interval = "min"))$x_sec_min %>% get_interval,
               "min")
})

test_that("thicken gives correct output when x is a vector", {
  day_sorted <- sort(x_day)
  day_to_year <- thicken(day_sorted %>% as.data.frame, colname = "x", interval = "year")$x
  day_to_year2 <- thicken(day_sorted %>% as.data.frame, "x", interval = "year",
                          rounding = "up")$x

  expect_equal(day_to_year %>% length, 100)
  expect_equal(lubridate::year(day_to_year[1]), 2015)
  expect_equal(lubridate::year(day_to_year[100]), 2016)
  expect_equal(lubridate::year(day_to_year2[1]), 2016)
  expect_equal(lubridate::year(day_to_year2[100]), 2017)
})

test_that("thicken gives correct ouput when x is a df", {
  X <- data.frame(day_var = seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by = "day"), #nolint
                  value   = runif(366, 50, 100))

  expect_equal(thicken(X, interval = "month") %>% nrow, 366)
  expect_equal( lubridate::month(thicken(X, interval = "month")$day_var_month) %>% max, 12) #nolint
  expect_error( (thicken(dplyr::as_tibble(X), interval = "month")), NA)
  expect_error( thicken(data.table::as.data.table(X), interval = "month"), NA)
})

test_that("column naming works properly", {
  a <- sort(x_day)
  a_df <- data.frame(a = a, b = 42)
  expect_equal(colnames(thicken(a_df, interval = "week"))[3], "a_week")
  expect_equal(colnames(thicken(a_df, interval = "2 days", colname = "jos"))[3], "jos")
})


context("test set_to_original_type")

test_that("set_to_original_type returns tbl or data.table", {
  expect_equal(sw(dplyr::as_tibble(df_with_one_date) %>% thicken("2 mon") %>% class),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(sw(data.table::as.data.table(df_with_one_date) %>% thicken("2 mon") %>%
                    class),
               c("data.table", "data.frame"))
})


context("thicken with missing values")

test_that("thicken works properly on NA values", {
  coffee_na <- coffee %>% thicken("day", "d") %>% count(d) %>% pad() %>%
    fill_by_value()
  coffee_na[3, 1] <- NA
  coffee_na_thickened <- sw(coffee_na %>% thicken("week"))
  expect_error(sw(coffee_na %>% thicken("week")), NA)
  expect_warning(coffee_na %>% thicken("week"),
                 "There are NA values in the column d.
Returned dataframe contains original observations, with NA values for d and d_week.")
  expect_equal(coffee_na_thickened %>% nrow, 4)
  expect_equal(coffee_na_thickened %>% filter(is.na(d)) %>% nrow, 1)
  expect_equal(coffee_na_thickened %>% filter(is.na(d_week)) %>% nrow, 1)
  expect_equal(coffee_na_thickened$d[3] %>% as.character(), NA_character_)

  coffee_two_nas <-  coffee %>% thicken("day", "d") %>% count(d) %>% pad() %>%
    fill_by_value()
  coffee_two_nas[c(2, 3), 1] <- NA

  expect_equal(sw(thicken(coffee_two_nas, "week"))$d_week,
               c(as.Date("2016-07-03"), NA, NA, as.Date("2016-07-10")))
})

test_that("add_na_to_thicken unit tests", {
  thickened_date     <- ymd(c(20180101, 20180101))
  thickened_datetime <- ymd_h(c("20180101 01", "20180101 04"))
  thickened_datetime_CET <- as.POSIXct(as.character(thickened_datetime), tz = "CET")


  expect_equal(add_na_to_thicken(thickened_date, 2),
               ymd(c(20180101, NA, 20180101)))
  expect_equal(add_na_to_thicken(thickened_datetime, 2),
               ymd_h(c("20180101 01", NA, "20180101 04")))
  expect_equal(add_na_to_thicken(thickened_date, c(2, 4)),
               ymd(c(20180101, NA, 20180101, NA)))
  expect_equal(add_na_to_thicken(thickened_datetime, c(2, 4)),
               ymd_h(c("20180101 01", NA, "20180101 04", NA)))

  expect_equal(add_na_to_thicken(thickened_datetime_CET, c(2, 4)),
               ymd_h(c("20180101 01", NA, "20180101 04", NA), tz = "CET"))
})

context("thicken drop argument")
test_that("the drop argument gives the desired result", {
  hourly <- ymd_h(c("20160707 09",
                    "20160707 09",
                    "20160709 13",
                    "20160710 10"), tz = "CET")
  coffee_hour <- coffee %>% mutate(time_stamp_hour = hourly)
  no_drop <- coffee_hour
  with_drop <- coffee_hour %>% select(-time_stamp)
  expect_equal(thicken(coffee, "hour"), no_drop)
  expect_equal(thicken(coffee, "hour", drop = FALSE), no_drop)
  expect_equal(thicken(coffee, "hour", drop = TRUE), with_drop)
})

context("ties_to_earlier argument to thicken")
x <- data.frame(
  dt = ymd_hm("20171021 1631", "20171021 1700", "20171021 1731"))

test_that("ties_to_earlier works with rounding down regular",{
  expect_equal(thicken(x, "hour", ties_to_earlier = TRUE)$dt_hour,
               ymd_h("20171021 16", "20171021 16", "20171021 17"))
})

test_that("ties_to_earlier works with rounding up regular", {
  expect_equal(thicken(x, "hour", rounding = "up", ties_to_earlier = TRUE)$dt_hour,
               ymd_h("20171021 17", "20171021 17", "20171021 18"))
})

test_that("ties_to_earlier works with rounding down ties on edges", {
  expect_equal(thicken(x[2:3, ,drop = FALSE], "hour", ties_to_earlier = TRUE)$dt_hour,
               ymd_h("20171021 16", "20171021 17"))
})

test_that("ties_to_earlier works with rounding up ties on edges", {
  expect_equal(thicken(x[1:2, ,drop = FALSE], "hour", rounding = "up", ties_to_earlier = TRUE)$dt_hour,
               ymd_h("20171021 17", "20171021 17"))
})

