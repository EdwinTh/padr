context("Helper functions for pad_cust")

test_that("check_dt_in_spanned", {
  sp <- span_date(20170101, len_out = 3)
  expect_error(check_dt_in_spanned(as.Date("2017-01-01"), sp), NA)
  expect_error(check_dt_in_spanned(span_date(20170101, len_out = 2), sp), NA)
  expect_error(check_dt_in_spanned(as.Date("2017-01-04"), sp),
               "Observations in the datetime variable, that are not in spanned.
       Run thicken_cust in combination with aggregation first.")
})


test_that("check_same_data_type", {
  date <- span_date(20170101, len_out = 3)
  time <- span_time(20170101, len_out = 3)
  expect_error(check_same_data_type(date, date), NA)
  expect_error(check_same_data_type(time, time), NA)
  expect_error(check_same_data_type(time, date),
               "spanned and the datetime variables of different data types")
  expect_error(check_same_data_type(date, time),
               "spanned and the datetime variables of different data types")
})

test_that("pad_cust drop last argument", {
  x <- data.frame(a = span_date(20160101, len_out = 2, interval = "day"), b = 1)
  span <- span_date(20160101, len_out = 3, interval = "day")
  expect_equal(x, pad_cust(x, span, drop_last_spanned = TRUE))
  expect_equal(pad_cust(x, span, drop_last_spanned = FALSE) %>% nrow(), 3)
  expect_equal(pad_cust(x, span, drop_last_spanned = FALSE)$a, span)
})

test_that("pad_cust_group_span", {
  sp  <- span_date(20170101, len_out = 3)
  gvu1 <- data_frame(var1 = letters[1:2])
  gvu2 <- data_frame(var1 = rep(letters[1:2], each = 2),
                     var2 = rep(letters[3:4], 2))
  result_null <- data_frame(span = sp)
  result_gvu1 <- data_frame(span = rep(sp, 2),
                            var1 = letters[rep(1:2, each = 3)])
  result_gvu2 <- data_frame(span = rep(sp, 4),
                            var1 = letters[rep(1:2, each = 6)],
                            var2 = letters[rep(1:2, each = 6)])

})
