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
  sp  <- span_date(20170101, len_out = 4)[c(1, 3, 4)]

  one_group_padded <- data_frame(date = rep(sp, 2),
                                 grp  = rep(c("a", "b"), each = 3),
                                 val  = c(1, NA, 1, 1, NA, 1))

  one_group <- one_group_padded[c(1, 3, 4, 6), ]

  og_no_group <- bind_rows(one_group[1:2, ],
                           data_frame(date = ymd(20170103), grp = NA, val = NA),
                           one_group[3:4, ])

  two_group <- data_frame(date = sp[rep(c(1, 3), 4)],
                          grp1 = rep(c("a", "b"), each = 4),
                          grp2 = rep(c("d", "e"), 4),
                          val  = 1)

  expect_equal(
    pad_cust(one_group, spanned = sp, drop_last_spanned = FALSE),
    og_no_group)

  expect_equal(
    pad_cust(one_group, spanned = sp, group = "grp", drop_last_spanned = FALSE),
    one_group_padded
  )

})
