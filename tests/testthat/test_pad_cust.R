context("Helper functions for pad_cust")

test_that("check_dt_in_spanned", {
  sp <- span_date(20170101, len_out = 3)
  expect_error(check_dt_in_spanned(as.Date("2017-01-01"), sp), NA)
  expect_error(check_dt_in_spanned(span_date(20170101, len_out = 2), sp), NA)
  expect_error(check_dt_in_spanned(as.Date("2017-01-04"), sp),
               "Observations in the datetime variable, that are not in spanned.
  Either run thicken_cust in combination with aggregation first, or rerun this function
  with drop_last_spanned = FALSE.")
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
  x <- data.frame(a = span_date(20160101, len_out = 2, by = "day"), b = 1)
  span <- span_date(20160101, len_out = 3, by = "day")
  expect_equal(x, pad_cust(x, span, drop_last_spanned = TRUE))
  expect_equal(pad_cust(x, span, drop_last_spanned = FALSE) %>% nrow(), 3)
  expect_equal(pad_cust(x, span, drop_last_spanned = FALSE)$a, span)
})

test_that("pad_cust_group_span", {
  sp  <- span_date(20170101, len_out = 4)[c(1, 3, 4)]

  one_group_padded <- tibble(date = rep(sp, 2),
                             grp  = rep(c("a", "b"), each = 3),
                             val  = c(1, NA, 1, 1, NA, 1))

  one_group <- one_group_padded[c(1, 3, 4, 6), ]

  og_no_group <- bind_rows(one_group[1:2, ],
                           tibble(date = ymd(20170103), grp = NA, val = NA),
                           one_group[3:4, ]) %>%
    arrange(date)

  two_group_padded <- tibble(date = rep(sp, 4),
                             grp1 = rep(c("a", "b"), each = 6),
                             grp2 = rep(c("d", "e"), 6),
                             val  = c(1, NA, 1, 1, NA, 1, 1, NA, 1, 1, NA, 1)) %>%
    arrange(grp1, grp2, date)

  two_group <- two_group_padded[-c(2, 5, 8, 11), ]

  tg_one_group <- two_group_padded[-c(5, 11), ]
  tg_one_group[c(2, 7), "grp2"] <- NA
  tg_one_group <- arrange(tg_one_group, grp1, date)
  tg_no_group <- tg_one_group[-8, ]
  tg_no_group[3, "grp1"] <- NA
  tg_no_group <- arrange(tg_no_group, date)

  expect_equal(
    pad_cust(one_group, spanned = sp, drop_last_spanned = FALSE),
    og_no_group)

  expect_equal(
    pad_cust(one_group, spanned = sp, group = "grp", drop_last_spanned = FALSE),
    one_group_padded
  )

  expect_equal(
    one_group %>% group_by(grp) %>% pad_cust(spanned = sp, drop_last_spanned = FALSE),
    one_group_padded %>% group_by(grp)
  )

  expect_equal(
    pad_cust(two_group, span = sp, drop_last_spanned = FALSE),
    tg_no_group
  )

  expect_equal(
    pad_cust(two_group, span = sp, group = "grp1", drop_last_spanned = FALSE),
    tg_one_group
  )

  expect_equal(
    two_group %>% group_by(grp1) %>% pad_cust(span = sp, drop_last_spanned = FALSE),
    tg_one_group %>% group_by(grp1)
  )

  expect_equal(
    pad_cust(two_group, span = sp, group = c("grp1", "grp2"), drop_last_spanned = FALSE),
    two_group_padded
  )

  expect_equal(
    two_group %>% group_by(grp1, grp2) %>% pad_cust(span = sp, drop_last_spanned = FALSE),
    two_group_padded %>% group_by(grp1, grp2)
  )
})
