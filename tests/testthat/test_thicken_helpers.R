
a_date <- seq(lubridate::ymd('20151201'), lubridate::ymd('20160201'), by = 'day') %>%
  sample(20) %>% sort
a_ct <- seq(lubridate::ymd_h('20151201 03'), lubridate::ymd_h('20160201 03'), by = 'hour') %>%
  sample(20) %>% sort
a_ct_cet <- seq(as.POSIXct('2015-12-01 03:00:00', tz = 'CET'),
                as.POSIXct('2016-02-01 )3:00:00', tz = 'CET'), by = 'hour') %>%
  sample(20) %>% sort
a_lt <- a_ct %>% as.POSIXlt

b_date <- span(a_date, 'month')
b_ct   <- span(a_ct, 'day')
b_ct_cet <- span(a_ct_cet, 'day')
b_lt   <- span(a_lt, 'day')


context("round_ gives correct output")

test_that('round_down gives correct ouput', {
  a_date_down   <- round_down(a_date, b_date)
  a_ct_down     <- round_down(a_ct, b_ct)
  a_ct_cet_down <- round_down(a_ct_cet, b_ct_cet)
  a_lt_down     <- round_down(a_lt, b_lt)
  expect_true(all(a_date >= a_date_down))
  expect_true(all(a_ct >= a_ct_down))
  expect_true(all(a_ct_cet >= a_ct_cet_down))
  expect_true(all(a_lt >= a_lt_down))
  expect_true( all(lubridate::day(a_date_down) == 1) )
  expect_true( all(lubridate::hour(a_ct_down) == 0) )
  expect_true( all(lubridate::hour(a_ct_cet_down) == 0) )
  expect_true( all(lubridate::hour(a_lt_down) == 0) )
})

test_that('round_up gives correct ouput', {
  a_date_up   <- round_up(a_date, b_date)
  a_ct_up     <- round_up(a_ct, b_ct)
  a_ct_cet_up <- round_up(a_ct_cet, b_ct_cet)
  a_lt_up     <- round_up(a_lt, b_lt)
  expect_true(all(a_date < a_date_up))
  expect_true(all(a_ct < a_ct_up))
  expect_true(all(a_ct_cet < a_ct_cet_up))
  expect_true(all(a_lt < a_lt_up))
  expect_true( all(lubridate::day(a_date_up) == 1) )
  expect_true( all(lubridate::hour(a_ct_up) == 0) )
  expect_true( all(lubridate::hour(a_ct_cet_up) == 0) )
  expect_true( all(lubridate::hour(a_lt_up) == 0) )
})
