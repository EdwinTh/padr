# Testing wether month thicken works appropriately

start_val <- ymd_hms('20160101 010101'); end_val <- ymd_hms('20170101 010101')
start_val_cet <- ymd_hms('20160101 010101', tz = 'CET')
end_val_cet <- ymd_hms('20170101 010101', tz = 'CET')
start_day <- as.Date(start_val); end_day <- as.Date(end_val)

df_month <- data_frame(datetime = seq(start_day, end_day, by = 'month'))
df_month_dt <- data_frame(datetime = seq(start_val, end_val, by = 'month'))
df_month_dt_cet <- data_frame(datetime = seq(start_val_cet, end_val_cet, by = 'month'))


context("Thicken month to year gives correct result, x = date")

test_that('Thicken month to year, x = date, no offset', {
  expect_equal(thicken(df_month)[,2],
               ymd(c(rep(20160101, 4), 20170101)))
  expect_equal(thicken(df_month, rounding = 'up')[,2],
               ymd(c(rep(20170101, 4), 20180101)))
})


test_that('Thicken month to year, x = date, date offset', {
  expect_equal(thicken(df_month, start_val = ymd(20151231))[,2],
               ymd(c(rep(20151231, 4), 20161231)))
  expect_equal(thicken(df_month, rounding = 'up', start_val = ymd(20151231))[,2],
               ymd(c(rep(20161231, 4), 20171231)))
})


test_that('Thicken month to year, x = date, posix offset', {
  expect_equal(thicken(df_month, start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20151231 010101', 4), '20161231 010101')))
  expect_equal(thicken(df_month, rounding = 'up', start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20161231 010101', 4), '20171231 010101')))
})


test_that('Thicken month to year, x = date, posix offset, with CET', {
  expect_equal(thicken(df_month, start_val = ymd_hms('20151231 010101', tz = 'CET'))[,2],
               ymd_hms(c(rep('20151231 010101', 4), '20161231 010101')))
  expect_equal(thicken(df_month, rounding = 'up', start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20161231 010101', 4), '20171231 010101')))
})


context("Thicken month to year gives correct result, x = posix")

test_that('Thicken month to year, x = posix, no offset', {
  expect_equal(thicken(df_month_dt)[,2],
               ymd(c(rep(20160101, 4), 20170101)))
  expect_equal(thicken(df_month_dt, rounding = 'up')[,2],
               ymd(c(rep(20170101, 4), 20180101)))
})

test_that('Thicken month to year, x = posix, date offset', {
  expect_equal(thicken(df_month_dt, start_val = ymd(20151231))[,2],
               ymd(c(rep(20151231, 4), 20161231)))
  expect_equal(thicken(df_month_dt, rounding = 'up', start_val = ymd(20151231))[,2],
               ymd(c(rep(20161231, 4), 20171231)))
})

test_that('Thicken month to year, x = posix, posix offset', {
  expect_equal(thicken(df_month_dt, start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20151231 010101', 4), '20161231 010101')))
  expect_equal(thicken(df_month_dt, rounding = 'up', start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20161231 010101', 4), '20171231 010101')))
})

test_that('Thicken month to year, x = posix, posix offset, with CET', {
  expect_equal(thicken(df_month_dt_cet, start_val = ymd_hms('20151231 010101', tz = 'CET'))[,2],
               ymd_hms(c(rep('20151231 010101', 4), '20161231 010101'), tz = 'CET'))
  expect_equal(thicken(df_month_dt_cet, rounding = 'up', start_val = ymd_hms('20151231 010101', tz = 'CET'))[,2],
               ymd_hms(c(rep('20161231 010101', 4), '20171231 010101'), tz = 'CET'))
})

#### Done from here

context("Thicken month to quarter gives correct result, x = date")

test_that('Thicken month to quarter, x = date, no offset', {
  expect_equal(thicken(df_month)[,2],
               ymd( rep ( c(20160101, 20160401, 20160701, 20161001), each = 3)  %>% c(20170101)) )
  expect_equal(thicken(df_month, rounding = 'up')[,2],
               ymd( rep ( c(20160401, 20160701, 20161001, 20170101), each = 3)  %>% c(20170401)) )
})


test_that('Thicken month to quarter, x = date, date offset', {
  expect_equal(thicken(df_month, start_val = ymd(20151231))[,2],
               ymd( rep ( c(20151231, 20160331, 20160701, 20161001), each = 3)  %>% c(20161231)) )
  expect_equal(thicken(df_month, rounding = 'up', start_val = ymd(20151231))[,2],
               ymd( rep ( c(20160331, 20160701, 20161001, 20161231), each = 3)  %>% c(20170331)) )
})


test_that('Thicken month to quarter, x = date, posix offset', {
  expect_equal(thicken(df_month, start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms( rep ( c('20151231 010101', '20160331 010101', '20160701 010101', '20161001 010101'), each = 3)  %>% c('20161231 010101')) )
  expect_equal(thicken(df_month, rounding = 'up', start_val = ymd_hms('20151231 010101'))[,2],
               ymd( rep ( c('20160331 010101', '20160701 010101', '20161001 010101', '20161231 010101'), each = 3)  %>% c('20170331 010101')) )
})

### Done till here



test_that('Thicken month to quarter, x = date, posix offset, with CET', {
  expect_equal(thicken(df_month, start_val = ymd_hms('20151231 010101', tz = 'CET'))[,2],
               ymd_hms(c(rep('20151231 010101', 4), '20161231 010101')))
  expect_equal(thicken(df_month, rounding = 'up', start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20161231 010101', 4), '20171231 010101')))
})


context("Thicken month to quarter gives correct result, x = posix")

test_that('Thicken month to quarter, x = posix, no offset', {
  expect_equal(thicken(df_month_dt)[,2],
               ymd(c(rep(20160101, 4), 20170101)))
  expect_equal(thicken(df_month_dt, rounding = 'up')[,2],
               ymd(c(rep(20170101, 4), 20180101)))
})

test_that('Thicken month to quarter, x = posix, date offset', {
  expect_equal(thicken(df_month_dt, start_val = ymd(20151231))[,2],
               ymd(c(rep(20151231, 4), 20161231)))
  expect_equal(thicken(df_month_dt, rounding = 'up', start_val = ymd(20151231))[,2],
               ymd(c(rep(20161231, 4), 20171231)))
})

test_that('Thicken month to quarter, x = posix, posix offset', {
  expect_equal(thicken(df_month_dt, start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20151231 010101', 4), '20161231 010101')))
  expect_equal(thicken(df_month_dt, rounding = 'up', start_val = ymd_hms('20151231 010101'))[,2],
               ymd_hms(c(rep('20161231 010101', 4), '20171231 010101')))
})

test_that('Thicken month to quarter, x = posix, posix offset, with CET', {
  expect_equal(thicken(df_month_dt_cet, start_val = ymd_hms('20151231 010101', tz = 'CET'))[,2],
               ymd_hms(c(rep('20151231 010101', 4), '20161231 010101'), tz = 'CET'))
  expect_equal(thicken(df_month_dt_cet, rounding = 'up', start_val = ymd_hms('20151231 010101', tz = 'CET'))[,2],
               ymd_hms(c(rep('20161231 010101', 4), '20171231 010101'), tz = 'CET'))
})

