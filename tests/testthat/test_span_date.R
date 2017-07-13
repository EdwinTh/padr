context("integer_to_date function")

test_that("function only works on the appropriate classes", {
  expect_error(valid_integer_dt("a", "jos"))
  expect_error(valid_integer_dt(TRUE,  "jos"))
  expect_error(valid_integer_dt(2014, "jos"), NA)
  expect_error(valid_integer_dt(2014.0), NA)
  expect_error(valid_integer_dt(2014.1, "jos"))
  expect_error(valid_integer_dt(as.Date("2014-01-01"), "jos"))
})

test_that("function only works on the appropriate length", {
  expect_error(valid_integer_dt(201, "jos"), "jos is not of a valid length")
  expect_error(valid_integer_dt(2011), NA)
  expect_error(valid_integer_dt(20110, "jos"), "jos is not of a valid length")
  expect_error(valid_integer_dt(201101), NA)
  expect_error(valid_integer_dt(2011010, "jos"), "jos is not of a valid length")
  expect_error(valid_integer_dt(20110101), NA)
})

test_that("function works correctly on doubles", {
  expect_error(valid_integer_dt(2014.0), NA)
  expect_error(valid_integer_dt(2014.1, "jos"), "jos is not a valid integer")
})

context("general workings span_date")

test_that("check_two_null give right error",{
  jos <- 40
  expect_error(check_two_null(jos, jos), NA)
  expect_warning(check_two_null(jos, jos),
                 "both to and len_out are specified, len_out is ignored")
  expect_error(check_two_null(jos, NULL), NA)
  expect_error(check_two_null(NULL, jos), NA)
  expect_error(check_two_null(NULL, NULL),
               "either to or len_out must be specified")
})

test_that("check_equal_length works properly", {
  expect_error(check_equal_length(1998, 1999), NA)
  expect_error(check_equal_length(1998, NULL), NA)
  expect_error(check_equal_length(1998, 199901),
               "from and to should be of equal length")
})


context("span_date integration tests")
test_that("span_date gives the desired outputs", {
  year_span <- seq.Date(as.Date("2011-01-01"), as.Date("2015-01-01"), by = "year")
  month_span <- seq.Date(as.Date("2011-01-01"), as.Date("2015-01-01"), by = "month")
  day_span <- seq.Date(as.Date("2011-01-01"), as.Date("2011-02-01"), by = "day")
  expect_equal(span_date(2011, 2015), year_span)
  expect_equal(span_date("2011", 2015), year_span)
  expect_equal(span_date("2011", "2015"), year_span)
  expect_equal(span_date(201101, 201501), month_span)
  expect_equal(span_date(2011, 2015, interval = "month"), month_span)
  expect_equal(span_date(20110101, 20110201), day_span)
  expect_equal(span_date(20110101, 20150101, interval = "month"), month_span)
  expect_equal(span_date(2011, len_out = 5), year_span)
  expect_equal(span_date(201101, len_out = 49), month_span)
  expect_equal(span_date(20110101, len_out = 32), day_span)
  expect_equal(span_date(20110101, len_out = 49, interval = "month"), month_span)
})
