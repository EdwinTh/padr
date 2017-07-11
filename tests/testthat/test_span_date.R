context("integer_to_date function")

test_that("function only works on the appropriate classes", {
  expect_error(integer_to_date("a"))
  expect_error(integer_to_date(TRUE))
  expect_error(integer_to_date(2014), NA)
  expect_error(integer_to_date(2014.0), NA)
  expect_error(integer_to_date(2014.1))
  expect_error(integer_to_date(as.Date("2014-01-01")))
})

test_that("function only works on the appropriate length", {
  expect_error(integer_to_date(201), "from is not of a valid length")
  expect_error(integer_to_date(2011), NA)
  expect_error(integer_to_date(20110), "from is not of a valid length")
  expect_error(integer_to_date(201101), NA)
  expect_error(integer_to_date(2011010), "from is not of a valid length")
  expect_error(integer_to_date(20110101), NA)
})

test_that("function works correctly on doubles", {
  expect_error(integer_to_date(2014.0), NA)
  expect_error(integer_to_date(2014.1), "from is not a valid integer")
})

context("general workings span_date")

test_that("check_two_null give right error",{
  jos <- 40
  expect_error(check_two_null(jos, jos), NA)
  expect_warning(check_two_null(jos, jos),
                 "both to and out_len are specified, out_len is ignored")
  expect_error(check_two_null(jos, NULL), NA)
  expect_error(check_two_null(NULL, jos), NA)
  expect_error(check_two_null(NULL, NULL),
               "either to or length must be specified")
})

test_that("check_equal_length works properly", {
  expect_error(check_equal_length(1998, 1999), NA)
  expect_error(check_equal_length(1998, NULL), NA)
  expect_error(check_equal_length(1998, 199901),
               "from and to should be of equal length")
})


contex("span_date integration tests")
test_that("span_date gives the desired outputs", {
  year_span <- seq.Date(as.Date("2011-01-01"), as.Date("2015-01-01"), by = "year")
  month_span <- seq.Date(as.Date("2011-01-01"), as.Date("2015-01-01"), by = "month")
  day_span <- seq.Date(as.Date("2011-01-01"), as.Date("2011-02-01"), by = "day")
  expect_equal(span_date(2011, 2015), year_span)
  expect_equal(span_date(201101, 201501), month_span)
  expect_equal(span_date(2011, 2015, interval = "month"), month_span)
  expect_equal(span_date(20110101, 20110201), day_span)
  expect_equal(span_date(20110101, 20110201, interval = "month"), month_span)
  expect_equal(span_date(2011, len_out = 5), year_span)
  expect_equal(span_date(201101, len_out = 49), month_span)
  expect_equal(span_date(20110101, len_out = 32), day_span)
  expect_equal(span_date(20110101, len_out = 49, interval = "month"), month_span)
})
