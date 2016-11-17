# padr

[![Build Status](https://travis-ci.org/EdwinTh/padr.png?branch=master)](https://travis-ci.org/EdwinTh/padr)
[![codecov.io](https://codecov.io/github/EdwinTh/padr/coverage.svg?branch=master)](https://codecov.io/github/EdwinTh/padr?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/padr)](https://cran.r-project.org/package=padr)
[![](http://cranlogs.r-pkg.org/badges/padr)](https://cran.r-project.org/package=padr)

# Usage

```r
library(padr)
library(dplyr)

coffee <- data.frame(
  time_stamp =  as.POSIXct(c(
    '2016-07-07 09:11:21', '2016-07-07 09:46:48',
    '2016-07-09 13:25:17', '2016-07-10 10:45:11'
  )),
  amount = c(3.14, 2.98, 4.11, 3.14)
)

coffee %>%
  thicken('day') %>%
  dplyr::group_by(time_stamp_day) %>%
  dplyr::summarise(day_amount = sum(amount)) %>%
  pad() %>%
  fill_by_value(day_amount, value = 0) %>%
  ggplot2::ggplot(ggplot2::aes(time_stamp_day, day_amount)) +
    ggplot2::geom_line()
```
