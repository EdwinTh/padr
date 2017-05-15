
<!-- README.md is generated from README.Rmd. Please edit that file -->
padr
====

[![Build Status](https://travis-ci.org/EdwinTh/padr.png?branch=master)](https://travis-ci.org/EdwinTh/padr) [![codecov.io](https://codecov.io/github/EdwinTh/padr/coverage.svg?branch=master)](https://codecov.io/github/EdwinTh/padr?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/padr)](https://cran.r-project.org/package=padr) [![](http://cranlogs.r-pkg.org/badges/padr)](https://cran.r-project.org/package=padr)

`padr` is an R package that assists with preparing time series data. It provides two main functions that will quickly get the data in the format you want. When data is observed on too low a level, `thicken` will add a column of a higher interval to the data frame, after which the user can apply the appropriate aggregation. When there are missing records for time points where observations were absent, `pad` will automatically insert these records. A number of `fill_` functions help to subsequently fill the missing values.

Usage
=====

``` r
library(padr)
library(tidyverse)
coffee <- data.frame(
  time_stamp =  as.POSIXct(c(
    '2016-07-07 09:11:21', '2016-07-07 09:46:48',
    
    '2016-07-09 13:25:17',
    '2016-07-10 10:45:11'
  )),
  amount = c(3.14, 2.98, 4.11, 3.14)
)

coffee %>%
  thicken('day') %>%
  dplyr::group_by(time_stamp_day) %>%
  dplyr::summarise(day_amount = sum(amount)) %>%
  pad() %>%
  fill_by_value(day_amount, value = 0)
```

    ## # A tibble: 4 × 2
    ##   time_stamp_day day_amount
    ##           <date>      <dbl>
    ## 1     2016-07-07       6.12
    ## 2     2016-07-08       0.00
    ## 3     2016-07-09       4.11
    ## 4     2016-07-10       3.14

More information
================

See the the general [introduction Vignette](https://github.com/EdwinTh/padr/blob/master/vignettes/padr.Rmd) for more examples. The [implementation details Vignette](https://github.com/EdwinTh/padr/blob/master/vignettes/padr_implementation.Rmd) describes how `padr` handles different time zones and daylight savings time.
