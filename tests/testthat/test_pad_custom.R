source("library.R")

a <- data.frame(a = seq(ymd(20160101), ymd(20160131), by = '30 d'),
                v = 1)

pad_custom(a, '10 days')
pad(a, 'hour')
