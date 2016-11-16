library(lubridate)

x_date <- data.frame(x = seq(ymd(20160101), by = 'day', length.out = 367*24))
x_posix_utc <- data.frame(x = seq(ymd_h('20160101 00'), by = 'hour',
                                  length.out = 367*24))
x_posix_cet <- data.frame(x = seq(ymd_h('20160101 00', tz = 'CET'),
                                  by = 'hour', length.out = 367*24))


thicken(x_date, interval = 'month')



