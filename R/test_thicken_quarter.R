# This script tests thicken with x having a quarter interval.

library(dplyr)
library(lubridate)



df_quarter <- data_frame(datetime = seq(start_day, end_day, by = 'quarter'))
df_quarter_dt <- data_frame(datetime = seq(start_day, end_day, by = 'quarter'))
