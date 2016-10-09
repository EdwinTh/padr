library(stringr)
library(lubridate)
library(dplyr)
library()
set.seed(98765)
sample_monthly <- function(categorie = 'rent',
                           in_out = 'out',
                           amount = 1150,
                           days = 21:24,
                           weights = c(.2, .4, .4, .2)){
  sampled_days <- sample(days, 12, TRUE, weights)
  sampled_hours <- (100 + sample(0:23, 12, TRUE)) %>% str_sub(2, 3)
  sampled_mins  <- (100 + sample(0:59, 12, TRUE)) %>% str_sub(2, 3)
  sampled_seconds  <- (100 + sample(0:59, 12, TRUE)) %>% str_sub(2, 3)
  sampled_times <- paste(paste(2016, sampled_days, sampled_hours, sep = '-'),
                   paste(sampled_mins, sampled_mins, sampled_seconds, sep = ':'))
  
  ymd_hms(sampled_times)
}