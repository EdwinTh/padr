# Download for inclusion = 2016-10-17 16:15:40 CEST

library(dplyr)

url <- "https://storage.googleapis.com/montco-stats/tzr.csv"

emergency <- read.csv(url(urlLink), header = TRUE, stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  select(-desc, -e) %>%
  mutate(timeStamp = ymd_hms(timeStamp, tz='EST')) %>%
  rename(time_stamp = timeStamp)

# Be careful when rerunning, data will be updated.
devtools::use_data(emergency)
