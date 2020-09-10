## code to prepare `coffee` dataset goes here

time_stamp_raw <- c("2016-07-07 09:11:21",
                    "2016-07-07 09:46:48",
                    "2016-07-09 13:25:17",
                    "2016-07-10 10:45:11")

coffee <- tibble(
  time_stamp = as.POSIXct(time_stamp_raw, tz = "CET"),
  amount     = c(3.14, 2.98, 4.11, 3.14)
)

usethis::use_data(coffee, overwrite = TRUE)
