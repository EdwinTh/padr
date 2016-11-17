library(dplyr)
library(ggplot2)
library(lubridate)
filter <- dplyr::filter

# Download for inclusion = 2016-10-17 16:15:40 CEST
url <- "https://storage.googleapis.com/montco-stats/tzr.csv"
# emergency <- read.csv(url(urlLink), header = TRUE, stringsAsFactors = FALSE) %>%
#   as_data_frame %>%
#   select(-desc, -e) %>%
#   mutate(timeStamp = ymd_hms(timeStamp, tz='EST')) %>%
#   rename(time_stamp = timeStamp)

# Be careful when rerunning, data will be updated.
# devtools::use_data(emergency)

#' Emergency Calls for Montgomery County, PA
#'
#' The emergency calls coming in at Montgomery County, PA since 2015-12-10.
#' Data set was created at 2016-10-17 16:15:40 CEST from the API and contains
#' events untill 2016-10-17 09:47:03 EST. From the original set the columns
#' desc and e are not included.
#' @format A data frame with 120450 rows and 6 variables:
#' \describe{
#' \item{lat}{Latitude from Google maps, based on the address}
#' \item{lng}{Longitude from Google maps, based on the address}
#' \item{zip}{Zipcode from Google, when possible}
#' \item{title}{Title, emergency category}
#' \item{time_stamp}{YYYY-MM-DD HH:MM:SS}
#' \item{twp}{Township}
#' }
#' @source \url{https://storage.googleapis.com/montco-stats/tzr.csv}
"emergency"
