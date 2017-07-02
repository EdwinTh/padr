
#' Emergency Calls for Montgomery County, PA
#'
#' The emergency calls coming in at Montgomery County, PA since 2015-12-10.
#' Data set was created at 2016-10-17 16:15:40 CEST from the API and contains
#' events until 2016-10-17 09:47:03 EST. From the original set the columns
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


#' Coffee Data Set
#'
#' Made-up data set for demonstrating \code{padr}.
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#' \item{time_stamp}{YYYY-MM-DD HH:MM:SS}
#' \item{amount}{Amount spent on coffee}
#' }
"coffee"
