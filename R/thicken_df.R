#' Thicken a data.frame
#'
#' This function will \code{thicken} a \code{data.frame} by thickening a
#' variable of class
#'


thicken_df <- function(df,
                       by = NULL,
                       interval = c('year',
                                    'month',
                                    'day',
                                    'hour',
                                    'minute'),
                       rounding = c('closest',
                                    'up',
                                    'down'),
                       allow_duplicates = TRUE){

  if(is.null(by)) {
    date_variable <- get_date_variables(df)

    if(length(date_variable) == 0) {
      stop('df does not contain variables of class POSIXct, POSIXlt or Date')
    } else if(length(date_variable) > 1) {
      stop('x contains multiple variables POSIXct, POSIXlt or Date
           please provide the variable to pad by')
    } else {
      date_variable <- df[,colnames(df) == date_variable]
    }
  }

}
