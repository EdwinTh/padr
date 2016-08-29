#' Pad a data.frame
#'
#' This function will take a \code{data.frame} that contains a variable of the
#' following classes \code{Date}, \code{POSIXct}, or \code{POSIXlt}. It will
#' insert rows for all the instances that are missing from the data.
#' @details If the interval is not provided \code{pad_date} will determine the
#' interval itself by using the \code{get_interval} function. If the interval
#' is provided, but the interval present in the data is of a finer granualarity
#' it will automatically set the data to the closest instance of \code{interval}.
#' If multiple records should be set to

pad_date <- function(df,  # data.frame to pad
                     pad = NULL,
                     interval  = NULL, # the time level of padding, default is the smallest alteration level
                     with = c( # how to pad the other variables
                       '0_or_NA', # fill each variable with 0 if numeric or integer, NA otherwise
                       'NA',  # fill each variable with
                       'last_caried_forward' # last available value
                     ),
                     pad_present_NA = FALSE,
                     .... # provide your own pad function or value
) {
  # should find the variable of type POSIXt or Date, must be exactly 1 otherwise throw an error
  if(is.null(pad)) {
    date_variable <- get_date_variables(df)

    if(length(date_variable) == 0) {
      stop('data.frame does not contain variables of class POSIXct, POSIXlt or Date')
    } else if(length(date_variable) > 1) {
      stop('data.frame contains multiple variables POSIXct, POSIXlt or Date
           please provide the variable to pad by')
    } else {
      date_variable <- df[,colnames(df) == date_variable]
    }
  }

  # not sure whether this is the prettiest way to do it
  # TODO do this more elegantly
  date_variable <-
    df %>%
    select(date_variable_ind %>% which) %>%
    unlist %>%
    as.POSIXct(origin = as.Date('1970-01-01'))

  if(is.null(by)) {
    alternation_level <- get_alternation_level(date_variable)
  } else {
    alternation_level <- by
  }

  # create a data.frame to left join against
  left_side <-
    data.frame(x = seq(
      date_variable %>% min,
      date_variable %>% max,
      by = alternation_level)
    )

  colnames(left_side) <- colnames(df)[date_variable_ind]
  padded_df <- left_join(left_side,
                         df)

  # TODO check how we can do this cleanly, just provide the argument
  # not the first of a vector
  if(with[1] == '0_or_na'){

  }?s

  if(with[1] == 'last_caried_forward') {
    df_padded
  }






  # if fill is not equal to NA fill with either 0 r f
}
