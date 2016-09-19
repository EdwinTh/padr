# Internal functions for a data.frame and for a vector that check
# if the data format is as expected.
check_data_frame <- function(x,
                             by = NULL){
  x <- as.data.frame(x)
  arguments <- as.list(match.call())

  if(!is.null(arguments$by)) {
    if(length(arguments$by) > 1) stop('by can indicate one variable only')
    return(eval(arguments$by, x))
  } else {
    dt_var_name <- get_date_variables(x)
    if(length(dt_var_name) == 0) {
      stop('x does not contain a variabel of class Date, POSIXct, or POSIXlt',
           call. = FALSE)
    }
    if(length(dt_var_name) > 1){
      stop('x contains multiple variables of class Date, POSIXct, or POSIXlt,
           please specify which variable to use in the by argument',
           call. = FALSE)
    }
    return(x[ ,colnames(x) == dt_var_name])
  }
}



check_vector <- function(x){
  if( !( c('Date', "POSIXt") %in% class(x) %>% any) ) {
    stop('x should be a data.frame or a vector of class Date, POSIXct, or POSIXlt', call. = FALSE)
  }
  return(x)
}
