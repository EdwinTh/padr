# Internal functions for a data.frame check if the data format is as expected.

check_data_frame <- function(x,
                             by = NULL){
  arguments <- as.list(match.call())

  if (!is.null(arguments$by)) {

    if (length(arguments$by) > 1){
      stop('by can indicate one variable only.', call. = FALSE)
    }
    if (sum(colnames(x) == by) == 0){
      stop('by name not found in the column names.', call. = FALSE)
    }

    return( x[, colnames(x) == by] )

  } else {

    dt_var_name <- get_date_variables(x)
    if (length(dt_var_name) == 0) {
      stop ('x does not contain a variable of class Date, POSIXct, or POSIXlt.',
           call. = FALSE)
    }
    if (length(dt_var_name) > 1){
      stop('x contains multiple variables of class Date, POSIXct, or POSIXlt.\n        Please specify which variable to use in the by argument.', #nolint
           call. = FALSE)
    }
    return(x[, colnames(x) == dt_var_name])
  }
}


check_data_frame_int <- function(x, by) {
  if (length(by) > 1){
    stop('by can indicate one variable only.', call. = FALSE)
  }
  if (sum(colnames(x) == by) == 0){
    stop('by name not found in the column names.', call. = FALSE)
  }
  int_var <- x[, colnames(x) == by]

  if ( inherits(int_var, "character") | inherits(int_var, "factor")){
    stop("Indicated variable is character or factor, should be an integer.")
  }

  if ( inherits(int_var, "Date") | inherits(int_var, "POSIXt")){
    stop("Indicated variable is a datetime variable, use pad instead of pad_int.")
  }

  if (any( (int_var %% 1) != 0 ) ) {
    stop('Indicated variable is not an integer.', call. = FALSE)
  }

  return(x[, colnames(x) == by])
}
