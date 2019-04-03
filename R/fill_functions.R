#' Fill missing values by a single value
#'
#' Replace all missing values in the specified columns by the same value.
#' @param x A data frame.
#' @param ... The unquoted column names of the variables that should be filled.
#' @param  value The value to replace the missing values by.
#' @return \code{x} with the altered columns.
#'
#' @examples
#' library(dplyr) # for the pipe operator
#' x <- seq(as.Date('2016-01-01'), by = 'day', length.out = 366)
#' x <- x[sample(1:366, 200)] %>% sort
#' x_df <- data_frame(x  = x,
#'                    y1 = runif(200, 10, 20) %>% round,
#'                    y2 = runif(200, 1, 50) %>% round,
#'                    y3 = runif(200, 20, 40) %>% round,
#'                    y4 = sample(letters[1:5], 200, replace = TRUE))
#' x_padded <- x_df %>% pad
#' x_padded %>% fill_by_value(y1)
#' x_df %>% pad %>% fill_by_value(y1, y2, value = 42)
#' @export
fill_by_value <- function(x,
                          ...,
                          value = 0) {

  is_df(x)

  inds <- get_the_inds(x, ...)

  for (i in inds) {
    val <- x[, i]
    val[is.na(val)] <- value
    x[, i] <- val
  }
  x
}

#' Fill missing values by a function of the nonmissings
#'
#' For each specified column in \code{x} replace the missing values by a
#' function of the nonmissing values.
#' @param x A data frame.
#' @param ... The unquoted column names of the variables that should be filled.
#' @param fun The function to apply on the nonmissing values.
#' @return \code{x} with the altered columns.
#' @examples
#' library(dplyr) # for the pipe operator
#' x <- seq(as.Date('2016-01-01'), by = 'day', length.out = 366)
#' x <- x[sample(1:366, 200)] %>% sort
#' x_df <- data_frame(x  = x,
#'                    y1 = runif(200, 10, 20) %>% round,
#'                    y2 = runif(200, 1, 50) %>% round)
#' x_df %>% pad %>% fill_by_function(y1, y2)
#' x_df %>% pad %>% fill_by_function(y1, y2, fun = median)
#' @export
fill_by_function <- function(x,
                             ...,
                             fun = mean) {
  if (! is.function(fun) ) {
    stop('fun is not a valid function', call. = FALSE)
  }

  is_df(x)

  inds <- get_the_inds(x, ...)

  for (i in inds) {
      val <- unlist( x[, i] )
      val_no_na <- val[!is.na(val)]
      value <- fun(val_no_na)

     if (length(value) > 1){
       warning('fun does return multiple values, only the first is used',
               call. = FALSE)
       value <- value[1]
     }

     val[is.na(val)] <- value
     x[, i] <- val
   }
   return(x)
}

#' Fill missing values by the most prevalent nonmissing value
#'
#' For each specified column in \code{x} replace the missing values by the most
#' prevalent nonmissing value.
#' @param x A data frame.
#' @param ... The unquoted column names of the variables that should be filled.
#' @return \code{x} with the altered columns.
#' @examples
#' library(dplyr) # for the pipe operator
#' x <- seq(as.Date('2016-01-01'), by = 'day', length.out = 366)
#' x <- x[sample(1:366, 200)] %>% sort
#' x_df <- data_frame(x  = x,
#'                   y1 = rep(letters[1:3], c(80, 70, 50)) %>% sample,
#'                   y2 = rep(letters[2:5], c(60, 80, 40, 20)) %>% sample)
#' x_df %>% pad %>% fill_by_prevalent(y1, y2)
#' @export
fill_by_prevalent <- function(x,
                              ...) {

  is_df(x)

  inds <- get_the_inds(x, ...)

  for (i in inds) {
    val <- unlist ( x[, i] )

    x_count <- table(val)

    if ( sum(x_count == max(x_count)) > 1 ) {
       tied <- paste(names( which (x_count == max(x_count) ) ), collapse = ', ')
       stop(paste( tied,
                   'tie for most prevalent, please select a value and use fill_by_value', #nolint
                   call. = FALSE) )
    }

  value <- names( which( x_count == max(x_count) ) )
  if ( is.numeric(val) ) value <- as.numeric(value)
  val[is.na(val)] <- value
  x[, i] <- val
  }
  return(x)
}

# Get the indicators of the variables on which the function should be applied
# arguments are the colnames of x and the arguments of the original functiont
get_the_inds <- function(x,
                         ...) {

  dots <- rlang::quos(...)

  if (length(dots) == 0) {
    return(all_containing_nas(x))
  }

  which(colnames(x) %in% sapply(dots, rlang::quo_name))
}

all_containing_nas <- function(x) {
  which(colSums(is.na(x)) > 0)
}
