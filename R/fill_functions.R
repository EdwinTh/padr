#' Fill all NA values in a vector by the same value
#'
#' @param x A vector containing NA values.
#' @param replace The value to replace the NA values by.
#' @return \code{x} with its NA values replaced by \code{replace}
#' @examples
#' x <- round(rnorm(200), 2)
#' x[sample(1:200, 50)] <- NA
#' fill_by_value(x, 42)
#'
#' x <- rep(letters[1:10], each = 10)
#' x[sample(1:100, 25)] <- NA
#' fill_by_value(x, 'Missing')

fill_by_value <- function(x,
                          value = 0) {
  if(! any (is.na(x) )) {
    warning('x does not contain NA values, just returning x')
    return(x)
  }
  x[is.na(x)] <- value
  return(x)
}


#' Fill all NA values in a vector by a function of it
#'
#' Take all the nonmissing values in x and apply a function on it. Subsequently
#' replace all NA values by the outcome of the function.
#' @param x A vector containing NA values.
#' @param fun The function to apply on all nonmissing values of x.
#' @param ... Optional parameters for \code{fun}.
#' @return \code{x} with its NA values replaced by the outcome of \code{fun}.
#' x <- round(rnorm(200), 2)
#' x[sample(1:200, 50)] <- NA
#' fill_by_function(x)
#' fill_by_function(x, median)
#'
#' my_function <- function(x, y) mean(x^2 / y)
#' fill_by_function(x, my_function, .1)
#' fill_by_function(x, my_function, 10)
fill_by_function <- function(x,
                             fun = mean,
                             ...) {
  if(! any (is.na(x) )) {
    warning('x does not contain NA values, just returning x')
    return(x)
  }
  if(! is.function(fun) ) {
    break('fun is not a valid function')
  }

  x_no_na <- x[!is.na(x)]
  value <- fun(x_no_na, ...)

  if(length(value) > 1){
    warning('fun does return multiple values, only the first is used')
    value <- value[1]
  }
  x[is.na(x)] <- value
  return(x)
}

#' Fill all NA values in a vector by the most prevalent nonmissing value
#'
#' @param x A vector containing NA values.
#' @return \code{x} with its NA values replaced by its most prevalent
#' nonmissing value.
#' @examples
#' x <- rep(letters[1:10], seq(20, 2, by =-2))
#' x[sample(length(x), 25)] <- NA
#' fill_by_prevalent(x)

fill_by_prevalent <- function(x) {
  if(! any (is.na(x) )) {
    warning('x does not contain NA values, just returning x')
    return(x)
  }

  x_count <- table(x)
  if( sum(x_count == max(x_count)) > 1 ) {
    vals <- paste(names( which (x_count == max(x_count) ) ), collapse = ', ')
    break(paste( vals, 'tie for most prevalent, please select a value and use fill_by_value') )
  }

  value <- names( which( x_count == max(x_count) ) )
  x[is.na(x)] <- value
  return(x)
}


