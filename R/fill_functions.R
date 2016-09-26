
x <- seq(ymd('20150101'), ymd('20160101'), by = 'day') %>%
  sample(200) %>% sort %>% data_frame(x = .)
x$y <- runif(200, 1, 20) %>% round
x$z <- sample(LETTERS[1:5], 200, TRUE)
x$w <- rnorm(200, 2, 2) %>% round(2)
x <- pad(x)

fill_na_by_mean <- function(x, ...){
  arguments <- as.list(match.call())
  x <- as.data.frame(x)
  if(length(arguments) == 2) {
    cols_to_fill_ind <- sapply(x, class) %in% c('integer', 'numeric') %>% which
    cols_to_fill_name <- colnames(x)[cols_to_fill_ind]
  } else {
    cols_to_fill_names <- arguments[3:length(arguments)] %>% as.character
    cols_to_fill_ind   <- colnames(x) %in% cols_to_fill_names %>% which
    if(length(cols_to_fill_ind) != cols_to_fill_names) {
      stop('One or more supplied variables to fill is not a column name.')
    }
  }
  means <- x[cols_to_fill] %>% colMeans(na.rm = TRUE)
  fill_na <- function(x, m) ifelse(x %>% is.na, m, x)

  filled <- mapply(fill_na, x[cols_to_fill_ind], means)

  # TODO place filled back in x and return
}


