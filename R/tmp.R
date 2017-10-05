library(dplyr)
a <- round(runif(30, 1, 99)) %>% sort
b <- seq(0, 100, 10)

rd_tmp <- function(a, b) {
  current_b  <- b[1]
  next_b     <- b[2]
  current_b_index <- 0
  ret <- numeric(length(a))
  last_b <- max(b)

  for(i in seq_along(a)) {
    if (next_b > a[i] | current_b == last_b) {
      ret[i] <- current_b
    } else {
      while (next_b <= a[i]) {
        current_b_index <- current_b_index + 1
        current_b       <- b[current_b_index]
        next_b          <- b[current_b_index + 1]
        if (is.na(next_b)) break
      }
      ret[i] <- current_b
    }
  }
  ret
}

b[1:9]
rd_tmp(a,b[2:10])
