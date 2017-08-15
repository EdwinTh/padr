
## ----------- new work ----------------##
# steps:
# when Date >> go to old way right away.
# else: see if can get coerced to Date, if yes go to old way.
# else: determine min_dif_posix: if %% (24 * 3600) == 0, coerce to date and go to
   # old way (this require new function, that cuts hh:mm:ss and coerces to date)
# else: seconds to interval

get_interval_units <- function(x) {
  x_num <- sort(unique(as.numeric(x)))
  if (length(unique(x_num)) == 1) {
    stop("x does not vary, cannot determine the interval", call. = FALSE)
  }
  unit_difs <- get_difs(x_num)
  if (min(unit_difs) == 1) {
    return(1)
  }
  factors  <- lapply(unit_difs, all_factors, max_to_test = min(unit_difs))
  span_cap <- lowest_max(factors)
  if (span_cap == 1) {
    return(1)
  }
  find_commen_fac(factors, span_cap)
}

all_factors <- function(x,
                        max_to_test = max(x)) {
  if (x %% 2 == 0) {
    x_test <- c(1:(x / 2), x)
  } else {
    x_test <- c(1:ceiling(x / 3), x)
  }
  x_test <- x_test[x_test <= max_to_test]
  x_test[x %% x_test == 0]
}

lowest_max <- function(x) {
  min(sapply(x, max))
}

span_logical <- function(f, cap) {
  lgl_vec <- rep(FALSE, pmin(f[length(f)], cap))
  lgl_vec[f[f < cap]] <- TRUE
  lgl_vec
}

find_commen_fac <- function(f, cap) {
  lgl_mat <- sapply(f, span_logical, cap = cap)
  max(which(rowSums(lgl_mat) == length(f)))
}


seconds_to_interval <- function(x) {
  if (x %% 3600 == 0) {
    paste0(x / 3600, " hour")
  } else if (x %% 60 == 0) {
    paste0(x / 60, " min")
  } else {
    paste0(x, " sec")
  }
}




