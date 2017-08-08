spanned <- span_time("20151201 00", 2017)
spanned_sub <- subset_span(spanned, list(hour = c(6:10, 16:20)))
spanned = spanned_sub
x = emergency

thicken_cust <- function(x,
                         spanned,
                         colname  = "jos",
                         rounding = c('down',
                                     'up'),
                         by        = NULL) {

  is_df(x)

  original_data_frame <- x
  x <- as.data.frame(x)

  dt_var_info <- get_dt_var_and_name(x, by)
  dt_var      <- dt_var_info$dt_var
  dt_var_name <- dt_var_info$dt_var_name

  rounding <- match.arg(rounding)

  if (check_for_sorting(dt_var)){
    warning('Datetime variable was unsorted, result will be unsorted as well.',
            call. = FALSE)
  }

  start_val <- min(spanned)
  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
    spanned <- enforce_time_zone(spanned, dt_var)
  }

  ind_to_keep <- start_val_after_min_dt(start_val, dt_var)
  x <- x[ind_to_keep, , drop = FALSE] #nolint
  dt_var <- dt_var[ind_to_keep]

  if (is.null(by)) by <- dt_var_name

  dt_var <- check_for_NA_thicken(dt_var, dt_var_name, colname)

  thickened <- round_thicken(dt_var, spanned, rounding)

  if (all(all.equal(thickened, dt_var) == TRUE)) {
    stop("The thickened result is equal to the original datetime variable,
         the interval specified is too low for the interval of the datetime variable", call. = FALSE)
  }

  thickened_frame <- data.frame(dt_var, thickened)
  colnames(thickened_frame)[1] <- dt_var_name

  return_frame <- suppressMessages(dplyr::left_join(x, thickened_frame))
  colnames(return_frame)[ncol(return_frame)] <- colname

  return_frame <- set_to_original_type(return_frame, original_data_frame)

  return(return_frame)
  }
