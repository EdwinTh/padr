
span_time("20160707 00", 20160711, tz = "CET") -> spanned
  subset_span(list(hour = c(0, 9, 16)))
#coffee %>% thicken_cust(x, )

pad_cust <- function(x,
                     spanned,
                     by        = NULL,
                     group     = NULL){
  is_df(x)
  group <- get_dplyr_groups(x, group)

  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.', call. = FALSE)
  }

  original_data_frame <- x
  x <- as.data.frame(x)
  original_interval <- interval

  # we have to get the dt_var twice, first on the original data. If there are
  # NA values in it, we have to get it again on x with NA values filtered out.
  dt_var_info_original <- get_dt_var_and_name(x, by)
  dt_var_name <- dt_var_info_original$dt_var_name
  x_NA_list <- check_for_NA_pad(x, dt_var_info_original$dt_var,
                                dt_var_name)
  x <- x_NA_list$x
  x_NA <- x_NA_list$x_NA

  dt_var_info <- get_dt_var_and_name(x, by)
  dt_var      <- dt_var_info$dt_var

  check_dt_var_in_group(dt_var_name, group)

  # TODO check if the spanned an the dt_var are of the same type.

    # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(x) == dt_var_name)
  x[, pos] <- dt_var

  group_vars_un  <- group_unique_vars(x, group)
  spanned_groups <- pad_cust_group_span(spanned, group_vars_un)

  if (!is.null(interval)) {
    if (!is.null(start_val)) dt_var <- dt_var[dt_var >= start_val]
    if (!is.null(end_val)) dt_var <- dt_var[dt_var <= end_val]
    check_interval_validity(spanned$span, dt_var)
  }

  colnames(x)[colnames(x) == dt_var_name] <- 'span'

  return_frame <- suppressMessages(
    dplyr::left_join(spanned, x)
  )

  colnames(return_frame)[colnames(return_frame) == 'span'] <- dt_var_name

  return_frame <- to_original_format(return_frame,
                                     group,
                                     dt_var_name,
                                     original_data_frame)

  return_frame <- set_to_original_type(return_frame, original_data_frame)

  if (is.null(original_interval)) {
    interval_message(interval)
  }

  dplyr::bind_rows(return_frame, x_NA)
}

group_unique_vars <- function(x, group) {
  if (is.null(group)) {
    NULL
  } else {
    unique(x[ ,group])
  }
}

pad_cust_group_span <- function(spanned, group_vars) {
  if (is.null(group)) {
    data_frame(spanned)
  } else {
    spanned <- rep(spanned, nrow(group_vars))

  }
}
