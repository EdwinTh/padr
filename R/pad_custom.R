#' @export
pad_custom <- function(x,
                       interval,
                       start_val = NULL,
                       end_val   = NULL,
                       by        = NULL,
                       group     = NULL){

  # this is preferred over as.list(match.call()) because of magrittr
  pad_args <- list(x         = x,
                   interval  = interval,
                   start_val = start_val,
                   end_val   = end_val,
                   by        = by,
                   group     = group)


  if (is.null(group)) {
    do.call(pad_single_custom, pad_args)
  } else {
    do.call(pad_multiple_custom, pad_args)
  }
}

pad_single_custom  <- function(x,
                               interval,
                               start_val = NULL,
                               end_val   = NULL,
                               by        = NULL,
                               group     = NULL){

  if (!is.data.frame(x)) {
    stop('x should be a data frame.')
  }

  arguments <- as.list(match.call())
  if (!is.null(by)) by_val <- as.character(arguments$by)

  original_data_frame <- x
  x <- as.data.frame(x)

  if (!is.null(by)){
    dt_var <- check_data_frame(x, by = by_val)
    dt_var_name <- by_val
  } else {
    dt_var <- check_data_frame(x)
    dt_var_name <- get_date_variables(x)
  }

  # When start_val or end_val are of a different time zone, coerce to tz of dt_var
  if (inherits(start_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(start_val, dt_var)
  }

  if (inherits(end_val, 'POSIXt') & inherits(dt_var, 'POSIXt')) {
    start_val <- enforce_time_zone(end_val, dt_var)
  }

  if (! is.null(start_val )) {
    dt_var <- to_posix(dt_var, start_val)$a
    start_val <- to_posix(dt_var, start_val)$b
  }

  if (! is.null(end_val )) {
    dt_var <- to_posix(dt_var, end_val)$a
    end_val <- to_posix(dt_var, end_val)$b
  }


  # If we have just one value specified it depends on start_val / end_val what to do
  if (length(unique(dt_var)) == 1 ) {

    if (is.null(start_val) && is.null(end_val) ) {
      warning ('datetime variable contains one value only and start_val and end_val are not specified,\n  returning x without padding') #nolint
      return(x)
    }

  } else {

    if ( !all(dt_var[1:(length(dt_var) - 1)] <= dt_var[2:length(dt_var)] ) ) {
      dt_var <- sort(dt_var)
      warning('Datetime variable was unsorted, pad result is sorted.')
    }

  }

  # if we want to pad a lower level than the dt_interval, we need to make it
  # a posix first to do proper padding
  # interval needs to be checked before padding, less straigtforward in custom setting
  needs_posix <- check_needs_posix(interval)
  if (inherits(dt_var, 'Date') & int_hierarchy[interval] > 5) {
    dt_var <- as.POSIXct(as.character(dt_var))
  }

  # Because dt_var might be changed we need to adjust it in the df to join later
  pos <- which(colnames(original_data_frame) == dt_var_name)
  original_data_frame[, pos] <- dt_var

  spanned <- span_pad_custom(dt_var, start_val, end_val, interval)
  check_interval_custom(spanned)

  join_frame <- data.frame(spanned = spanned)

  cols_to_join_on <- 'spanned'

  # we simply add the keys before joining
  if (!is.null(group)) {
    stopifnot(is.data.frame(group))
    # cbind gives a warning when row names are unequal with base df's
    join_frame <- suppressWarnings( cbind(join_frame,
                                          as.data.frame(group)) )
    cols_to_join_on <- c(cols_to_join_on, colnames(group))
  }

  colnames(original_data_frame)[colnames(original_data_frame) ==
                                  dt_var_name] <- 'spanned'


  return_frame  <- merge(join_frame, original_data_frame, by = cols_to_join_on,
                         all.x = TRUE)
  colnames(return_frame)[colnames(return_frame) == 'spanned'] <- dt_var_name

  return_frame <- set_to_original_type(return_frame, original_data_frame)
  return(return_frame)
}

# This is the wrapper around pad_single

pad_multiple <- function(x,
                         interval  = NULL,
                         start_val = NULL,
                         end_val   = NULL,
                         by        = NULL,
                         group     = group){
  stopifnot(is.data.frame(x))
  if (!all(group %in% colnames(x))) {
    stop('Not all grouping variables are column names of x.')
  }

  groupings <- unique(x[, colnames(x) %in% group, drop = FALSE])
  padded_groups <- vector("list", nrow(groupings))


  for (i in 1:nrow(groupings)){

    indic_mat <- matrix(0, nrow(x), ncol(groupings))
    for (j in 1:ncol(groupings)){
      indic_mat[, j] <-
        unlist(x[, colnames(x) == group[j]]) == unlist(groupings[i, j])
    }

    x_iter <- x[rowSums(indic_mat) == ncol(groupings), ]

    # because we span a data frame with the grouping vars included, we want to
    # remove them from the data frame going into pad

    pad_args <- list(x         = x_iter,
                     interval  = interval,
                     start_val = start_val,
                     end_val   = end_val,
                     by        = by,
                     group     = groupings[i, , drop = FALSE]) # nolint

    padded_groups[[i]] <- do.call(pad_single, pad_args)
  }
  return(do.call("rbind", padded_groups))
}


span_pad_custom <- function(x,
                            interval,
                            start_val = NULL,
                            end_val   = NULL) {

  if (is.null(start_val)) start_val <- min(x)
  if (is.null(end_val))   end_val   <- max(x)

  span <- seq(start_val, end_val, interval)
  return(span)
}



check_interval_custom <- function(dt_var,
                                  start_val,
                                  end_val,
                                  spanned){
  if (is.null(start_val)) start_val <- min(dt_var)
  if (is.null(end_val)) end_val <- max(dt_var)

  dt_var_sort <- sort(dt_var)
  dt_var_between_start_and_end <-
    dt_var_sort[dt_var_sort > start_val & dt_var_sort < end_val]

  all_elements <- rbind(data.frame(total_pad = start_val),
                        data.frame(total_pad = dt_var),
                        data.frame(total_pad = end_val))

  if (!(all(all_elements %in% spanned))) {
    stop(
"The desired interval is not valid for this datetime variable,
possibly in combination with the start_val and / or end _val."
    )
  }
}


