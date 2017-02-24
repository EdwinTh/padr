

pad_int <- function(x,
                    by,
                    start_val = NULL,
                    end_val   = NULL,
                    group     = NULL,
                    step      = 1){

  # this is preferred over as.list(match.call()) because of magrittr
  pad_args <- list(x         = x,
                   by        = by,
                   start_val = start_val,
                   end_val   = end_val,
                   group     = group,
                   step      = step)

  if (is.null(group)) {
    do.call(pad_single_int, pad_args)
  } else {
    do.call(pad_multiple_int, pad_args)
  }
}

pad_single_int  <- function(x,
                            by,
                            start_val = NULL,
                            end_val   = NULL,
                            group     = NULL,
                            step      = 1){

  if (!is.data.frame(x)) {
    stop('x should be a data frame.')
  }

  arguments <- as.list(match.call())
  pad_var_name <- as.character(arguments$by)

  original_data_frame <- x
  x <- as.data.frame(x)

  pad_var <- check_data_frame_int(x, by = pad_var_name)

  spanned <- span_pad_int(pad_var, start_val, end_val, step)

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
                                  pad_var_name] <- 'spanned'

  return_frame  <- merge(join_frame, original_data_frame, by = cols_to_join_on,
                         all.x = TRUE)
  colnames(return_frame)[colnames(return_frame) == 'spanned'] <- pad_var_name

  # return_frame <- set_to_original_type(return_frame, original_data_frame)
  return(return_frame)
}

# This is the wrapper around pad_single

pad_multiple_int <- function(x,
                             by        = NULL,
                             start_val = NULL,
                             end_val   = NULL,
                             group     = group,
                             step      = 1){
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
                     by        = by,
                     start_val = start_val,
                     end_val   = end_val,
                     group     = groupings[i, , drop = FALSE],
                     step      = step) # nolint

    padded_groups[[i]] <- do.call(pad_single_int, pad_args)
  }
  return(do.call("rbind", padded_groups))
}

span_pad_int <- function(pad_var, start_val, end_val, step) {
  if (is.null(start_val)) start_val <- min(pad_var)
  if (is.null(end_val)) end_val <- max(pad_var)
  pad_var_rel <- pad_var[pad_var >= start_val & pad_var <= end_val]
  if ( any(  ((pad_var_rel- start_val) %% step) != 0 )  ) {
    stop('step size unvalid for this variable, possibly due to the start_val or end_val') #nolint
  }
  return( seq(start_val, end_val, by = step))
}


set_to_original_type <- function(x,
                                 original) {
  if (inherits(original, "tbl_df")) {
    x <- dplyr::as_data_frame(x)
  } else if (inherits(original, "data.table")) {
    x <- data.table::as.data.table(x)
  }
  return(x)
}

