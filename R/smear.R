#' @rdname thicken
smear <- function(x,
                  interval = c('month',
                               'day',
                               'hour',
                               'min',
                               'sec'),
                  by = NULL) {

  arguments <- as.list(match.call())
  if('by' %in% names(arguments)) by_val <- as.character(arguments$by)

  if(is.data.frame(x)) {
    original_data_frame <- x
    x <- as.data.frame(x)
    if('by' %in% names(arguments)){
      dt_var <- check_data_frame(x, by = by_val)
      dt_var_name <- by_val
    } else {
      dt_var <- check_data_frame(x)
      dt_var_name <- get_date_variables(x)
    }
  } else {
    dt_var <- check_vector(x)
  }

  interval <- match.arg(interval)
  interval_dt_var <- get_interval(dt_var)

  int_hierarchy <- 1:6
  names(int_hierarchy) <- c('year','month','day','hour','min', 'sec')
  if(int_hierarchy[interval_dt_var] > int_hierarchy[interval]) {
    stop('The interval in the datetime variable is higher than the interval given,
you might be looking fo thicken rather than for thicken.')
  } else if (int_hierarchy[get_interval(dt_var)] == int_hierarchy[interval]) {
    stop('The interval in the datetime variable is equal to the interval given,
you might be looking for pad rather than for thicken.')
  }

  # The smearing is done at POSIXct level, if applicable later converted back to Date
  start_val   <- min(dt_var)
  end_val     <- max(dt_var) %>% as.POSIXct
  if(lubridate::is.Date(dt_var)) {
    start_val <- as.POSIXct(start_val); end_val <- as.POSIXct(end_val)
    lubridate::hour(start_val) <- lubridate::hour(end_val) <- 0
  }
  end_val_seq <- seq(end_val, by = get_interval(dt_var), length.out = 2)[2]
  if(interval == 'day') smear_int <- 'DSTday' else smear_int <- interval
  smeared     <- seq(start_val, end_val_seq, by = smear_int)
  smeared     <- smeared[-length(smeared)]
  original    <- smeared

  if(int_hierarchy[interval_dt_var] < 6) lubridate::second(original) <- 0
  if(int_hierarchy[interval_dt_var] < 5) lubridate::minute(original) <- 0
  if(int_hierarchy[interval_dt_var] < 4) lubridate::hour(original)   <- 0
  if(int_hierarchy[interval_dt_var] < 3) lubridate::day(original)    <- 1
  if(int_hierarchy[interval_dt_var] < 2) lubridate::month(original)  <- 1

  if(lubridate::is.Date(dt_var)) original <- as.Date(original) + 1
  if(int_hierarchy[interval] < 4) smeared <- as.Date(smeared) + 1

  join_frame <- data.frame(original, smeared)

  if(!is.data.frame(x)){
    return(join_frame)
  } else {
    colnames(original_data_frame)[colnames(original_data_frame) ==
                                    dt_var_name] <- 'original'
    return_frame <- dplyr::inner_join(original_data_frame, join_frame)
    colnames(return_frame)[colnames(return_frame) == 'original'] <- dt_var_name
    return(return_frame)
  }
}
