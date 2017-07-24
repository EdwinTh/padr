# strategy 2: regular spanning and subsetting
spanned <- span_time("2016", "2017", interval = "hour", tz = "EST")
spanned <- span_date(20160101, 2018)

pattern_list <- list(hour = c(7:19, 22),
                     wday = 1:6)

irreg_span <- function(spanned,
                       pattern_list){
  original_type <- class(spanned)
  spanned_lt    <- as.POSIXlt(spanned)
  parts         <- names(pattern_list)
  check_filter_on(parts)
  if ("year" %in% parts) pattern_list <- adjust_year(pattern_list)
  subset_function <- make_subset_function(parts)
}

check_filter_on <- function(x) {
  if (!any(x %in% c("year", "mon", "mday", "wday", "hour", "min", "sec"))) {
    stop("invalid names in the span_list", call. = FALSE)
  }
}

adjust_year <- function(pattern_list) {
   pattern_list$year - 1900
}

filter_one_part <- function(spanned,
                            pattern_list,
                            part) {
  part_quo <- rlang::enquo(part)

  pattern <- pattern_list[[part]]
  spanned$txt
  part

}

filter_subset <- function(spanned,
                          pattern_list) {

}
