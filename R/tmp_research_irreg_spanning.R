# strategy 2: regular spanning and subsetting
spanned <- span_time("2016", "2017", interval = "hour", tz = "EST")

span_list <- list(hour = c(7:19, 22),
                  wday = 1:6)

irreg_span <- function(spanned,
                       span_list){
  check_filter_on(names(span_list))
  subset_function <- make_subset_function(span_list)
}

check_filter_on <- function(x) {
  if (!any(x %in% c("year", "mon", "mday", "wday", "hour", "min", "sec"))) {
    stop("invalid names in the span_list", call. = FALSE)
  }
}


func_list <- list(
  hour = function(x)
)
