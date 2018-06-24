# library(padr)
# library(dplyr)
#library(lubridate)
#thicken_join <- function(df_x, 
#                       df_y, 
#                       interval, 
#                       join_type = c("inner", 
#                                     "left", 
#                                     "right", 
#                                     "full", 
#                                     "anti",
#                                     "semi"),
#                       additional_keys = NULL,
#                       by_x = NULL,
#                       by_y = NULL) {
#  df_x <- thicken(df_x, interval = interval, colname = "join_me", by = by_x)
#  df_y <- thicken(df_y, interval = interval, colname = "join_me", by = by_y)
#  fun_name <- paste0(match.arg(join_type), "_join")
#  join_keys <- c("join_me", additional_keys)
#  ret <- do.call(eval(fun_name), list(x = df_x, y = df_y, by = join_keys))
#  select(ret, -join_me)
#}

