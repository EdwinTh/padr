library(padr)
library(dplyr)
library(lubridate)
gelfandize <- function(df_x, 
                       df_y, 
                       interval, 
                       join_type = c("inner", 
                                     "left", 
                                     "right", 
                                     "full", 
                                     "anti",
                                     "semi"),
                       additional_keys = NULL,
                       by_x = NULL,
                       by_y = NULL) {
  df_x <- thicken(df_x, interval = interval, colname = "join_me", by = by_x)
  df_y <- thicken(df_y, interval = interval, colname = "join_me", by = by_y)
  fun_name <- paste0(match.arg(join_type), "_join")
  join_keys <- c("join_me", additional_keys)
  ret <- do.call(eval(fun_name), list(x = df_x, y = df_y, by = join_keys))
  select(ret, -join_me)
}

x <- data_frame(time_x = ymd_hms("20180314 073133", "20180314 073151", "20180314 073312", "20180314 081221"),
                val_x  = 5:8)
y <- data_frame(time_y = ymd_hms("20180314 073151", "20180314 080433"),
                val_y  = c("a", "b"))
gelfandize(x, y, "min", "left")
gelfandize(x, y, "hour", "left")
gelfandize(x, y, "day", "left")
