#set of functions that take a "by_frame" input and output a data frame in DAM format for export
#as a tab-delimited DAM Monitor file. 
#Choose whether you want to use nblobs or size

#Data columns 11-42 correspond to individuals 1-32
add_columns_to_42 <- function(df) {
  current_ncol <- ncol(df)
  while (current_ncol < 42) {
    new_col_name <- paste0("X", current_ncol + 1 - 10)
    df <- df %>% mutate(!!new_col_name := 0)
    current_ncol <- current_ncol + 1
  }
  return(df)
}

make_dam_file <- function(by_frame, #data frame to use
                     variable_name = "s"){ #store sum of blobs (s) or nblobs ? 
  
  dam_output <- tibble(date = by_frame$time,
                    status = 1, #1=valid data, 51=no data received
                    extras = 0, 
                    monitor = 42, #monitor number
                    tube_no = 0, 
                    data = "Ct", #counts total
                    blank = 0, #unused
                    sensor = ifelse(by_frame$treatment=="L",1,0), #1=lights-on, 0=lights-off
                    version = by_frame$pi,
                    counts = dplyr::pull(by_frame, variable_name)) %>%
    pivot_wider(names_from = version, 
                values_from = counts, 
                values_fill = 0) %>%
    separate(date, sep = -8, into = c("date","time")) %>%
    separate(date, into = c(NA, NA, "day"), extra = "drop") %>%
    add_column(index = 1:(nrow(by_frame)/length(unique(by_frame$pi))), .before = "day") %>%
    tibble::add_column(month = str_sub(months(unique(by_frame$time)), 0, 3),
                       year = str_sub(year(unique(by_frame$time)),-2), .before = "time") %>%
    unite("date", day:year, sep = " ")
  
  dam_output <- add_columns_to_42(dam_output)
  
  return(dam_output)
 }
