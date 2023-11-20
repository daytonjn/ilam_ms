#set of functions that take a "by_change" input and output a data frame to be used as input for make_dam_file()

filter_nblobs <- function(by_change) { #data frame to use
  
    compare_df <- dplyr::filter(by_change, s>0 & s<2000000 & !is.na(s)) %>% #use group_by("pi")
    summarise(blob_size = round(mean(s, na.rm=TRUE),0),
              count = n(),
              sd = round(sd(s),0),
              lb = round(blob_size-1*sd, 0),
              ub = round(blob_size+2*sd, 0))
  
  by_change = by_change %>%
    cross_join(compare_df) %>% #use left_join(by = pi)
    mutate(s = replace(s, s>0 & s <= lb, NA)) %>%
    mutate(s = replace(s, s!=2000000 & s >= ub, NA))
  
  by_frame <- 
    by_change %>% ungroup() %>% 
    group_by(pi, ID, time, treatment) %>%
    summarize(n = length(s[!is.na(s)]), #number of blobs of size (s) != NA
              s = sum(s, na.rm = TRUE)) %>% #sum of blob sizes, NA removed
    mutate(n = ifelse(s == 0, 0, n)) %>% #if sum of blobs=0, then n<-0 (otherwise it'd be 1)
    distinct(pi, ID, time, treatment, n, s) #sanity check to remove any duplicates
  
  by_frame <-
    by_frame %>% ungroup() %>%
    mutate(s = replace(s, s==2000000, NA),
           n = replace(n, is.na(s), NA))
  
  by_frame <-
    by_frame %>% group_by(pi) %>%
    mutate(s = round((na.locf0(s, fromLast = TRUE) + na.locf0(s, fromLast = FALSE))/2,0),
           n = round((na.locf0(n, fromLast = TRUE) + na.locf0(n, fromLast = FALSE))/2,0)) %>% ungroup()
  
  by_frame <-
    by_frame %>% ungroup() %>%
    mutate(s = replace(s, is.na(s), 0),
           n = replace(n, is.na(n), 0))
  
  return(by_frame)
 }
