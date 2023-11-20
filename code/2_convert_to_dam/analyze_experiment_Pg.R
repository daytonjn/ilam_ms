#Working script to process by_change_Pg dataframe
library(behavr)
library(damr)
library(scopr)
library(sleepr)
library(ggetho)
library(zeitgebr)

#1. Read in iLAM output from image_analysis and parse into a dataframe containing the size, centroid location, and origin (time, pi) of all identified blobs (by_change) with ```parse_movements()```

out_file_name = "Pgreeni_v4"
start_photophase = 5 #project-specific, time that lights turn on
end_photophase = 21 #project-specific, time that dark starts

#parse movements into workable dfs: all movements (by_change) and
by_change_Pg <- parse_movements(file_mvmnts = paste0(out_file_name,".csv"),
                             start_photophase = start_photophase,
                             end_photophase = end_photophase)

#2. Collapse all identified blobs (within by_change_[]) into a dataframe containing the sum of blobs and number of blobs/movements observed per frame/timepoint (by_frame_[])
by_frame_Pg <- 
  by_change_Pg %>% ungroup() %>% 
  group_by(pi, ID, time, treatment) %>%
  summarize(n = length(s[!is.na(s)]), #number of blobs of size (s) != NA
            s = sum(s, na.rm = TRUE)) %>% #sum of blob sizes, NA removed
  mutate(n = ifelse(s == 0, 0, n)) %>% #if sum of blobs=0, then n<-0 (otherwise it'd be 1)
  distinct(pi, ID, time, treatment, n, s) #sanity check to remove any duplicates

```

#3. Filter movements captured from rare, noisy images.

by_frame_Pg <-
  by_frame_Pg %>% ungroup() %>%
   mutate(s = replace(s, s==2000000, NA),
          n = replace(n, is.na(s), NA))

by_frame_Pg <-
  by_frame_Pg %>% group_by(pi) %>%
  mutate(s = round((na.locf0(s, fromLast = TRUE) + na.locf0(s, fromLast = FALSE))/2,0),
         n = round((na.locf0(n, fromLast = TRUE) + na.locf0(n, fromLast = FALSE))/2,0)) %>% ungroup()

by_frame_Pg <-
  by_frame_Pg %>% ungroup() %>%
   mutate(s = replace(s, is.na(s), 0),
          n = replace(n, is.na(n), 0))

dplyr::filter(by_frame_Pg, n>0) %>% group_by(pi) %>%
  summarise(blob_size = mean((s/n), na.rm=TRUE)) #output the average blob size


#4. Use iLAM wrapper ```make_dam_file()``` to convert by_frame_[] into a DAM-like output and save as a DAM file with summed blob sizes/timepoint

ilam_Pg = make_dam_file(by_frame_Pg, variable_name = "s") 

#output a tab-delimited file containing 
write_tsv(ilam_Pg, "ilam_Pg.tsv", col_names = F)