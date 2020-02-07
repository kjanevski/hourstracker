library(readxl)
library(tidyverse)

#read merged ht data
raw_entries <-  read_excel("raw/ht_merged_file.xlsx") %>% 
  rename_all(tolower)

#read morning logs
morning_logs <- read_excel("raw/Master_MLOG_Data.xlsx") %>% 
  rename_all(tolower) %>% 
  rename('study_day' = "study day", "id" = "code_name") %>% 
  select(id,study_day,try_to_sleep_dechour) %>% 
  mutate(study_day = study_day-1, try_to_sleep_dechour)


ht_data <- raw_entries %>% 
  left_join(morning_logs, by = c("id","study_day"))



#pivot data
dat <- ht_data %>% pivot_longer(cols =c(starts_with("screen"), starts_with("act")),
                                names_to = c(".value","time"),
                                names_pattern = "(.*)_(.*)") %>% 
  #convert the time to numeric value 13-29
  mutate(ampm = gsub("[[:digit:]]","",time)) %>% 
  mutate(time = as.numeric(gsub("\\D","",time))) %>% 
  mutate(time = if_else(ampm == "pm", time+12, time + 24)) %>% 
  mutate(time = if_else(time == 36, time - 12, time)) %>% 
  select(-ampm) %>% 
  #create "entry_adj"
  mutate(entry_adj = time - floor(try_to_sleep_dechour)) %>% 
  mutate(screen_mins = ifelse(activity == 0, NA, screen_mins))
