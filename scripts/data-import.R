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
  mutate(study_day = study_day)


ht_data <- morning_logs %>% 
  left_join(raw_entries, by = c("id","study_day"))



#pivot data
dat <- ht_data %>%
  pivot_longer(cols =c(starts_with("screen"), starts_with("act")),
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



#import melatonin data
dat_dlmo <- read_excel("raw/ht_melatonin.xlsx") %>% 
  mutate(baseline_dlmo = baseline_dlmo+24) %>% 
  left_join(dat, by = "id")

#import ttst data, ignore W88, just baseline days
dat_dlmo <- read_excel("raw/ht_ttst.xlsx") %>% 
  left_join(dat_dlmo, by =c("id","study_day")) %>% 
  filter(study_day %in% c(0:14)) %>% 
  filter(id != "W88") %>% 
  mutate(activity = factor(activity))

#create "dlmo_adj"
dat_dlmo <- dat_dlmo %>% 
  mutate(dlmo_adj = time - floor(baseline_dlmo)) %>% 
  mutate(dlmo_group =  cut(baseline_dlmo, b = c(17,20.357,21.3,27), labels = c("early","middle","late")))
