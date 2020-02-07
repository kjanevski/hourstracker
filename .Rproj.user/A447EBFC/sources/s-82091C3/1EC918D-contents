library(lemon)

dat_dlmo <- read_excel("raw/ht_melatonin.xlsx") %>% 
  mutate(baseline_dlmo = baseline_dlmo+24) %>% 
  left_join(dat, by = "id")

dat_dlmo <- read_excel("raw/ht_ttst.xlsx") %>% 
  left_join(dat_dlmo, by =c("id","study_day"))

ggplot(dat_dlmo %>% 
         filter(study_day %in% c(0:14)) %>%
         mutate(group = recode(group, "1" = "Luigi", "2" = "Mario")) %>% 
         drop_na(), aes(factor(study_day), time, fill =  screen_mins)) +
  geom_tile() +
  geom_point(aes(y = ttst), color = "red" )+
  geom_hline(aes(yintercept = baseline_dlmo))+
  geom_text(aes(label = activity), size = 2)+
  scale_y_continuous(minor_breaks = seq(15, 30, 1))+
  coord_flip()+
  facet_rep_wrap(~group + id, repeat.tick.labels = 'all')
