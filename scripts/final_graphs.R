# Screen Mins Plot

par(mar=c(7,4,4,2))
boxplot(screen_mins ~ activity, las =2, xlab = "",
        ylab = "Screen Time (minutes/60)",
        data = dat %>% 
          filter(study_day %in% seq(0,14)) %>% 
          mutate(activity = recode(activity, "0" = "In bed for night",
                                   "1" = "TV",
                                   "2" = "Phone",
                                   "3" = "HW",
                                   "4" = "Other",
                                   "5" = "Other",
                                   "6" = "Socializing",
                                   "7" = "Other",
                                   "8" = "Social Media",
                                   "9" = "Computer",
                                   "10" = "Commuting",
                                   "11" = "Other",
                                   "12" = "Other",
                                   "13" = "Other",
                                   "14" = "Nap",
                                   "15" = "Other",
                                   "16" = "Video Games",
                                   "999" = "Download")) %>% 
          mutate(activity = factor(activity, levels = c("Computer",
                                                        "Video Games",
                                                        "Social Media",
                                                        "TV",
                                                        "Phone",
                                                        "HW",
                                                        "Commuting",
                                                        "Socializing",
                                                        "Other",
                                                        "Download"))))







#Activities Plot - removed Download


with(dat %>% filter(activity != 0) %>% filter(study_day %in% seq(0,14)) %>% 
       mutate(activity = recode(activity, "0" = "In bed for night",
                                "1" = "TV",
                                "2" = "On Phone",
                                "3" = "Homework",
                                "4" = "Eating",
                                "5" = "Hygiene",
                                "6" = "Socializing",
                                "7" = "Other",
                                "8" = "Social Media",
                                "9" = "Computer",
                                "10" = "Commuting",
                                "11" = "Chores",
                                "12" = "Work",
                                "13" = "Shopping",
                                "14" = "Napping",
                                "15" = "Other",
                                "16" = "Video Games",
                                "999" = "Download")) %>% 
       mutate(activity = factor(activity, levels = c("On Phone",
                                                     "TV",
                                                     "Commuting",
                                                     "Homework",
                                                     "Socializing",
                                                     "Other",
                                                     "Computer",
                                                     "Eating",
                                                     "Hygiene",
                                                     "Napping",
                                                     "Chores",
                                                     "Social Media",
                                                     "Video Games",
                                                     "Work",
                                                     "Shopping"
                                                     ))) %>% 
       group_by(activity) %>% 
       summarise(n = n()),
     barplot(n ~ activity, las = 2, xlab = "", ylab = "", ylim=range(0,1000)))



#Screen Mins - Time

boxplot(screen_mins ~ time, dat %>% 
          filter(study_day %in% seq(0,14)) %>%
          filter(time %in% seq(15,27)) %>% 
          mutate(time = factor(time, levels = seq(15,27), labels = c("3PM","4PM","5PM","6PM", "7PM", "8PM", "9PM",
                                                               "10PM", "11PM", "12AM", "1AM","2AM","3AM"))),
          ylab = "Screen Time (minutes/60)", xlab = "", las=2)

dat %>% 
  filter(study_day %in% seq(0,14)) %>% 
  filter(activity != 0) %>% 
  group_by(time) %>% 
  summarise(n = n())



#HeatMap W83
ggplot(dat_dlmo %>% filter(id == "W83", activity != 0), 
      aes(factor(study_day), time, fill = screen_mins)) +
  geom_tile()+
  geom_segment(aes(y = baseline_dlmo, yend = baseline_dlmo, x=.5,xend=14.5), size = 1)+
  geom_text(aes(label = activity, size = 2))+
  geom_point(aes(y = ttst,  color = "Selected Bedtime"))+
  coord_flip()+
  scale_y_continuous(breaks = seq(15, 28, 1), labels = c("3PM","4PM","5PM","6PM", "7PM", "8PM", "9PM",
                                                         "10PM", "11PM", "12AM", "1AM","2AM","3AM", "4AM"))+
  labs(x = "Study Day",y = "")+  
  scale_color_manual(name="", values= c("red"))+
  scale_fill_continuous(name = "Screen Time\n(minutes/60)")+
  scale_size_continuous(name = "", labels = "Activity")+
  theme_classic()+
  annotate("rect", xmin = .5, xmax = 15.2, ymin = 19.5, ymax = 24.5, alpha = .2)+
  annotate("text",label = "DLMO",x = 14.9, y = 21.8)



#HeatMap W83 - constrained
ggplot(dat_dlmo %>% filter(id == "W83", activity != 0, time >= 20), 
       aes(factor(study_day), time, fill = screen_mins)) +
  geom_tile()+
  geom_segment(aes(y = baseline_dlmo, yend = baseline_dlmo, x=.5,xend=14.5), size = 1)+
  geom_text(aes(label = activity, size = 2))+
  geom_point(aes(y = ttst, x = study_day,  color = "Selected Bedtime"))+
  coord_flip()+
  scale_y_continuous(breaks = seq(20, 28, 1), labels = c("8PM", "9PM",
                                                         "10PM", "11PM", "12AM", "1AM","2AM","3AM", "4AM"))+
  labs(x = "Study Day",y = "")+  
  scale_color_manual(name="", values= c("red"))+
  scale_fill_continuous(name = "Screen Time\n(minutes/60)")+
  scale_size_continuous(name = "", labels = "Activity")+
  theme_classic()+
  annotate("rect", xmin = .5, xmax = 15.2, ymin = 19.5, ymax = 24.5, alpha = .2)+
  annotate("text",label = "DLMO",x = 14.9, y = 21.8)
