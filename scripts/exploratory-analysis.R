theme_set(theme_bw())

with(dat %>% 
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
       group_by(activity) %>% 
       summarise(n = n()),
     barplot(n ~ activity, las = 2, xlab = ""))


hist(dat$screen_mins, breaks = 60)


ggplot(dat, aes(x = entry_adj, fill = factor(activity))) + geom_bar()


