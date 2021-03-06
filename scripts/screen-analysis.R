
#Screen mins by activity
par(mar=c(7,4,4,2))
boxplot(screen_mins ~ activity, las =2, data = dat %>% 
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
                                   "999" = "Download")))


#Screen mins by clock time
boxplot(screen_mins ~ time, dat, las = 2)



#Fit LM
screen_fit1 <- lm(screen_mins ~ time, data = dat)
summary(screen_fit1)

plot(jitter(screen_mins,5) ~ jitter(time), col=alpha(rgb(0,0,0), 0.1), xlab = "Time", ylab = "Screen Mins/60", data = dat)
abline(screen_fit1)

plot(screen_fit1)



hist(screen_mins, breaks = 60)
