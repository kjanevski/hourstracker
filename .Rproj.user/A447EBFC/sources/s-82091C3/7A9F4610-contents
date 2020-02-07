theme_set(theme_bw())

ggplot(dat, aes(x = factor(activity))) + geom_bar()

ggplot(dat, aes(screen_mins)) + geom_bar()


# Lots of NA's
dat %>% filter(!complete.cases(screen_mins)) %>% 
  view


ggplot(dat, aes(x = entry_adj, fill = factor(activity))) + geom_bar()



