library(tidyverse)

placeholder <- tibble(dlmo = c(12,13,14,15,17,18,19,20,22,23,24,25), group = c(rep("early",4),rep("middle",4),rep("late",4)))
                      
ggplot(aes(y= dlmo, x= fct_reorder(group,dlmo), fill = group), data = placeholder) + 
  geom_boxplot() + 
  geom_jitter()+
  coord_flip() + 
  theme_classic()+
  theme(panel.background = element_rect(colour = "black", size=.7))+
  scale_fill_manual(values = c("#e41a1c","#377eb8","#4daf4a"))

placeholder2 <- tibble(screen_mins = rep(c("0-20","20-40","40-60"),3), group = c(rep("early",3),rep("middle",3),rep("late",3)), prop = c(c(.45,.2,.35),c(.3,.25,.42),c(.25,.3,.5)))

ggplot(aes(y = prop, x = screen_mins, fill = group, group = factor(group, levels = c("early","middle","late"))), data = placeholder2) + 
  geom_bar(stat = "identity", position = "dodge", colour="black")+
  scale_fill_manual(values = c("#e41a1c","#377eb8","#4daf4a"))+
  theme_classic()+
  theme(axis.ticks.x = element_blank(), panel.background = element_rect(colour = "black", size=.7)) +
  scale_y_continuous(limits=c(0, 0.6), expand = c(0,0))

  