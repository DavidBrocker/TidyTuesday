library(tidytuesdayR)
library(stringr)
library(dplyr,warn.conflicts = F)
library(tidytext)
library(ggplot2)
library(forcats)
library(ggthemes)
library(emoG)
bball <- tidytuesdayR::tt_load('2020-10-06')

bball <- bball$tournament


bball_pal <- c("#FFFFFF","#A4C8E1")

bball %>% 
  mutate(First_Game_Home=x1st_game_at_home,
         First_Game_Home=ifelse(First_Game_Home=="Y","Yes","No")) %>% 
  filter(tourney_finish=="Champ") %>% 
  group_by(school,First_Game_Home) %>% 
  count(sort=T) %>% 
  ggplot(aes(fct_reorder(school,n),n,fill=First_Game_Home))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="Tournaments Won",
       subtitle="There's No Place Like Home!\n",
       x="\nTournament Wins",
       y="Win Count",
       caption = "Visual: @DaveBrocker \n Source: @FiveThirtyEight")+
  theme_clean()+
  theme(
    panel.background = element_rect(fill="#000E2F"),
    plot.background = element_rect(fill="#000E2F"),
    axis.text = element_text(color="white"),
    plot.subtitle = element_text(color="white",size = 10),
    plot.title = element_text(color="white"),
    plot.caption = element_text(color="gold",face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.title = element_text(color="white"),
    legend.background = element_rect(fill="#000E2F",color = NA),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white")
  )+
  guides(fill=guide_legend(title="First Game at Home?"))+
  scale_fill_manual(values=bball_pal)


ggsave("wnba.jpeg",device="jpeg")






















