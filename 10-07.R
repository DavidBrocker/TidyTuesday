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

bball %>% 
  filter(tourney_finish=="Champ") %>% 
  group_by(school,year) %>% 
  ggplot(aes(school,conference))

bball %>% 
  mutate(pts = case_when(
    seed==1~100,
    seed==2~72.7
    seed==3~54.5
    seed==4~48.5
    seed==5~33.3
    seed==6~33.3
    seed==7~27.3
    seed==8~18.2
    seed==9~18.2
    seed==10~18.2
    seed==11~18.2
    seed==12~15.2
    seed==13~9.09
    seed==14~6.06
    seed==15~3.03
    seed==16~0
  ))



bball %>% 
  mutate(home_game=
           ifelse(x1st_game_at_home=="Y",1,0)
         ) %>% 
  filter(tourney_finish=="Champ") %>% 
  select(school,home_game,tourney_finish) %>% 
  group_by(school,home_game) %>% 
  count(sort=T) %>% 
  ggplot(aes(fct_reorder(school,n),n,fill=home_game)) +
    geom_bar(stat="identity")+
  coord_flip()

bball %>% 
  filter(tourney_finish=="Champ") %>% 
  group_by(school) %>% 
  count(sort=T)





conn <- bball %>% 
  filter(school=="UConn")

conn$label <- rep(emoji("basketball"),30)

conn %>%   
ggplot(aes(year,tourney_w,label=label)) +
  geom_hline(yintercept=6,linetype=2,color="green")+
  geom_point()+
  theme_clean()+
  geom_text(family="OpenSansEmoji", 
            size=5,color="orange")+
  labs(title="UConn WNBA Tournament Outcomes\n\n",
       x="\nTournament Wins",
       y="\n\nYear",
       caption = "Visual: @DaveBrocker \n Source: @FiveThirtyEight")+
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="black"),
    axis.text = element_text(color="white"),
    plot.subtitle = element_text(color="white"),
    plot.title = element_text(color="white"),
    plot.caption = element_text(color="gold",face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.title = element_text(color="white")
  )

idk <- paste0(conn$year,"-01-01")

as.Date(idk,format = "%y")



idkset.seed(123)
x <- rnorm(10)
set.seed(321)
y <- rnorm(10)
plot(x, y, cex=0)
text(x, y, labels=emoji('basketball'), cex=1.5, col='orange', family='OpenSansEmoji')









