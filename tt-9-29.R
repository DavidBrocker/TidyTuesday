library(tidytuesdayR)
library(tidytext)
library(dplyr,warn.conflicts = F)
library(stringr)
library(ggplot2)
library(tidyr)
library(genius)
library(forcats)
# Week 40 Not Available Yet
# Load Manually
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')
########### Beyonce ###########
# Remove Live Songs
beyonce_nl <- 
  beyonce_lyrics %>% 
  filter(!str_detect(song_name,"Live|live"))
# Single-Word
beyonce_nl %>% 
  unnest_tokens(word,line) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  count(sort=T)
# Bigram Count
beyonce_nl %>% 
  unnest_tokens(bigram,line,token = "ngrams", n=2) %>% 
  separate(bigram,c("word1","word2"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  na.omit() %>% 
  count(word1,word2,sort=T)
# Trigram Count
beyonce_nl %>% 
  unnest_tokens(trigram,line,token = "ngrams", n=3) %>% 
  separate(trigram,c("word1","word2","word3"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  unite(trigram, word1,word2,word3 ,sep=" ") %>% 
  count(trigram,sort=T)
############# Taylor Swift ##########
# Single-Word
taylor_swift_lyrics %>% 
  unnest_tokens(word,Lyrics) %>% 
  anti_join(stop_words) %>% 
  group_by(word) %>% 
  count(sort=T)
# Bigram Count
taylor_swift_lyrics %>% 
  unnest_tokens(bigram,Lyrics,token = "ngrams", n=2) %>% 
  separate(bigram,c("word1","word2"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  na.omit() %>% 
  count(word1,word2,sort=T)
# Trigram Count
taylor_swift_lyrics %>% 
  unnest_tokens(trigram,Lyrics,token = "ngrams", n=3) %>% 
  separate(trigram,c("word1","word2","word3"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word) %>% 
  unite(trigram, word1,word2,word3 ,sep=" ") %>% 
  count(trigram,sort=T)
# Uniqueness: Taylor Swift
un_ts <- 
  taylor_swift_lyrics %>% 
  unnest_tokens(word,Lyrics) %>% 
  anti_join(stop_words) %>% 
  group_by(Album) %>% 
  summarise(total_w = length(word),
            unique_w = length(unique(word)),
            ttr = unique_w/total_w*100) %>% 
  ungroup()
# Uniqueness by Time
un_ts$Album <- factor(un_ts$Album,
                      levels = c("Taylor Swift",
                                 "Fearless",
                                 "Speak Now",
                                 "Red",
                                 "1989",
                                 "reputation",
                                 "Lover",
                                 "folklore"),
                      ordered = T) 
# Lyrics by Album
b1 <- genius_album("Beyonce","Dangerously in Love")
b2 <- genius_album("Beyonce","B'Day")
b3 <- genius_album("Beyonce","I Am... Sasha Fierce")
b4 <- genius_album("Beyonce","4")
b5 <- genius_album("Beyonce","Beyoncé")
b6 <- genius_album("Beyonce","Lemonade")
# Add Album Column
b1$Album <- rep("Dangerously in Love",length(line))
b2$Album <- rep("B'Day",length(line))
b3$Album <- rep("I Am... Sasha Fierce",length(line))
b4$Album <- rep("4",length(line))
b5$Album <- rep("Beyoncé",length(line))
b6$Album <- rep("Lemonade",length(line))
# Combine
beyonce_lyr <- 
  rbind(
    b1,b2,b3,b4,b5,b6
)

# Beyonce Factor
beyonce_lyr$Album <- 
  factor(beyonce_lyr$Album,
         levels = c("Dangerously in Love",
                    "B'Day",
                    "I Am... Sasha Fierce",
                    "4",
                    "Beyoncé",
                    "Lemonade"),
         ordered = T)
# Beyonce Uniqueness by Album 
un_b <- 
  beyonce_lyr %>% 
  unnest_tokens(word,lyric) %>% 
  anti_join(stop_words) %>% 
  group_by(Album) %>% 
  summarise(total_w = length(word),
            unique_w = length(unique(word)),
            ttr = unique_w/total_w*100) %>% 
  ungroup()
# Combine both datasets
combined <- 
  rbind(un_ts,un_b)
# Make Artist Category
combined$Artist <- c(rep("Taylor Swift",8),
                     rep("Beyonce",6)
)
# Rearrange
combined %>% 
  select(Artist,Album,total_w,unique_w,ttr)
# Combined Palette
comb_pal <- c("#2C5F2D","#97BC62FF")
# Combined Plot
comb_plt <- 
  combined %>% 
  ggplot(aes(fct_rev(Album),ttr,fill=Artist))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(x="Album Title\n\n",y="\n\nUniqueness (Type Token Ratio)\n Unique Words/Total Words",
       title = "\n Lyrical Uniqueness: Taylor Swift vs. Beyonce",
       subtitle="\n Who Run the World?",
       caption="Visual: @DaveBrocker \n Source: @Rosie_Baillie_ | @sastoudt")+
  theme(plot.title.position = "plot",
        panel.background = element_rect(fill="black"),
        plot.background = element_rect(fill="black"),
        axis.text = element_text(color="white"),
        plot.title = element_text(color="white"),
        plot.subtitle=element_text(color="white",face = "italic"),
        plot.caption = element_text(color="gold"),
        plot.caption.position = "plot",
        axis.title = element_text(color="white"),
        legend.background = element_rect(fill="black"),
        legend.text = element_text(color="white"))+
  scale_fill_manual(values=comb_pal)+
  coord_flip()+
  ylim(0,54)

ggsave("tidy_tues_929.jpeg",device = "jpeg")






