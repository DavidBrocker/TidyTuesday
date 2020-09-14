library(friends)
library(dplyr,warn.conflicts = F)
library(tidytext)

dd <- friends::friends
# Who Spoke the Most?
dd_spoke_most <- 
  dd %>% 
  filter(speaker != "Scene Directions") %>% 
  group_by(speaker) %>% 
  count(sort=T) %>% 
  ungroup() %>% 
  slice(1:10)
# Most Lines Spoken?
dd_most_lines <- 
  dd %>% 
  group_by(season) %>% 
  summarise(most_lines=max(utterance))
# Rachel Wins!
Rachel_lines <- 
  dd %>% 
  filter(speaker=="Rachel Green") %>% 
  select(text) %>% 
  mutate(line=1:length(text))
# Most said words
w_stop <- 
  Rachel_lines %>% 
  unnest_tokens(word,text) %>% 
  group_by(word) %>% 
  count(sort=T)
# Most said (non-stop) words
w_o_stop <- 
  Rachel_lines %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
    group_by(word) %>% 
    count(sort=T)
# Who's name did she say the most?
Rachel_lines %>% 
  unnest_tokens(word,text) %>% 
  filter(word %in% c("joey","ross","monica","phoebe","chandler")) %>% 
  group_by(word) %>% 
  count(sort=T)



##### Split by Character ########
dd_spoke_most$speaker[1]

Rachel <- 
  dd %>% 
  filter(speaker==dd_spoke_most$speaker[1]) %>% 
  select(text) %>% 
  mutate(line=1:length(text)) %>% 
  unnest_tokens(word,text) %>% 
  filter(word %in% c("joey","ross","monica","phoebe","chandler")) %>% 
  group_by(word) %>% 
  count(sort=T) %>% 
  ungroup() %>% 
  mutate(name=rep("Rachel",5))
Phoebe <-  
  dd %>% 
  filter(speaker==dd_spoke_most$speaker[6])%>% 
  select(text) %>%
  mutate(line=1:length(text)) %>% 
    unnest_tokens(word,text) %>% 
    filter(word %in% c("joey","ross","monica","phoebe","chandler")) %>% 
    group_by(word) %>% 
    count(sort=T)%>% 
  ungroup() %>% 
  mutate(name=rep("Phoebe",5))
Ross <- 
  dd %>% 
  filter(speaker==dd_spoke_most$speaker[2])%>% 
  select(text) %>% 
  mutate(line=1:length(text)) %>% 
    unnest_tokens(word,text) %>% 
    filter(word %in% c("joey","ross","monica","phoebe","chandler")) %>% 
    group_by(word) %>% 
    count(sort=T)%>% 
  ungroup() %>% 
  mutate(name=rep("Ross",5))
Joey <- 
  dd %>% 
  filter(speaker==dd_spoke_most$speaker[5])%>% 
  select(text) %>% 
  mutate(line=1:length(text)) %>% 
    unnest_tokens(word,text) %>% 
    filter(word %in% c("joey","ross","monica","phoebe","chandler")) %>% 
    group_by(word) %>% 
    count(sort=T)%>% 
  ungroup() %>% 
  mutate(name=rep("Joey",5))
Chandler <- 
  dd %>% 
  filter(speaker==dd_spoke_most$speaker[3])%>% 
  select(text) %>% 
  mutate(line=1:length(text)) %>% 
    unnest_tokens(word,text) %>% 
    filter(word %in% c("joey","ross","monica","phoebe","chandler")) %>% 
    group_by(word) %>% 
    count(sort=T)%>% 
  ungroup() %>% 
  mutate(name=rep("Chandler",5))
Monica <- 
  dd %>% 
  filter(speaker==dd_spoke_most$speaker[4])%>% 
  select(text) %>% 
  mutate(line=1:length(text)) %>% 
    unnest_tokens(word,text) %>% 
    filter(word %in% c("joey","ross","monica","phoebe","chandler")) %>% 
    group_by(word) %>% 
    count(sort=T)%>% 
  ungroup() %>% 
  mutate(name=rep("Monica",5))

friends_dat <- rbind(Rachel,Monica,Joey,Ross,Phoebe,Chandler)

friends_dat <- 
  friends_dat %>% 
  select(name,word,n)

collapsibleTree::collapsibleTreeSummary(friends_dat,
                                 hierarchy = c("name","word"),
                                 root="F.R.I.E.N.D.S",
                                 attribute = "n")




