#this code is for the analysis of the predator artifact data

#---------------------------------------------------
####libraries####
#---------------------------------------------------

library(patchwork)
library(jpeg)
library(ggpubr)

#---------------------------------------------------
#data management####
#---------------------------------------------------

#artifact data

mort_summary <- tetherdata %>% 
  filter(length < 10) %>% 
  filter(fate != "a") %>% 
  filter(fate != "d") %>% 
  mutate(count = 1) %>% 
  group_by(season, fate) %>% 
  summarise(n = sum(count)) %>% 
  mutate(n.tot =sum(n),
         perc.contr = n/n.tot*100,
         prop.contr = n/n.tot,
         upp = (prop.contr + (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = (prop.contr - (1.96 * sqrt(((1 - prop.contr)*prop.contr)/(n.tot + 4))))*100,
         low = if_else(low > 0, true = low, false = 0.00))


#predator data

invpredatorsumm <- predatordata %>% 
  mutate(year = year (`D.O.C`),
         juvcray = if_else(SpeciesCode == "Profal" & StandLen_mm < 14, true = 0,
                           false = 1)) %>% 
  filter(juvcray != 0) %>% 
  drop_na(`D.O.C`) %>% 
  filter(SiteCode == "M4" | SiteCode == "M2") %>% 
  filter(Phylum == "Arthropoda") %>% 
  mutate(count = 1) %>% 
  group_by(Season,SpeciesCode) %>% 
  summarise(count= sum(count)) %>%
  ungroup() %>% 
  complete(Season,SpeciesCode, fill = list(count = 0)) %>% 
  group_by(Season) %>% 
  mutate(total = sum(count),
         prop = count/total,
         percent = prop*100,
         upp = (prop + (1.96 * sqrt(((1 - prop)*prop)/(total + 4))))*100,
         low = (prop - (1.96 * sqrt(((1 - prop)*prop)/(total + 4))))*100,
         upp = if_else(upp > 100, true = 100.00, false = upp),
         low = if_else(low < 0, true = 0.00, false = low))

#-----------------------------------------------------
####plot the data####
#-----------------------------------------------------

img_crush <- readJPEG(here("Pomacea/Isocline_manuscript/pics","crushed_cutout.jpg"), native = T)
img_empty <- readJPEG(here("Pomacea/Isocline_manuscript/pics","emptyshell.jpg"), native = T)
img_miss <- readJPEG(here("Pomacea/Isocline_manuscript/pics","missing.jpg"),native = T)

#artifact data

p6 <- mort_summary %>% 
  ggplot(aes(y = n, x = season))+
  geom_bar(aes(fill = fate), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  
  scale_fill_manual(values = c("#333333","#999999", "white"),
                    labels = c("crushed", "empty", "missing"))+
  theme(legend.title = element_blank(),
        legend.position = c(.75,.8))+
  labs(y = "Count",
       title = " ")

p6 + 
  inset_element(p = img_crush,
                left = 0.08,
                bottom = 0.28,
                right = 0.18,
                top = 0.38) +
  inset_element(p = img_crush,
                left = 0.54,
                bottom = 0.28,
                right = 0.64,
                top = 0.38)+
  inset_element(p = img_empty,
                left = 0.23,
                bottom = 0.98,
                right = 0.31,
                top = 1.06)+
  inset_element(p = img_empty,
                left = 0.68,
                bottom = 0.40,
                right = 0.76,
                top = 0.48)+
  inset_element(p = img_miss,
                left = 0.82,
                bottom = 0.40,
                right = 0.90,
                top = 0.48)+
  inset_element(p = img_miss,
                left = 0.37,
                bottom = 0.95,
                right = 0.45,
                top = 1.03)
