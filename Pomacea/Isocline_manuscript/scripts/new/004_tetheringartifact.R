#this code is for the analysis of the predator artifact data

#---------------------------------------------------
####libraries####
#---------------------------------------------------

library(patchwork)
library(jpeg)
library(png)

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

vertpredatorsumm <- predatordata %>% 
  mutate(year = year (D.O.C)) %>% 
  drop_na(D.O.C) %>% 
  filter(SiteCode == "M4" | SiteCode == "M2") %>% 
  filter(Phylum == "Chordata") %>% 
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

#artifact images

img_crush <- readJPEG(here("Pomacea/Isocline_manuscript/pics","crushed_cutout.jpg"), native = T)
img_empty <- readJPEG(here("Pomacea/Isocline_manuscript/pics","emptyshell.jpg"), native = T)
img_miss <- readJPEG(here("Pomacea/Isocline_manuscript/pics","missing.jpg"),native = T)

#artifact data

p5 <- mort_summary %>% 
  ggplot(aes(y = n, x = season))+
  geom_bar(aes(fill = fate), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  
  scale_fill_manual(values = c("steelblue4","darkolivegreen", "tan4"),
                    labels = c("crushed", "empty", "missing"))+
  theme(legend.title = element_blank(),
        legend.position = c(.75,.9))+
  labs(y = "Artifact Count",
       x = "Season",
       title = " ")+
  annotate(geom = "text", x = .5, y = 22, label = "A)", size = 6)

#put the images on the plot

patch.artifact <- p5 + 
  inset_element(p = img_crush,
                left = 0.06,
                bottom = 0.28,
                right = 0.20,
                top = 0.48) +
  inset_element(p = img_crush,
                left = 0.52,
                bottom = 0.28,
                right = 0.65,
                top = 0.48)+
  inset_element(p = img_empty,
                left = 0.21,
                bottom = 0.98,
                right = 0.33,
                top = 1.16)+
  inset_element(p = img_empty,
                left = 0.66,
                bottom = 0.40,
                right = 0.78,
                top = 0.58)+
  inset_element(p = img_miss,
                left = 0.81,
                bottom = 0.40,
                right = .92,
                top = 0.58)+
  inset_element(p = img_miss,
                left = 0.36,
                bottom = 0.95,
                right = 0.47,
                top = 1.13)

#invertebrate predator plot

 p6 <- invpredatorsumm %>% 
  ggplot(aes(y = count, x = Season))+
  geom_bar(aes(fill = SpeciesCode), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  scale_fill_manual(values = c("darkolivegreen","steelblue4"),
                    labels = c("Belostoma", "Crayfish"))+
  theme(legend.title = element_blank(),
        legend.position = c(.75,.8))+
  labs(y = "Predator Abundance (n)",
       x = "Season",
       title = " ")+
   annotate(geom = "text", x = .6, y = 24, label = "B)", size = 6)

#invert images
 
img_cray <- readJPEG(here("Pomacea/Isocline_manuscript/pics","crayfishcut.jpg"),native = T)
img_belo <- readJPEG(here("Pomacea/Isocline_manuscript/pics","belostomacut.jpg"),native = T)

#put images on plot

patchinvert <- p6 +
  inset_element(p = img_cray,
                left = 0.75,
                bottom = 0.25,
                right = 0.89,
                top = 0.37)+
  inset_element(p = img_cray,
                left = 0.30,
                bottom = .97,
                right = 0.44,
                top = 1.09)+
  inset_element(p = img_belo,
                left = 0.10,
                bottom = .72,
                right = 0.23,
                top = .87)+
  inset_element(p = img_belo,
                left = 0.56,
                bottom = 0.26,
                right = 0.69,
                top = 0.41)

#plot vertebrate predator counts

p7 <- vertpredatorsumm %>% 
  ggplot(aes(y = count, x = Season))+
  geom_bar(aes(fill = SpeciesCode), stat = "identity", color = "black",
           position = position_dodge())+
  theme_classic()+
  scale_fill_manual(values = c("tan4","tan2","tan"),
                    labels = c("Mayan Cichlid", "Redear Sunfish","Greater Siren"))+

  theme(legend.title = element_blank(),
        legend.position = c(.87,.8),
        legend.text = element_text(size = 7))+
  labs(y = "Predator Abundance (n)",
       title = " ")+
  annotate(geom = "text", x = .6, y = 100, label = "C)", size = 6)

#vertebrate predator pictures

img_mayan <- readPNG(here("Pomacea/Isocline_manuscript/pics","Mayan-Cichlid_transparent.png"),native = T)
img_redear <- readPNG(here("Pomacea/Isocline_manuscript/pics","Redear-Knockout-DR.png"),native = T)
img_siren <- readJPEG(here("Pomacea/Isocline_manuscript/pics","sirencut.jpg"),native = T)

#put pictures on plot

patchvert <- p7+
  inset_element(p = img_mayan,
                left = 0.52,
                bottom = .87,
                right = 0.67,
                top = .99)+
  inset_element(p = img_mayan,
                left = 0.06,
                bottom = .75,
                right = 0.21,
                top = .87)+
  inset_element(p = img_redear,
                left = 0.22,
                bottom = .07,
                right = 0.32,
                top = .19)+
  inset_element(p = img_redear,
                left = 0.68,
                bottom = .12,
                right = 0.78,
                top = .24)+
  inset_element(p = img_siren,
                left = 0.81,
                bottom = .09,
                right = 0.91,
                top = .21)+
  inset_element(p = img_siren,
                left = 0.36,
                bottom = .28,
                right = 0.46,
                top = .40)


#create pannel of the three plots with the artifact pannel on top

patch.predator <- patch.artifact / (patchinvert + patchvert)

#view the plot
patch.predator

#save to the out folder
ggsave(here("Pomacea/Isocline_manuscript/out","fig2_artifact.png"),
       patch.predator, device = ragg::agg_png,
       units = "in", width = 8, height = 6)
