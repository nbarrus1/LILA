#this code is dedicated to the daily survival probability using logistic regression for the
#tethering data.

#-----------------------------------
####libraries######
#-----------------------------------

library(patchwork)
library(jpeg)
library(grid)
library(ggpubr)

#-----------------------------------
####data summary####
#-----------------------------------

#all data of snails less than 10 mm

tether.summ <- tetherdata %>% 
  filter(length < 10) %>%
  mutate(count = 1) %>% 
  group_by(fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  mutate(n = sum(sum.fate),
         rate = sum.fate/n)

tether.summ #the NA is from a snail that we couldn't check its fate because it got
#pulled off by us

#look at the data by season

tether.summ <- tetherdata %>%
  filter(length < 10) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m")) %>% 
  group_by(season, fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(season) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))))

tether.summ

#look at the data by location

tether.summ <- tetherdata %>%
  filter(length < 10) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m")) %>% 
  group_by(wetland,season, fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(wetland,season) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))))

tether.summ

#-------------------------------------------------------------
#####figure one plot#####
#-------------------------------------------------------------

trans_img <- readJPEG(here("Pomacea/Isocline_manuscript/pics","TetheringTransect.jpg"))

p1 <- ggplot(iris, aes(Species, Sepal.Length))+
  background_image(trans_img)+
  theme(axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  labs(x =NULL, y = NULL)

tether_img <- readJPEG(here("Pomacea/Isocline_manuscript/pics","TetheredSnails.jpg"))

p2 <- ggplot(iris, aes(Species, Sepal.Length))+
  background_image(tether_img)+
  theme(axis.text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))+
  labs(x =NULL, y = NULL)

p3 <- tetherdata %>% 
  filter(wetland == "M2"|wetland=="M4") %>% 
  ggplot(aes(x = length, y = survival, linetype = season,color = season, fill = season)) +
  theme_classic()+
  geom_smooth( method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), show.legend = F)+
  geom_smooth(method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), fill = NA)+
  coord_cartesian(ylim =  c(0.55, 1.00))+
  labs(y = "Daily Survival Probability",
       x = "Shell Length (mm)")+
  scale_color_manual(values = c("palegreen3","darkolivegreen"))+
  scale_fill_manual(values = c("olivedrab1","olivedrab1"))+
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = c(.8,.3),
        legend.background = element_rect(colour = "black"))

p4 <- tetherdata %>% 
  filter(wetland == "M2"|wetland=="M4") %>% 
  ggplot(aes(x = length, y = survival, linetype = season, color = season, fill = season)) +
  theme_classic()+
  geom_smooth(method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), show.legend = F)+
  geom_smooth(method="glm", se=T, fullrange=TRUE, 
              method.args = list(family=binomial), fill = NA)+
  coord_cartesian(ylim =  c(0.55, 1.00),
                  xlim = c(3,15))+
  scale_x_continuous(breaks = c(3,6,9,12,15))+
  scale_color_manual(values = c("palegreen3","darkolivegreen"))+
  scale_fill_manual(values = c("olivedrab1","olivedrab1"))+
  labs(y = "Daily Survival Probability",
       x = "Shell Length (mm)")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = c(.8,.3),
        legend.background = element_rect(colour = "black"))


patch.survival <- (p1+p3)/(p2+p4)

patch.survival.annotate<- patch.survival+
  plot_annotation(tag_levels = "A",tag_suffix = ")")

ggsave(here("Pomacea/Isocline_manuscript/out","fig1_tethering.png"),
       patch.survival.annotate, device = ragg::agg_png,
       units = "in", width = 8, height = 6)
