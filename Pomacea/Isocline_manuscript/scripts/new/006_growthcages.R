####this code is for the analysis of the growth data from the in situ mesocosms

#------------------------------
#libraries####
#------------------------------

library(lme4)
library(lmerTest)
library(MuMIn)

#--------------------------------------
#####analysis####
#--------------------------------------

####growth rate parameters old way####

#dry season LILA

fit_me <- lmer(SGR ~ Start_mm*Code + (1|Cage), data = growthdata[growthdata$Season == "dry",])
model_summ <- summary(fit_me)  #results suggest a nonsignificant effect on the treatment

dry_est <- model_summ$coefficients[1,1]

dry_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

#wet season LILA
fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata[growthdata$Season == "wet",])
model_summ <- summary(fit_me)

wet_est <- model_summ$coefficients[1,1]

wet_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

#combined LILA
fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata)
model_summ <- summary(fit_me)

comb_est <- model_summ$coefficients[1,1]

comb_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))



#Wet season TP
t.test(TP_data$TP[TP_data$Treatment == "0%_exposure"],
       TP_data$TP[TP_data$Treatment == "17%_exposure"])

#remove outliers
t.test(TP_data$TP[TP_data$Treatment == "0%_exposure"],
       TP_data$TP[TP_data$Treatment == "17%_exposure" & TP_data$Cage != 5])  

#remove outliers
t.test(TP_data$TP[TP_data$Treatment == "0%_exposure"],
       TP_data$TP[TP_data$Treatment == "17%_exposure" & TP_data$Cage != 5 & TP_data$Cage != 3])  

####growth rate parameters Correct way#####

growthdata %>% 
  #filter(Treatment == "0%_exposure") %>% 
  ggplot(aes(y = SGR, x = Length_Mean, color = Season))+
  geom_point()+
  geom_smooth(method = "lm",se = F)+
  facet_wrap(~Season)+
  theme_classic()


no_exposure_wet <- growthdata %>% 
  filter(Treatment == "0%_exposure")


fit <- lm(SGR~Length_Mean* Season, data = growthdata)

summary(fit)
#-------------------------------
####plots exposure treatment results ####
#-------------------------------

#####dry season w/ exposure treatment####

p11 <- growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "dry") %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = if_else(Treatment == "100%_exposure",
                                             true = "100% exposure",
                                             false = if_else(Treatment == "11%_exposure",
                                                             true = "11% exposure",
                                                             false = "22% exposure")))) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = "Starting Length (mm)")+
  scale_y_continuous(limits = c(0.000,0.015),
                     breaks = c(0.000,0.005,0.010,0.015))+
  scale_x_continuous(limits = c(3.0,10.0),
                     breaks = c(2.5,5.0,7.5,10.0))+
  scale_color_manual(values = c("black", "#333333","#666666", "black"))

#####wet season exposure treatment#####

######TP data

p12 <-TP_data %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Treatment, y = TP))+
  theme_classic()+
  geom_boxplot()+
  labs(x = NULL,
       title = "A)")+
  annotate(geom = "text", label = expression(paste(italic("t"), " = -0.367")), x = "0% exposure", y = 400)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.727")), x = "0% exposure", y = 365)

#with out cage 5 outlier
p13 <- TP_data %>% 
  filter(Cage != 5) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Treatment, y = TP))+
  theme_classic()+
  geom_boxplot()+
  labs(x = NULL,
       title = "C)")+
  annotate(geom = "text", label =expression(paste(italic("t"), " = 1.384")), x = "0% exposure", y = 50)+
  annotate(geom = "text", label =expression(paste(italic("p"), " = 0.204")), x = "0% exposure", y = 35)

#without cage 5 and cage 3 outliers

p14 <- TP_data %>% 
  filter(Cage != 5 & Cage != 3) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Treatment, y = TP))+
  theme_classic()+
  labs(title = "E)")+
  geom_boxplot()+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 0.914")), x = "17% exposure", y = 160)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.390")), x = "17% exposure", y = 150)


####growth data

#all combined

p15 <-growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "wet") %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = NULL,
       title = "B)")+
  scale_y_continuous(limits = c(0.001,0.025),
                     breaks = c(0.005,0.010,0.015,0.020,0.025))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("black", "#333333","#666666"))+
  theme(legend.position = c(0.8,0.85),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.background = element_rect(colour = "black", linetype = "solid"))+
  annotate(geom = "text", label = "interaction:", x = 5.0 , y =0.006)+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 1.270")), x = 5.0, y = 0.004)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.212")), x = 5.0, y = 0.002)

##w/o cage 5

p16<-growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "wet") %>% 
  filter(Cage != 5) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = NULL,
       title = "D)")+
  scale_y_continuous(limits = c(0.001,0.025),
                     breaks = c(0.005,0.010,0.015,0.020,0.025))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("black", "#333333","#666666"))+
  theme(legend.position = c(0.8,0.85),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.background = element_rect(colour = "black", linetype = "solid"))+
  annotate(geom = "text", label = "interaction:", x = 5.0 , y =0.006)+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 2.089")), x = 5.0, y = 0.004)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.044")), x = 5.0, y = 0.002)

#w/o cage 5 and 3

p17 <- growthdata %>% 
  filter(Fate == "alive") %>% 
  filter(Season == "wet") %>% 
  filter(Cage !=5 & Cage != 3) %>% 
  mutate(Treatment = if_else(Treatment == "0%_exposure",
                             true = "0% exposure",
                             false = "17% exposure")) %>% 
  ggplot(aes(x = Start_mm, y = SGR, fill = Treatment,
             linetype = Treatment,
             shape = Treatment))+
  geom_smooth(method = "lm", se =T, fill = "#999999", color = "black",
              show.legend = F)+
  geom_point(aes(color = Treatment))+
  theme_classic()+
  labs(y = "SGR",
       x = "Starting Length (mm)",
       title = "F)")+
  scale_y_continuous(limits = c(0.001,0.025),
                     breaks = c(0.005,0.010,0.015,0.020,0.025))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("black", "#333333","#666666"))+
  theme(legend.position = c(0.8,0.85),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 8),
        legend.background = element_rect(colour = "black", linetype = "solid"))+
  annotate(geom = "text", label = "interaction:", x = 5.0 , y =0.006)+
  annotate(geom = "text", label = expression(paste(italic("t"), " = 1.402")), x = 5.0, y = 0.004)+
  annotate(geom = "text", label = expression(paste(italic("p"), " = 0.170")), x = 5.0, y = 0.002)

patch.exposure <- (p12 + p15)/(p13+ p16)/(p14+p17)

patch.exposure.annotate <- patch.exposure +
  plot_layout(guides = 'collect')

patch.exposure.annotate

#-------------------------------------
####wet season vs dry season comparison####
#-------------------------------------

###plot the grwoth data

p18 <- growthdata %>% 
  filter(Fate == "alive") %>% 
  ggplot(aes(x = Start_mm, y = SGR))+
  geom_smooth(aes(color = Season, fill = Season),
              method = "lm", se =T,
              show.legend = T)+
  #geom_smooth(method = "lm", fill = "#999999", color = "black", show.legend = F)+
  geom_point(aes(color = Season))+
  theme_classic()+
  labs(y = "SGR (protional daily growth)",
       x = "Starting Length (mm)")+
  scale_y_continuous(limits = c(0.001,0.03),
                     breaks = c(0.005,0.010,0.015,0.020,0.025,0.03))+
  scale_x_continuous(limits = c(3.0,13.2),
                     breaks = c(2.5,5.0,7.5,10.0,12.5))+
  scale_color_manual(values = c("tan4","steelblue4"))+
  scale_fill_manual(values = c("tan","steelblue1"))+
#  annotate(geom = "text", label = "dry: y = -0.0004x + 0.0111", y = 0.030, x =11.0)+
 # annotate(geom = "text", label = "wet: y = -0.0010x + 0.0207", y = 0.028, x =11.0)+
 # annotate(geom = "text", label = "combined: y = -0.0007x + 0.0159", y = 0.026, x =10.52)+
  theme(legend.position = c(.75,.75))

###plot the temperature data for context

p19 <- TEMP %>% 
  filter(type != "air_WestPalm") %>% 
  ggplot(aes(x = season, y = temp.c,fill = season, color = season))+
  theme_classic()+
  geom_jitter(show.legend = F, alpha = .8)+
  geom_boxplot(alpha = .2, show.legend = F)+
  scale_color_manual(values = c("tan4","steelblue4"))+
  scale_fill_manual(values = c("tan4","steelblue4"))+
  labs(x = "Season", y = "Water Temperature (\u00B0C)")

#combine the plots  

patch.growth <- p19 + p18

#annotate

patch.growth.annotate <- patch.growth+
  plot_annotation(tag_levels = "A",tag_suffix = ")")
  
#save the plots

ggsave(here("Pomacea/Isocline_manuscript/out","fig3_growth.png"),
       patch.growth.annotate, device = ragg::agg_png,
       units = "in", width = 8, height = 4)
