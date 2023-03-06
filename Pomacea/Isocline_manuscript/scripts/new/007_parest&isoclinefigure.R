#### this script is to build isocline, estimate the parameters,  plot
#the parameters on the isocline, plot the different reproductive conditions used
#to build the isocline
#---------------
####libraries
#---------------

library(lme4)
library(lmerTest)
library(MuMIn)

#-----------------------------
####read in the threshold data from script 002####
#----------------------------

threshold.dataM2 <- read_csv(here("Pomacea/Isocline_manuscript/data","threshold100_data_2size.csv"))
threshold.dataM3 <- read_csv(here("Pomacea/Isocline_manuscript/data","threshold100_deep_data_2size.csv"))
threshold.dataopt<- read_csv(here("Pomacea/Isocline_manuscript/data","threshold100_optimal_data_2sizes.csv"))

#####load in environmental used to create isocline data #####

environment_data <- read_xlsx(here("Pomacea/Isocline_manuscript/data","EnviroData_DBHYDRO_SnailModel.xlsx"), sheet = 3)   

environment_data <- environment_data %>% 
  mutate(Depth_M2_cm = (Depth_M2_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         Depth_M4_cm = (Depth_M4_ft - 13.5)*12*2.54,     #convert to cm above deep slough depth
         mon = months(environment_data$Date))        

#-------------------------------
####for loops to find thresholds####
#-------------------------------

#M2

ks <- as.character(seq(from = 0.01, to =  0.09, by = 0.005))

isodat_M2 <- tibble(k = as.character(ks),
                   Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_M2$k)) {
  
  temp <- threshold.dataM2 %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_M2$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}

#m3

isodat_M3 <- tibble(k = as.character(ks),
                    Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_M3$k)) {
  
  temp <- threshold.dataM3 %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_M3$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}

#optimal

isodat_opt <- tibble(k = as.character(ks),
                    Sint = rep(0, times = length(ks)))

for (i in 1:length(isodat_M2$k)) {
  
  temp <- threshold.dataopt %>% 
    filter(k == ks[i]) 
  
  fit <- lm(rs ~ log(Sint), data = temp)
  
  isodat_opt$Sint[i] <- exp(-fit$coefficients[1]/fit$coefficients[2])
  
}

#------------------------------------
###empirical estimates of parameters####
#------------------------------------

###growth parameters old estimates

#fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata[growthdata$Season == "dry",])
#model_summ <- summary(fit_me)  #results suggest a nonsignificant effect on the treatment

#dry_est <- model_summ$coefficients[1,1]

#dry_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

#wet season LILA
#fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata[growthdata$Season == "wet",])
#model_summ <- summary(fit_me)

#wet_est <- model_summ$coefficients[1,1]

#wet_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

#combined LILA
#fit_me <- lmer(SGR ~ Start_mm + (1|Cage), data = growthdata)
#model_summ <- summary(fit_me)

#comb_est <- model_summ$coefficients[1,1]

#comb_CI <- as_tibble(confint(fit_me,parm = "(Intercept)",level = 0.95))

LILA_k <- growthdata %>% 
  filter(Treatment == "0%_exposure") %>% 
  drop_na(k) %>% 
  group_by(Season) %>% 
  summarise(k_ave = mean(k, na.rm = T),
            n_obs = n(),
            k_sd = sd(k, na.rm = T)) %>% 
  mutate(k_se = k_sd/ sqrt(n_obs),
         upp = k_ave + (1.96*k_se),
         low = k_ave - (1.96*k_se))

LILA_k_comb <- growthdata %>% 
  filter(Treatment == "0%_exposure") %>% 
  drop_na(k) %>% 
  summarise(k_ave = mean(k, na.rm = T),
            n_obs = n(),
            k_sd = sd(k, na.rm = T)) %>% 
  mutate(k_se = k_sd/ sqrt(n_obs),
         upp = k_ave + (1.96*k_se),
         low = k_ave - (1.96*k_se))

####WCA Growth Rate###
#regression
growthvsTPregression <- lmer(Growth_length~species + TP + species:TP + (1| year),
                             data = growth.summ)

summary(growthvsTPregression)

b0 <- -0.0414728+0.3267086
b1 <- 0.0026002-0.0022298

#WCA old estimate

#WCA_TP_par <- WCA_TP %>% 
#  group_by(Site) %>% 
#  summarise(n_obs = n(),
#            TP_ave = mean(TP),
#            sd_ave = sd(TP)) %>% 
#  mutate(se = sd_ave/(sqrt(n_obs)),
#         upp = TP_ave + 1.96*se,
#         low = TP_ave - 1.96*se,
#         k = b0+b1*TP_ave,
#         k_upp = b0+b1*upp,
#         k_low = b0+b1*low,
#         season = "wet",
#         Site = if_else(Site == "WCA2", true = "WCA02",
#                        false = if_else(Site == "WCA3",
#                                        true = "WCA03",
#                                        false = "error")))
  

#combine all estimates into one

k <- tibble(Site = c(rep("LILA", times = 3),WCA_TP_par$Site),
             Season = c("dry","wet","Combined",WCA_TP_par$season),
             k = c(LILA_k$k_ave,LILA_k_comb$k_ave,0.058528084,0.048985688),
             k_upp =as.double(c(LILA_k$upp,LILA_k_comb$upp, 0.059445,0.050099)),
             k_low = as.double(c(LILA_k$low,LILA_k_comb$low,0.057604,0.047855)))
k

######CJS survival estimates####

CJS <- tetherdata %>%
  filter(length < 10) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m"),
         Site = if_else(wetland == "M2"|wetland == "M4",
                        true = "LILA", false = wetland)) %>% 
  group_by(Site,season, fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(Site,season) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4))))) %>% 
  filter(fate == "s") %>% 
  dplyr::select(-fate,-sum.fate,-tot)

CJS_LILA_Combined <-tetherdata %>%
  filter(length < 10) %>% 
  mutate(count = 1,
         fate = if_else(fate == "a", true = "s",
                        false = "m"),
         Site = if_else(wetland == "M2"|wetland == "M4",
                        true = "LILA", false = wetland)) %>% 
  filter(Site == "LILA") %>% 
  group_by(Site, fate) %>% 
  summarise(sum.fate = sum(count, na.rm = T)) %>% 
  group_by(Site) %>% 
  mutate(tot = sum(sum.fate),
         CJS = sum.fate/tot,
         upp = (CJS + (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         low = (CJS - (1.96 * sqrt(((1 - CJS)*CJS)/(tot + 4)))),
         season = "Combined") %>% 
  filter(fate == "s") %>% 
  dplyr::select(-fate,-sum.fate,-tot)

CJS_LILA_Combined

CJS<-CJS %>% 
  bind_rows(CJS_LILA_Combined)%>% 
  mutate(predator = "present") %>% 
  rename(CJS_upp = upp,
         CJS_low = low)

CJS

#####CJS w/o predators####

CJS_encl <- enclosuredata %>% 
  group_by(season) %>% 
  summarise(ave = mean(proportion),
            n = n(),
            sd = sd(proportion),
            se = sd/sqrt(n),
            upp = ave + (1.96*se),
            low = ave - (1.96*se),
            upp = if_else(upp >1, true = 1.00, false = upp),
            Site = "LILA") %>% 
  select(Site, season,ave,upp,low)

CJS_encl_comb <-enclosuredata %>% 
  summarise(ave = mean(proportion),
            n = n(),
            sd = sd(proportion),
            se = sd/sqrt(n),
            upp = ave + (1.96*se),
            low = ave - (1.96*se),
            upp = if_else(upp >1, true = 1.00, false = upp),
            Site = "LILA",
            season = "Combined") %>% 
  select(Site,season,ave,upp,low)

CJS_encl_comb

CJS_encl <- CJS_encl %>% 
  bind_rows(CJS_encl_comb) %>% 
  mutate(predator = "absent") %>% 
  rename(CJS = ave,
         CJS_upp = upp,
         CJS_low = low)

CJS_encl

CJS <- CJS %>% 
  bind_rows(CJS_encl)

CJS

k <- k %>% 
  rename(season = Season)

#combine all inot one file called parameters####

parameter <- CJS %>% 
  left_join(k, by = c("Site","season"))

parameter

#------------------------------------
####create the isocline####
#------------------------------------

p20 <- isodat_M2 %>% 
  ggplot(aes(x = Sint, y  = as.numeric(k)))+
 # geom_point(aes(x = (0.987*.987), y = 0.05), size = 3, shape = 21, color = "black",
 #            fill = "dark red")+
  geom_smooth(color = "#333333", size = 1.5, linetype = 1, se = F)+
  geom_smooth(data = isodat_opt, color = "#999999", linewidth = 1.5, linetype = 1, se = F)+
  geom_smooth(data = isodat_M3, color = "black", linewidth = 1.5, linetype = 1, se = F)+
  geom_pointrange(data = parameter, aes(x = CJS, y = k, ymin = k_low, ymax = k_upp,
                                        color = season, shape = Site), show.legend = F)+
  geom_pointrange(data = parameter, aes(x = CJS, y = k, xmin = CJS_low, xmax = CJS_upp,
                                        color = season, shape = Site),show.legend = F)+
  geom_point(data = parameter, aes(x = CJS, y = k,color = season, shape = Site), size = 3)+
  theme_classic()+
  coord_flip()+
  scale_color_manual(values = c("darkolivegreen","tan4","steelblue4"))+
  scale_y_continuous(breaks = c(0.01,0.03,0.05,0.07,0.09))+
  scale_x_continuous(limits = c(0.65,1.04), breaks = c(0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.00))+
  labs(x ="Cumulative Juvenile Survival", y ="Growth (k)")+
  annotate(geom = "text", x = 0.73, y = 0.018, label = "Declining", size = 8)+
  annotate(geom = "text", x = 0.925, y = 0.075, label = "Increasing", size = 8)+
  #annotate(geom = "text", x = 0.993, y = 0.05, label = "(model parameters)",
  #       size = 3, color = "dark red")+
  annotate(geom = "text", x = 1.02, y = 0.039, label = "without predators")+
  annotate(geom = "text", x = 0.88, y = 0.038, label = "with")+
  annotate(geom = "text", x = 0.86, y = 0.038, label = "predators")+
  theme(legend.position = c(0.52,0.13),
        legend.box = "horizontal")
  
#environmental data plot

p21 <- environment_data %>% 
  gather(Depth_Opt,Depth_M2_cm, Depth_M1_cm, key = "scenario", value = "Depth_cm") %>% 
  ggplot(aes(x = Date, y = Depth_cm, color = scenario))+
  geom_line(linewidth = 1.5, linetype = 1, show.legend = T)+
  labs(y = "Depth (cm)")+
  theme_classic()+
  scale_color_manual(values = c("black","#333333","#999999"),
                     labels = c("Natural-poor","Natural-good","Optimized"))+
  theme(legend.position = c(0.25,0.8),
        legend.title = element_blank())


p22 <- environment_data %>% 
  gather(Temp_nat,Temp_opt, key = "scenario", value = "Temp_c") %>% 
  ggplot(aes(x = Date, y = Temp_c, color = scenario))+
  geom_line(linewidth = 1.5, linetype = 1)+
  scale_color_manual(values = c("black","#999999"),
                     labels = c("Natural","Optimized"))+
  theme_classic()+
  labs(y = "Temperature (\u00B0C)")+
  theme(legend.position = c(0.5,0.3),
        legend.title = element_blank())

patch.isocline <- (p21/p22)|p20
patch.isocline.annotate<- patch.isocline +
  plot_annotation(tag_levels = "A",tag_suffix = ")")

patch.isocline.annotate

ggsave(here("Pomacea/Isocline_manuscript/out","fig4_isocline.png"),
       patch.isocline.annotate, device = ragg::agg_png,
       units = "in", width = 12, height = 6)
