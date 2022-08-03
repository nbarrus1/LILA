rm(list = ls())  #clears global environment

####Important!!! packages that I have below will need to be installed if not installed 
####previously

library(plot3D)          #packaged to create 3D regression plane
library(MASS)            #model fitting package contains
library(lme4)            #model fitting package for mixed effect models GLMM models 
library(lmerTest)        #package that gets model summary information more readable
library(qpcR)            #            
library(lemon)           #package for creating more readable plot pannels
library(cowplot)         #package that puts multiple plots together
library(ggpubr)          #adds important ggplot features neccesary for publishing

#---------------------------------------------------------------------------------
####load in Data####
#---------------------------------------------------------------------------------

####load in LILA egg counts####

#line 31 -> read in our excel formatted data using sheet two 
#line 32 -> create a new column labelled year with all the years
#line 33 -> create a new column labelled month to get all months
#line 34 -> create a column of dayofyear formatted from the start of january indicated by %j


#-------------------------------------------------------------------------------------
####look at covariates before modelling ####
#-------------------------------------------------------------------------------------

cordata <- summ.LILA.egg %>% 
  ungroup() %>% 
  dplyr::select(ave.depth.cm,depth.change,sd.depth.cm,Season,pom.dens,ave.temp.c, photoperiod) %>% 
  mutate(Season = if_else(Season == "dry",
                          true = 0,
                          false = 1))


cordata <- as.matrix(cordata)
cormatrix <- cor(cordata)
cormatrix

pairs(cordata)

hist(summ.LILA.egg$ave.depth.cm)
hist(summ.LILA.egg$sd.depth.cm)
hist(summ.LILA.egg$depth.change)
plot(table(summ.LILA.egg$Season))
hist(summ.LILA.egg$ave.temp.c)



summ.LILA.egg %>% 
  ggplot(aes(x = depth.change, y = count, color = Pomacea_Species, shape = Pomacea_Species))+
  theme_classic()+
  geom_point()+
  geom_line()+
  facet_wrap(~Cell)

library(mgcv)


summ.LILA.egg %>% 
  filter(Pomacea_Species == "Paludosa") %>% 
  mutate(temp.group = if_else(ave.temp.c < 25.42, true = "temp < 25.4",
                              false =  "temp > 25.4")) %>% 
  ggplot(aes(x = ave.depth.cm, y = count, color = temp.group, shape = temp.group))+
  theme_classic()+
  geom_point(size = 3)+
  geom_smooth(aes(linetype = temp.group), method = "glm.nb", formula = y ~ x + I(x^2),
              color = "black", fill = "#999999", show.legend = F)+
  scale_color_manual(values = c("black","#666666"))+
  scale_x_continuous(limits = c(0,max(summ.LILA.egg$ave.depth.cm)), breaks = c(0,15,30,45,60,75,90))+
  labs(x = "Water Depth (cm)")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

summ.LILA.egg %>% 
  filter(Pomacea_Species == "Paludosa") %>% 
  mutate(depth.group = if_else(ave.depth.cm < 41.8, true = "depth < 41.8",
                              false =  "depth > 41.8")) %>% 
  ggplot(aes(x = ave.temp.c, y = count, color = depth.group))+
  theme_classic()+
  geom_point()+
  geom_smooth(method = "glm.nb", formula = y ~ x)

summ.LILA.egg %>% 
  filter(Pomacea_Species == "Paludosa") %>% 
  ggplot(aes(x = pom.dens, y = count))+
  theme_classic()+
  geom_point()+
  geom_smooth(method = "glm.nb", formula = y ~ x)

summ.LILA.egg %>% 
  filter(Pomacea_Species == "Maculata") %>% 
  mutate(photo.group = if_else(photoperiod < 12.93, true = "photo < 12.93",
                              false =  "photo > 12.93")) %>%
  ggplot(aes(x = ave.depth.cm, y = count))+
  theme_classic()+
    geom_rect(aes(xmin=42, xmax=Inf, ymin=-Inf, ymax=+Inf), 
            fill="grey", alpha = .01)+
  geom_point(pch = 21, fill = "#666666", size = 2)+
  geom_smooth(method = "glm.nb", formula = y ~ x + I(x^2), color = "black", fill = "#666666", size = 1)+
  labs(x = "Water Depth (cm)")+

  scale_x_continuous(limits = c(0,max(summ.LILA.egg$ave.depth.cm)), breaks = c(0,15,30,45,60,75,90))+
  scale_y_continuous(limits = c(0,max(summ.LILA.egg$count)), breaks = c(0,30,60,90,120,150))+
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))
  
ggsave(filename = "countvsdepth.png", plot = last_plot(), device = png, units = "in", width = 6, height = 6)

summ.LILA.egg %>% 
  filter(Pomacea_Species == "Maculata") %>% 
  ggplot(aes(x = pom.dens, y = count))+
  theme_classic()+
  geom_point()+
  geom_smooth(method = "glm.nb", formula = y ~ x)


#---------------------------------------------------------------------------------------
#####standardize all explanotory variables variables (for use in mixed effect modeling)#####
#---------------------------------------------------------------------------------------
summ.LILA.egg <- summ.LILA.egg %>% 
  ungroup() %>% 
  mutate(zavedepth = (ave.depth.cm - mean(ave.depth.cm, na.rm = TRUE))/ sd(ave.depth.cm, na.rm = T),
         zsddepth = (sd.depth.cm - mean(sd.depth.cm, na.rm = TRUE))/ sd(sd.depth.cm, na.rm = T),
         zavetemp = (ave.temp.c - mean(ave.temp.c, na.rm = TRUE))/ sd(ave.temp.c, na.rm = T),
         zdeltadepth = (depth.change - mean(depth.change, na.rm = TRUE))/ sd(depth.change, na.rm = T),
         zphotoperiod = (photoperiod - mean(photoperiod, na.rm = TRUE))/ sd(photoperiod, na.rm = T))


#------------------------------------------------------------------------------------------
####Summary Stats####
#------------------------------------------------------------------------------------------

f <- summ.LILA.egg 

f <- f %>% 
  ungroup() %>% 
  group_by(Pomacea_Species) %>% 
  summarise(n = n(),
            ave.depth = mean(ave.depth.cm, na.rm = T),
            sd.depth = sd(ave.depth.cm, na.rm = T),
            max.depth = max(ave.depth.cm, na.rm = T),
            min.depth = min(ave.depth.cm, na.rm = T),
            ave.dens = mean(pom.dens, na.rm = T),
            sd.dens = sd(pom.dens, na.rm = T),
            max.dens = max(pom.dens, na.rm = T),
            min.dens = min(pom.dens, na.rm = T),
            ave.temp = mean(ave.temp.c, na.rm = T),
            sd.temp = sd(ave.temp.c, na.rm = T),
            max.temp = max(ave.temp.c, na.rm = T),
            min.temp = min(ave.temp.c, na.rm = T),
            ave.sddepth = mean(sd.depth.cm, na.rm = T),
            sd.sddepth = sd(sd.depth.cm, na.rm = T),
            max.sddepth = max(sd.depth.cm, na.rm = T),
            min.sddepth = min(sd.depth.cm, na.rm = T),
            ave.change = mean(depth.change, na.rm = T),
            sd.change = sd(depth.change, na.rm = T),
            max.change = max(depth.change, na.rm = T),
            min.change = min(depth.change, na.rm = T),
            ave.photo = mean(photoperiod, na.rm = T),
            min.photo = min(photoperiod, na.rm = T),
            max.photo = max(photoperiod, na.rm = T),
            sd.photo = sd(photoperiod, na.rm = T))
f

M2.depth %>% 
  group_by(Season,year) %>% 
  summarise(depth.ave = mean(depth.cm, rm.na = T),
            n = n(),
            sd = sd(depth.cm))

f <- summ.LILA.egg 

f <- f %>% 
  dplyr::select(pom.dens, Pomacea_Species, Cell, year) %>% 
  ungroup() %>% 
  group_by(Pomacea_Species, Cell, year) %>% 
  summarise(ave.dens = mean(pom.dens, na.rm = T))

f


summary.egg <- summ.LILA.egg %>% 
  mutate(month = month(Date)) %>% 
  ungroup() %>% 
  group_by(Pomacea_Species, year, Cell) %>% 
  summarise(ave.count = mean(count, na.rm = T),
            sum.count = sum(count, na.rm = T),
            max.count = max(count, na.rm = T),
            n = n()) %>% 
  mutate(spp.count = sum(sum.count))

write_csv(summary.egg, file = "supplementarytable3.csv")

table(summ.LILA.egg$Pomacea_Species, summ.LILA.egg$Season)


#-----------------------------------------------------------------------------------------------------------
#####modelling with random effect of Cell#####
#-----------------------------------------------------------------------------------------------------------

#####paludosa#####
ppal.egg <- summ.LILA.egg %>% 
  ungroup() %>% 
  filter(Pomacea_Species == "Paludosa") %>% 
  mutate(zpaldens = (pom.dens - mean(pom.dens, na.rm = TRUE))/ sd(pom.dens, na.rm = T))

ppal.egg[,14:ncol(ppal.egg)]
pal.mefit1 <- glmer.nb(formula = count ~ zavedepth + (1|Cell)+ (1|year), data = ppal.egg)
summary(pal.mefit1)
pal.mefit2 <- glmer.nb(formula = count ~ zdeltadepth+ (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit3 <- glmer.nb(formula = count ~ zsddepth+ (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit4 <- glmer.nb(formula = count ~ zavedepth + zdeltadepth+ (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit5 <- glmer.nb(formula = count ~ zsddepth + zdeltadepth+ (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit6 <- glmer.nb(formula = count ~ zavedepth + zsddepth+ (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit7 <- glmer.nb(formula = count ~ zavedepth + zsddepth + zdeltadepth+ (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit8 <- glmer.nb(formula = count ~ zavedepth + I(zavedepth^2)+ (1|Cell)+ (1|year), data = ppal.egg)   ####singularity issues

tt <- getME(pal.mefit8,"theta")   ###check singularity
ll <- getME(pal.mefit8,"lower")  
min(tt[ll==0])                    ###shoun't have a problem with singularity
derivs1 <- pal.mefit8@optinfo$derivs                       ####check gradient
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))                                         ####shouldnt have issues try more interations
pal.mefit8.2 <- update(pal.mefit8,control=glmerControl(optCtrl=list(maxfun=2e4)))  ###fixed the problem
ss <- getME(pal.mefit8,c("theta","fixef"))                                                          ###start with previous theta
pal.mefit8.3 <- update(pal.mefit8,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###still has issues
pal.mefit8.4<- update(pal.mefit8,start=ss,control=glmerControl(optimizer="bobyqa",                 ###different optimzer works
                                                                 optCtrl=list(maxfun=2e5))) 

pal.mefit9 <- glmer.nb(formula = count ~ Season + (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit10 <- glmer.nb(formula = count ~ zavedepth*Season + (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit11 <- glmer.nb(formula = count ~ zdeltadepth*Season + (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit12 <- glmer.nb(formula = count ~ zsddepth*Season + (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit13 <- glmer.nb(formula = count ~ zavedepth*Season + I(zavedepth^2)*Season+ (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit14 <- glmer.nb(formula = count ~ zpaldens + (1|Cell)+ (1|year), data = ppal.egg)

tt <- getME(pal.mefit14,"theta")   ###check singularity
ll <- getME(pal.mefit14,"lower")  
min(tt[ll==0])                    ###shoun't have a problem with singularity
derivs1 <- pal.mefit14@optinfo$derivs                       ####check gradient
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))                                         ####shouldnt have issues try more interations

pal.mefit14.2 <- update(pal.mefit14,control=glmerControl(optCtrl=list(maxfun=2e4)))  ###still issues try starting at previous theta
ss <- getME(pal.mefit14,c("theta","fixef"))                                                          ###start with previous theta
pal.mefit14.3 <- update(pal.mefit14,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###still has issues
pal.mefit14.4<- update(pal.mefit14,start=ss,control=glmerControl(optimizer="bobyqa",                 ###different optimzer works
                                                                 optCtrl=list(maxfun=2e5)))  

pal.mefit15 <- glmer.nb(formula = count ~ zavedepth + zpaldens + (1|Cell)+ (1|year), data = ppal.egg)
pal.mefit16 <- glmer.nb(formula = count ~ zavedepth + zpaldens + Season + (1|Cell)+ (1|year), data = ppal.egg)

tt <- getME(pal.mefit16,"theta")   ###check singularity
ll <- getME(pal.mefit16,"lower")  
min(tt[ll==0])                    ###shoun't have a problem with singularity
derivs1 <- pal.mefit16@optinfo$derivs                       ####check gradient
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))                                         ####shouldnt have issues try more interations

pal.mefit16.2 <- update(pal.mefit16,control=glmerControl(optCtrl=list(maxfun=2e4)))  ###still issues try starting at previous theta
ss <- getME(pal.mefit16,c("theta","fixef"))                                                          ###start with previous theta
pal.mefit16.3 <- update(pal.mefit16,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###worked
pal.mefit16.4 <- update(pal.mefit16,start=ss,control=glmerControl(optimizer="bobyqa",                 ###different optimzer works
                                                                 optCtrl=list(maxfun=2e5)))  
pal.mefit16.5 <- update(pal.mefit16,start=ss,control=glmerControl(optimizer="Nelder_Mead",                 ###different optimzer works
                                                                  optCtrl=list(maxfun=2e5)))  
pal.mefit16.6 <- update(pal.mefit16,start=ss,control=glmerControl(optimizer="nlminbwrap",                 ###different optimzer works
                                                                  optCtrl=list(maxfun=2e5)))  
pal.mefit16.7 <- update(pal.mefit16,start=ss,control=glmerControl(optimizer="nloptwrap",                 ###different optimzer works
                                                                  optCtrl=list(maxfun=2e5,
                                                                               ftol_abs=1e-4,
                                                                               xtol_abs=1e-4)))         ###still has issues remove from list of models

environment(nloptwrap)$defaultControl

pal.mefit17 <- glmer.nb(formula = count ~ zpaldens + Season + (1|Cell), data = ppal.egg)

tt <- getME(pal.mefit17,"theta")   ###check singularity
ll <- getME(pal.mefit17,"lower")  
min(tt[ll==0])                    ###shoun't have a problem with singularity
derivs1 <- pal.mefit17@optinfo$derivs                       ####check gradient
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))                                         ####shouldnt have issues try more interations

pal.mefit17.2 <- update(pal.mefit17,control=glmerControl(optCtrl=list(maxfun=2e4))) #good


pal.mefit18 <- glmer.nb(formula = count ~ Season*zpaldens+ (1|Cell), data = ppal.egg)
pal.mefit19 <- glmer.nb(formula = count ~ zpaldens + zavedepth + I(zavedepth^2)+ (1|Cell), data = ppal.egg)
pal.mefit20 <- glmer.nb(formula = count ~ Season + zavedepth + I(zavedepth^2)+ (1|Cell), data = ppal.egg)
pal.mefit21 <- glmer.nb(formula = count ~ zpaldens + Season + zavedepth + I(zavedepth^2)+ (1|Cell), data = ppal.egg)
pal.mefit22 <- glmer.nb(formula = count ~ zpaldens + Season + zavedepth + I(zavedepth^2) +
                      Season*zavedepth + Season* I(zavedepth^2)+ (1|Cell), data = ppal.egg)
pal.mefit23 <- glmer.nb(formula = count ~ zavetemp + (1|Cell), data = ppal.egg)
pal.mefit24 <- glmer.nb(formula = count ~ zavetemp + zavedepth + (1|Cell), data = ppal.egg)
pal.mefit25 <- glmer.nb(formula = count ~ zavetemp + zavedepth +zavetemp*zavedepth+ (1|Cell), data = ppal.egg)
pal.mefit26 <- glmer.nb(formula = count ~ zavetemp + zavedepth + zpaldens + (1|Cell), data = ppal.egg)
pal.mefit27 <- glmer.nb(formula = count ~ zavetemp + zpaldens+ (1|Cell), data = ppal.egg)
pal.mefit28 <- glmer.nb(formula = count ~ zavetemp + zavedepth + I(zavedepth^2)+ (1|Cell), data = ppal.egg)
pal.mefit29 <- glmer.nb(formula = count ~ zavetemp + zpaldens + zavedepth + I(zavedepth^2)+ (1|Cell), data = ppal.egg)

pal.mefit30 <- glmer.nb(formula = count ~ zpaldens + zavetemp + zavedepth + I(zavedepth^2) +                          
                          zavetemp*zavedepth + zavetemp* I(zavedepth^2)+ (1|Cell), data = ppal.egg)  ###convergence issues needs fixed
ss <- getME(pal.mefit30,c("theta","fixef"))                                                          ###start with previous theta
pal.mefit30.2 <- update(pal.mefit30,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###still has issues
pal.mefit30.3<- update(pal.mefit30,start=ss,control=glmerControl(optimizer="bobyqa",                 ###different optimzer works
                                                       optCtrl=list(maxfun=2e5)))  

pal.mefit31 <- glmer.nb(formula = count ~ zavedepth*zavetemp + I(zavedepth^2)*zavetemp+ (1|Cell), data = ppal.egg)
pal.mefit32 <- glmer.nb(formula = count ~ zphotoperiod+ (1|Cell), data = ppal.egg)
pal.mefit33 <- glmer.nb(formula = count ~ zpaldens + zphotoperiod + zavedepth + I(zavedepth^2) +                                 #best model
                          zphotoperiod*zavedepth + zphotoperiod* I(zavedepth^2)+ (1|Cell), data = ppal.egg)

tt <- getME(pal.mefit33,"theta")   ###check singularity
ll <- getME(pal.mefit33,"lower")  
min(tt[ll==0])                    ###shoun't have a problem with singularity
derivs1 <- pal.mefit33@optinfo$derivs                       ####check gradient
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))                                         ####shouldnt have issues try more interations

pal.mefit33.2 <- update(pal.mefit33,control=glmerControl(optCtrl=list(maxfun=2e4)))  ###still issues try starting at previous theta
ss <- getME(pal.mefit33,c("theta","fixef"))                                                          ###start with previous theta
pal.mefit33.3 <- update(pal.mefit33,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###worked

pal.mefit34 <- glmer.nb(formula = count ~ zphotoperiod + zavedepth + I(zavedepth^2) +                                 
                          zphotoperiod*zavedepth + zphotoperiod* I(zavedepth^2)+ (1|Cell), data = ppal.egg )
pal.mefit35 <-  glmer.nb(formula = count ~zpaldens + zphotoperiod + zavedepth + I(zavedepth^2)+ (1|Cell), data = ppal.egg)

pal.mes <- c(pal.mefit1,pal.mefit2,pal.mefit3,pal.mefit4,pal.mefit5,pal.mefit6,pal.mefit7,pal.mefit8.2,pal.mefit9,pal.mefit10,
             pal.mefit11,pal.mefit12,pal.mefit13,pal.mefit14.4,pal.mefit15,pal.mefit17.2,pal.mefit18,pal.mefit19,pal.mefit20,
             pal.mefit21,pal.mefit22,pal.mefit23,pal.mefit24,pal.mefit25,pal.mefit26,pal.mefit27,pal.mefit28,pal.mefit29,pal.mefit30.3,
             pal.mefit31,pal.mefit32, pal.mefit33.3, pal.mefit34, pal.mefit35)


pal.AIC <- tibble(model = c(1:15,17:35),
  aic = sapply(pal.mes, AIC)) %>% 
  mutate(deltaAIC = aic-min(aic),
         w = akaike.weights(aic)$weights)

View(pal.AIC)

write_csv(pal.AIC, "pal_aic.csv")
#####maculata#####

pmac.egg <- summ.LILA.egg %>% 
  ungroup() %>% 
  filter(Pomacea_Species == "Maculata") %>% 
  mutate(zmacdens = (pom.dens - mean(pom.dens, na.rm = TRUE))/ sd(pom.dens, na.rm = T))

mac.mefit1 <- glmer.nb(formula = count ~ zavedepth + (1|Cell), data = pmac.egg)
mac.mefit2 <- glmer.nb(formula = count ~ zdeltadepth+ (1|Cell), data = pmac.egg,
                       control = glmerControl(optCtrl=list(maxfun=10000000)))  ###not converging must remove from list
mac.mefit3 <- glmer.nb(formula = count ~ zsddepth+ (1|Cell), data = pmac.egg)
mac.mefit4 <- glmer.nb(formula = count ~ zavedepth + zdeltadepth+ (1|Cell), data = pmac.egg)
mac.mefit5 <- glmer.nb(formula = count ~ zsddepth + zdeltadepth+ (1|Cell), data = pmac.egg,
                       control = glmerControl(optCtrl=list(maxfun=10000000))) ####not converging must remove from list

mac.mefit6 <- glmer.nb(formula = count ~ zavedepth + zsddepth+ (1|Cell), data = pmac.egg)
mac.mefit7 <- glmer.nb(formula = count ~ zavedepth + zsddepth + zdeltadepth+ (1|Cell), data = pmac.egg)
mac.mefit8<- glmer.nb(formula = count ~ zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)
mac.mefit9 <- glmer.nb(formula = count ~ Season + (1|Cell), data = pmac.egg)
mac.mefit10 <- glmer.nb(formula = count ~ zavedepth*Season + (1|Cell), data = pmac.egg)
mac.mefit11 <- glmer.nb(formula = count ~ zdeltadepth*Season + (1|Cell), data = pmac.egg)
mac.mefit12 <- glmer.nb(formula = count ~ zsddepth*Season + (1|Cell), data = pmac.egg)
mac.mefit13 <- glmer.nb(formula = count ~ zavedepth*Season + I(zavedepth^2)*Season+ (1|Cell), data = pmac.egg)
mac.mefit14 <- glmer.nb(formula = count ~ zmacdens + (1|Cell), data = pmac.egg)
mac.mefit15 <- glmer.nb(formula = count ~ zavedepth + zmacdens + (1|Cell), data = pmac.egg)
mac.mefit16 <- glmer.nb(formula = count ~ zavedepth + zmacdens + Season + (1|Cell), data = pmac.egg)
mac.mefit17 <- glmer.nb(formula = count ~ zmacdens + Season + (1|Cell), data = pmac.egg)
mac.mefit18 <- glmer.nb(formula = count ~ Season*zmacdens+ (1|Cell), data = pmac.egg)
mac.mefit19 <- glmer.nb(formula = count ~ zmacdens + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)
mac.mefit20 <- glmer.nb(formula = count ~ Season + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)
mac.mefit21 <- glmer.nb(formula = count ~ zmacdens + Season + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)
mac.mefit22 <- glmer.nb(formula = count ~ zmacdens + Season + zavedepth + I(zavedepth^2) +
                          Season*zavedepth + Season* I(zavedepth^2)+ (1|Cell), data = pmac.egg)

tt <- getME(mac.mefit22,"theta")   ###check singularity
ll <- getME(mac.mefit22,"lower")  
min(tt[ll==0])                    ###shoun't have a problem with singularity
derivs1 <- mac.mefit22@optinfo$derivs                       ####check gradient
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))                                         ####shouldnt have issues try more interations

mac.mefit22.2 <- update(mac.mefit22,control=glmerControl(optCtrl=list(maxfun=2e4)))  ###fixed it

mac.mefit23 <- glmer.nb(formula = count ~ zavetemp + (1|Cell), data = pmac.egg)
mac.mefit24 <- glmer.nb(formula = count ~ zavetemp + zavedepth + (1|Cell), data = pmac.egg)
mac.mefit25 <- glmer.nb(formula = count ~ zavetemp + zavedepth +zavetemp*zavedepth+ (1|Cell), data = pmac.egg)
mac.mefit26 <- glmer.nb(formula = count ~ zavetemp + zavedepth + zmacdens + (1|Cell), data = pmac.egg)
mac.mefit27 <- glmer.nb(formula = count ~ zavetemp + zmacdens+ (1|Cell), data = pmac.egg)
mac.mefit28 <- glmer.nb(formula = count ~ zavetemp + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)
mac.mefit29 <- glmer.nb(formula = count ~ zavetemp + zmacdens + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)
mac.mefit30 <- glmer.nb(formula = count ~ zmacdens + zavetemp + zavedepth + I(zavedepth^2) +
                          zavetemp*zavedepth + zavetemp* I(zavedepth^2)+ (1|Cell), data = pmac.egg)
tt <- getME(mac.mefit30,"theta")   ###check singularity
ll <- getME(mac.mefit30,"lower")  
min(tt[ll==0])                    ###shoun't have a problem with singularity
derivs1 <- mac.mefit30@optinfo$derivs                       ####check gradient
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))                                         ####shouldnt have issues try more interations

mac.mefit30.2 <- update(mac.mefit30,control=glmerControl(optCtrl=list(maxfun=2e4)))  ###still issues try starting at previous theta
ss <- getME(mac.mefit30,c("theta","fixef"))                                                          ###start with previous theta
mac.mefit30.3 <- update(mac.mefit30,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###still has issues
mac.mefit30.4 <- update(mac.mefit30,start=ss,control=glmerControl(optimizer="bobyqa",                 ###different optimzer works
                                                                 optCtrl=list(maxfun=2e5)))  

mac.mefit31 <- glmer.nb(formula = count ~ zavedepth*zavetemp + I(zavedepth^2)*zavetemp+ (1|Cell), data = pmac.egg)
mac.mefit32 <- glmer.nb(formula = count ~ zphotoperiod+ (1|Cell), data = pmac.egg)
mac.mefit33 <- glmer.nb(formula = count ~ zmacdens + zphotoperiod + zavedepth + I(zavedepth^2) +                                 #best model
                          zphotoperiod*zavedepth + zphotoperiod* I(zavedepth^2)+ (1|Cell), data = pmac.egg)
mac.mefit34 <- glmer.nb(formula = count ~ zphotoperiod + zavedepth + I(zavedepth^2) +                                 
                          zphotoperiod*zavedepth + zphotoperiod* I(zavedepth^2)+ (1|Cell), data = pmac.egg )
mac.mefit35 <-  glmer.nb(formula = count ~zmacdens + zphotoperiod + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)

mac.mes <- c(mac.mefit1,mac.mefit3,mac.mefit4,mac.mefit6,mac.mefit7,mac.mefit8,mac.mefit9,mac.mefit10,
             mac.mefit11,mac.mefit12,mac.mefit13,mac.mefit14,mac.mefit15,mac.mefit16,mac.mefit17,mac.mefit18,mac.mefit19,mac.mefit20,
             mac.mefit21,mac.mefit22.2,mac.mefit23,mac.mefit24,mac.mefit25,mac.mefit26,mac.mefit27,mac.mefit28,mac.mefit29,mac.mefit30.4,
             mac.mefit31, mac.mefit32, mac.mefit33, mac.mefit34, mac.mefit35)

mac.AIC <- tibble(model = c(1,3,4,6,7:35),
                  aic = sapply(mac.mes, AIC)) 
mac.AIC <- mac.AIC %>% 
  mutate(deltaAIC = aic-min(aic),
         w = akaike.weights(mac.AIC$aic)$weights)

write_csv(mac.AIC, "mac_aic.csv")
#--------------------------------------------------------------------------------------------
#######Best Model Exploration#########
#--------------------------------------------------------------------------------------------


###p. paludosa####
pal.mefit30 <- glmer.nb(formula = count ~ zpaldens + zavetemp + zavedepth + I(zavedepth^2) +                                 #best model
                          zavetemp*zavedepth + zavetemp* I(zavedepth^2)+ (1|Cell), data = ppal.egg)

summary(pal.mefit30)
summary(pal.mefit30.3)

pal.meNULL <- glmer.nb(formula = count~ 1 + (1|Cell), data = ppal.egg)

pal.pseudoR2 <- MuMIn::r.squaredGLMM(pal.mefit30.3, pal.meNULL)  ###trigamma is recommended
pal.pseudoR2 


ppal.egg <- ppal.egg %>% 
  mutate(predict.count = exp(predict(pal.mefit30.3)))

#make a 3d version of the interaction to see if that helps interpretation



grid.lines = 26
x.pred <- seq(min(ppal.egg$zavedepth), max(ppal.egg$zavedepth), length.out = grid.lines)
y.pred <- seq(min(ppal.egg$zavetemp), max(ppal.egg$zavetemp), length.out = grid.lines)
xy <- expand.grid( zavedepth = x.pred,  zavetemp = y.pred)
xy$Cell <- rep("M2", times = length(xy$zavedepth))
xy$zpaldens <- rep(0, times = length(xy$zavedepth))
xy$count <- exp(predict(pal.mefit30.3, newdata = xy, type = "link", allow.new.levels = TRUE))
table(is.na(xy))
z.pred <- matrix(exp(predict(pal.mefit30.3, newdata = xy, type = "link")), 
                 nrow = grid.lines, ncol = grid.lines)

(-0.6018598*sd(summ.LILA.egg$ave.depth.cm))+mean(summ.LILA.egg$ave.depth.cm)

(-2.2182418*sd(summ.LILA.egg$ave.temp.c))+mean(summ.LILA.egg$ave.temp.c)

(x.pred[19]*sd(summ.LILA.egg$ave.depth.cm))+mean(summ.LILA.egg$ave.depth.cm)
(y.pred[26]*sd(summ.LILA.egg$ave.temp.c))+mean(summ.LILA.egg$ave.temp.c)

sum(z.pred[4:14,1:10])/sum(z.pred)
(x.pred[4:14]*sd(summ.LILA.egg$ave.depth.cm))+mean(summ.LILA.egg$ave.depth.cm)
(y.pred[1:10]*sd(summ.LILA.egg$ave.temp.c))+mean(summ.LILA.egg$ave.temp.c)
# scatter plot with regression plane
par(mfrow = c(1, 1))
par(mai = c(0.4,0.4,0.4,0.4),
    cex.axis = 0.75)


depthtempinter3d <- scatter3D(xy$zavedepth, xy$zavetemp, xy$count+100, pch = NULL, cex = .0001, 
          theta = 160, phi = 20, ticktype = "detailed",col ="black",
           NAcol = "blue",colvar = xy$count,
          zlim = c(0,80), bty = "b2", 
          colkey = list(plot = FALSE), clim = c(0,80),
          xlab = "Depth (cm)", ylab = "Temp", zlab = "Count",
          surf = list(x = x.pred, y = y.pred, z = z.pred, 
                      facets = NA))
points3D(x = ppal.egg$zavedepth,y = ppal.egg$zavetemp, z = ppal.egg$count,
                             type = "h", add = TRUE, pch = 16, cex = 1.4,
         col = ramp.col(c("yellowgreen","darkgreen", "midnight blue")))

ggsave("depthtempinter3d.png", plot = depthtempinter3d, device = "png", width = 6.67, height = 7,
       units = "in")

par(mfrow = c(3, 3))
for (alph in c(0.25, 0.75))
  image2D(volcano, alpha = alph,
          main = paste("jet.col, alpha = ", alph))
image2D(volcano, main = "jet.col")
image2D(volcano, col = jet2.col(100), main = "jet2.col")
image2D(volcano, col = gg.col(100), main = "gg.col")
image2D(volcano, col = gg2.col(100), main = "gg2.col")
image2D(volcano, col = rainbow(100), main = "rainbow")
image2D(volcano, col = terrain.colors(100), main = "terrain.colors")
image2D(volcano, col = ramp.col(c("blue", "yellow", "green", "red")),
        main = "ramp.col")

mean(ppal.egg$ave.depth.cm)
sd(ppal.egg$ave.depth.cm)
mean(ppal.egg$ave.temp.c)
sd(ppal.egg$ave.temp.c)

 
####predicted versus actuals

ppal.egg %>%
  ggplot(aes(x = dayofyear, y = count))+
  facet_grid(Cell~as.character(year))+
  theme_classic()+
  geom_point()+
  geom_line(aes(x = dayofyear, y = count), color = "black")+
  geom_point(aes(x =dayofyear, y = predict.count), shape = 17, color = "#666666")+
  geom_line(aes(x=dayofyear, y = predict.count), color = "#666666", linetype = "dashed")+
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))

#####best model for P. maculata#####

mac.mefit35 <-  glmer.nb(formula = count ~zmacdens + zphotoperiod + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg)
summary(mac.mefit35)

mac.meNULL <-glmer.nb(formula = count ~ 1 + (1|Cell), data = pmac.egg)

mac.pseudoR2<-MuMIn::r.squaredGLMM(mac.mefit35, mac.meNULL) ###trigamma is recommended
mac.pseudoR2

####depth effect on P. maculata

depthinterdata <- data.frame(
  zavedepth = seq(from = min(pmac.egg$zavedepth), to = max(pmac.egg$zavedepth),
                      length.out = 1000),
  Cell = rep("M4", times = 1000),
  zmacdens = rep(0, times = 1000),
  zphotoperiod = rep (0, times = 1000))


depthinterdata <- cbind(depthinterdata, predict(mac.mefit35, depthinterdata, type = "link",
                                                allow.new.levels = TRUE)) 
depthinterdata$lncount.predict <- depthinterdata$`predict(mac.mefit35, depthinterdata, type = "link", allow.new.levels = TRUE)`
depthinterdata$count.predict <- exp(depthinterdata$lncount.predict)

depthinterdata.low<- data.frame(
  zavedepth.low = seq(from = min(pmac.egg$zavedepth), to = max(pmac.egg$zavedepth),
                  length.out = 1000),
  Cell.low = rep("M4", times = 1000),
  zmacdens.low = rep(-1.6338, times = 1000),
  zphotoperiod.low = rep (0, times = 1000))


depthinterdata.low <- cbind(depthinterdata, predict(mac.mefit35, depthinterdata, type = "link",
                                                allow.new.levels = TRUE)) 
depthinterdata.low$lncount.predict.low <- depthinterdata$`predict(mac.mefit35, depthinterdata, type = "link", allow.new.levels = TRUE)`
depthinterdata.low$count.predict.low <- exp(depthinterdata$lncount.predict)

depthinterdata %>% 
  ggplot(aes(x = zavedepth, y = count.predict)) +
  geom_line(size = 1.5)+
  geom_line(aes(x = zavedepth.low, y = count.predict.low),data = depthinterdata.low, color = "red")+
  geom_point(data = pmac.egg, aes(x = zavedepth, y = count))+
  theme_classic()+
  labs()+
  scale_x_continuous(breaks = c(-2, -1.5,-1,-0.5,0,0.5,1,1.5,2))+
  scale_y_continuous(limits = c(0,150),
    breaks = c(0,30,60,90,120,50))

pmac.egg <- pmac.egg %>% 
  mutate(predict.count = exp(predict(mac.mefit35)))

sum(depthinterdata$count.predict)

sum(depthinterdata$count.predict[depthinterdata$zavedepth > -0.1 & depthinterdata$zavedepth < 1.1])/
  sum(depthinterdata$count.predict)

sort((depthinterdata$zavedepth[depthinterdata$zavedepth > -0.1 & depthinterdata$zavedepth < 1.1]*
  sd(summ.LILA.egg$ave.depth.cm, na.rm = T))+mean(summ.LILA.egg$ave.depth.cm, na.rm = T))

(depthinterdata$zavedepth[depthinterdata$count.predict == max(depthinterdata$count.predict)]*
  sd(summ.LILA.egg$ave.depth.cm, na.rm = T))+mean(summ.LILA.egg$ave.depth.cm, na.rm = T)
####predicted versus actuals



pmac.egg %>%
  ggplot(aes(x = dayofyear, y = count))+
  facet_grid(Cell~as.character(year))+
  theme_classic()+
  geom_point()+
  geom_line(aes(x = dayofyear, y = count), color = "black")+
  geom_point(aes(x =dayofyear, y = predict.count), shape = 17, color = "#666666")+
  geom_line(aes(x=dayofyear, y = predict.count), color = "#666666", linetype = "dashed")+
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))


pmac.egg %>%
  ggplot(aes(x = dayofyear, y = zavetemp))+
  facet_grid(Cell~as.character(year))+
  theme_classic()+
  geom_point()+
  geom_line(aes(x = dayofyear, y = zavetemp), color = "black")+
  geom_point(aes(x =dayofyear, y = zavedepth), shape = 17, color = "#666666")+
  geom_line(aes(x=dayofyear, y = zavedepth), color = "#666666")+
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b"))+
  labs(y = "Z-score")

###leave one out cross validation####

#work flow

###set up data
ppal.egg <- ppal.egg %>% 
  mutate(predict.count = 0,
    obs = 1:length(ppal.egg$Pomacea_Species),
    upp = 0,
    low = 0)


##try with first row of data

train <- ppal.egg %>% 
  filter(obs == 1)
model <- ppal.egg %>% 
  filter(obs != 1)

pal.mefit30 <- glmer.nb(formula = count ~ zpaldens + zavetemp + zavedepth + I(zavedepth^2) +                          
                          zavetemp*zavedepth + zavetemp* I(zavedepth^2)+ (1|Cell), data = model)  ###convergence issues needs fixed
ss <- getME(pal.mefit30,c("theta","fixef"))                                                          ###start with previous theta
pal.mefit30.2 <- update(pal.mefit30,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###still has issues
pal.mefit30.3<- update(pal.mefit30,start=ss,control=glmerControl(optimizer="bobyqa",                 ###different optimzer works
                                                                 optCtrl=list(maxfun=2e5))) 
ppal.egg$predict.count[51] <- exp(predict(pal.mefit30.3, train, type = "link",
                                                     allow.new.levels = TRUE))

b <- bootMer(pal.mefit30.3, nsim = 100, function(x) predict(x, newdata = train))
ppal.egg$upp[1] = exp(quantile(b$t, probs = 0.975))
ppal.egg$low[1] = exp(quantile(b$t, probs = 0.025))

ppal.egg[,23:27]  ###it worked

####now create loop
for (i in 1:length(ppal.egg$predict.count)) {
  train <- ppal.egg %>% 
    filter(obs == i)
  model <- ppal.egg %>% 
    filter(obs != i)
  
  pal.mefit30 <- glmer.nb(formula = count ~ zpaldens + zavetemp + zavedepth + I(zavedepth^2) +                          
                            zavetemp*zavedepth + zavetemp* I(zavedepth^2)+ (1|Cell), data = model)  ###convergence issues needs fixed
  ss <- getME(pal.mefit30,c("theta","fixef"))                                                          ###start with previous theta
  pal.mefit30.2 <- update(pal.mefit30,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))         ###still has issues
  pal.mefit30.3<- update(pal.mefit30,start=ss,control=glmerControl(optimizer="bobyqa",                 ###different optimzer works
                                                                   optCtrl=list(maxfun=2e5))) 
  ppal.egg$predict.count[i] <- exp(predict(pal.mefit30.3, train, type = "link",
                                           allow.new.levels = TRUE))
  
  b <- bootMer(pal.mefit30.3, nsim = 100, function(x) predict(x, newdata = train))
  ppal.egg$upp[i] = exp(quantile(b$t, probs = 0.975))
  ppal.egg$low[i] = exp(quantile(b$t, probs = 0.025))
}

#create loop for p. maculata

pmac.egg <- pmac.egg %>% 
  mutate(predict.count = 0,
         upp = 0,
         low = 0,
         obs = 1:length(pmac.egg$Pomacea_Species))

for (i in 1:length(pmac.egg$predict.count)) {
  train <- pmac.egg %>% 
    filter(obs == i)
  model <- pmac.egg %>% 
    filter(obs != i)
  mac.mefit35 <-  glmer.nb(formula = count ~zmacdens + zphotoperiod + zavedepth + I(zavedepth^2)+ (1|Cell), data = pmac.egg) 
  pmac.egg$predict.count[i] <- exp(predict(mac.mefit35, train, type = "link",
                                           allow.new.levels = TRUE))
  
  b <- bootMer(mac.mefit35, nsim = 100, function(x) predict(x, newdata = train))
  pmac.egg$upp[i] = exp(quantile(b$t, probs = 0.975))
  pmac.egg$low[i] = exp(quantile(b$t, probs = 0.025))
}


###ppal.egg plot with 95% prediction intervals####
ppal.egg %>%
  ggplot(aes(x = dayofyear, y = count))+
  facet_rep_grid(Cell~as.character(year))+
  theme_classic()+
  geom_pointrange(aes(x =dayofyear, y = predict.count, ymin = low, ymax = upp,
                      color = "predict.count", shape = "predict.count"),
                  show.legend = F)+
  geom_point(aes(color = "count", shape = "count"),
             show.legend = T)+
  geom_line(aes(x = dayofyear, y = count,
                color = "count"), show.legend = F)+
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b"))+
  coord_cartesian(ylim = c(0,105))+
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.title = element_text(size = 12, hjust = 0.5),
        strip.background = element_rect(fill = "white", linetype = 0),
        strip.text = element_text(size = 12, face = "bold", color = "black"))+
  labs(x = NULL, y = "Total Egg Clutches in Transects")+
  scale_y_continuous(breaks = c(0,20,40,60,80,100))+
  scale_color_manual(name = NULL,
                     values = c( "predict.count" = "#666666", "count" = "black"),
                     labels = c("Observed", "Predicted"))+
  scale_shape_manual(name = NULL,
                     values = c("predict.count" = 17,"count" = 16),
                     labels = c("Observed", "Predicted"))

ggsave("C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/AppleSnail_LifeHistory_manuscript/Figures&Tables/Figure3_Paludosa_avp.pdf",
       plot = last_plot(), device = "pdf", units = "in", width = 7.5, height = 6)

####predicted versus actuals p. maculata

pmac.egg %>%
  ggplot(aes(x = dayofyear, y = count))+
  facet_rep_grid(Cell~as.character(year))+
  theme_classic()+
  geom_pointrange(aes(x =dayofyear, y = predict.count, ymin = low, ymax = upp,
                      color = "predict.count", shape = "predict.count"), show.legend = F)+
  geom_point(aes(y = count, color = "count", shape = "count"),size = 2, show.legend = T)+
  geom_line(aes(x = dayofyear, y = count, color = "count"),show.legend = F)+
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b"))+
  labs(x = NULL, y = "Total Egg Masses in Transects" ,
       color = "legend")+
  theme(axis.text.x = element_text(vjust = 0.5),
        axis.title = element_text(size = 12, hjust = 0.5),
        strip.background = element_rect(fill = "white", linetype = 0),
        strip.text = element_text(size = 12, face = "bold", color = "black"))+
  scale_y_continuous(breaks = c(0,30,60,90,120,150))+
  scale_color_manual(name = NULL,
                     values = c( "predict.count" = "#666666", "count" = "black"),
                     labels = c("Observed", "Predicted"))+
  scale_shape_manual(name = NULL,
                     values = c("predict.count" = 17,"count" = 16),
                     labels = c("Observed", "Predicted"))+
  coord_cartesian(ylim = c(0,150))



ggsave("C:/Users/Nathan Barrus/Documents/FAU/Masters Thesis/AppleSnail_LifeHistory_manuscript/Figures&Tables/Figure4_Maculata_avp.pdf",
       plot = last_plot(), device = "pdf", units = "in", width = 10, height = 8)


####model diagnostics####
library(DHARMa)


ppal.egg <- ppal.egg %>% 
  mutate(predict.count = exp(predict(pal.mefit30)),
         RQR.residual = qres.binom(pal.mefit30))
qresiduals(pal.mefit30)


pal.mefit30.3 <- glmer.nb(formula = count ~ zpaldens + zavetemp + zavedepth + I(zavedepth^2) +                                 #best model
                          zavetemp*zavedepth + zavetemp* I(zavedepth^2)+ (1|Cell), data = ppal.egg)

####Get Randomized Quantile Residuals####
testDispersion(pal.mefit30.3)
testOutliers(pal.mefit30.3)
simulationOutput <- simulateResiduals(fittedModel = pal.mefit30.3, plot = F)
simulationOutput
residuals(simulationOutput)

ppal.egg <- ppal.egg %>% 
  mutate(RQR = residuals(simulationOutput))

plot(simulationOutput)

testDispersion(mac.mefit35)
testOutliers(mac.mefit35)
simulationOutput <- simulateResiduals(fittedModel = mac.mefit35, plot = F)
simulationOutput
residuals(simulationOutput)

pmac.egg <- pmac.egg %>% 
  mutate(RQR = residuals(simulationOutput))


####Predicting other data ####
library(lubridate)

transectarea <- transectarea %>% 
  group_by(Cell) %>% 
  summarise(t.area = sum(Area))

avedepth = 44.80337
sddepth = 22.26986
avemac = 0.17410714
sdmac = 0.08470387
avepal = 0.04913919
sdpal = 0.03200296
avePhoto = 13.80392
sdPhoto = 0.8733435
aveTemp =25.41632
sdTemp =2.587796

n.egg.data <- tibble(Date = as_date(rep("2020-08-13", times = 8)),
                     Cell = rep(c("M1","M2","M3","M4"),times = 2),
                     Location = rep("CentralRidge", times = 8),
                     Depth = rep(c(42.9887,19.9926,39.0144,10.6680), times = 2),
                     Photo = rep(13.276, times = 8),
                     Temp = rep(28.9915, times = 8),
                     Area = rep(18000, times = 8),
                     Pomacea_Species = c(rep("Maculata", times = 4),
                                 rep("Paludosa", times = 4)),
                     Avepomdens = c(rep(avemac, times = 4),
                                    rep(avepal, times = 4)),
                     SDpomdens = c(rep(sdmac,times = 4),
                                   rep(sdpal,times = 4)),
                     count = c(18,17,5,4,0,0,10,0))

n.egg.data <- n.egg.data %>% 
  mutate(density = count/Area,
         year = year(Date),
         Pomacea_Species = if_else(Pomacea_Species == "Paludosa" & Cell == "M4",
                                   true = "NA", false = Pomacea_Species)) %>% 
  filter(Pomacea_Species != "NA") %>% 
 left_join(summ.TTdata, by = c("Cell","year", "Pomacea_Species")) %>% 
drop_na() %>% 
left_join(transectarea, by = "Cell") %>% 
 mutate(zavedepth = (Depth - avedepth)/sddepth,
         zavetemp = (Temp - aveTemp)/sdTemp,
         zphotoperiod = (Photo - avePhoto)/sdPhoto,
         zpomdens = (pom.dens - Avepomdens)/SDpomdens)

n.egg.data <- n.egg.data %>% 
  dplyr::select(Pomacea_Species,Location, Date, Cell,count,density)

#density on transect
trandensity<- summ.LILA.egg %>% 
  filter(year == 2020 & month == "August") %>% 
  dplyr::select(Pomacea_Species, Date, Cell, count, density) %>% 
  mutate(Location = "transects")



n.egg.data <- n.egg.data %>% 
  bind_rows(trandensity) 

n.egg.data <- n.egg.data%>% 
  group_by(Pomacea_Species, Cell) %>% 
  mutate(total.count = sum(count),
         total.dens = sum(density),
         perc.count = count/total.count*100,
         perc.dens = density/total.dens*100)



otheregg <- LILA.egg.counts %>% 
  filter(Transect == "T3"|Transect == "T4") %>% 
  ungroup() %>% 
  group_by(Date, Cell,Transect, Pomacea_Species, year, month, dayofyear, Area) %>% 
  summarise(count = sum(Count)) %>% 
  mutate(area = Area,
         density = count/area,
         Date = as_date(Date)) %>% 
  dplyr::select(-Area)

transects2 <- summ.LILA.egg %>% 
  filter(Date %in% as_date(otheregg$Date)) %>% 
  mutate(Transect = "transect") %>% 
  dplyr::select(-week.date,-sd.depth.cm,-ave.temp.c, -depth.change,-Season)

oedepth <- transects2 %>% 
  ungroup() %>% 
  dplyr::select(ave.depth.cm,Date,Cell)

otheregg <- otheregg %>% 
  bind_rows(transects2)

otheregg <- otheregg %>% 
  ungroup() %>% 
  group_by(Date, Cell, Pomacea_Species,year,month,dayofyear) %>% 
  mutate(tot.count = sum(count),
         tot.dens = sum(density),
         per.count = count/tot.count*100,
         per.dens = density/tot.dens*100) %>% 
 dplyr::select(Date,Cell, Pomacea_Species,Transect,per.dens,density) 

otheregg <- otheregg %>% 
  left_join(oedepth, by = c("Date","Cell"))

otheregg %>% 
  ggplot(aes(x = Transect, y = log(density+.00001)))+
  geom_boxplot()+
  theme_classic()

otheregg_summ <- otheregg %>% 
  filter(Transect == "transect") %>% 
  ungroup() %>% 
  mutate(majority  = if_else(per.dens < 50, true = 0, false = 1)) %>% 
  summarise(n.tot = n(),
            n.maj = sum(majority)) %>% 
  mutate(perc.maj = n.maj/n.tot*100)

ppal.n.egg <- n.egg.data %>% 
  ungroup() %>% 
  filter(Pomacea_Species == "Paludosa") %>% 
  rename(zpaldens = zpomdens) 

ppal.n.egg <- ppal.n.egg%>% 
  mutate(pred.count = exp(predict(pal.mefit30.3, newdata = ppal.n.egg)),
         pred.dens = pred.count/t.area)

pmac.n.egg <- n.egg.data %>% 
  ungroup() %>% 
  filter(Pomacea_Species == "Maculata") %>% 
  rename(zmacdens = zpomdens) 

pmac.n.egg <- pmac.n.egg%>% 
  mutate(pred.count = exp(predict(mac.mefit35, newdata = pmac.n.egg)),
         pred.dens = pred.count/t.area)

ppal.n.egg %>% 
  ggplot((aes(y = Density, x = "P. paludosa Observed")))+
  geom_boxplot()+
  geom_boxplot(aes(y = pred.dens, x = "P. paculata Predicted"))+
  geom_boxplot(data = pmac.n.egg, (aes(y = Density, x = "P. maculata Observed")))+
  geom_boxplot(data = pmac.n.egg, (aes(y = pred.dens, x = "P. maculata Predicted")))

library(tidyverse)
library(lubridate)

pmac.egg %>% 
  mutate(tempinter  = if_else(ave.temp.c > mean(ave.temp.c, na.rm = T),
                              true = "warm", false = "cool")) %>% 
   ggplot(aes(x = ave.depth.cm, y = log(sd.depth.cm), color = tempinter))+
  theme_classic()+
  geom_point()+
  geom_smooth(method = "lm")

summary(lm(log(pmac.egg$sd.depth.cm)~pmac.egg$ave.depth.cm * pmac.egg$ave.temp.c ))
plot(lm(log(pmac.egg$sd.depth.cm)~pmac.egg$ave.depth.cm * pmac.egg$ave.temp.c ))


