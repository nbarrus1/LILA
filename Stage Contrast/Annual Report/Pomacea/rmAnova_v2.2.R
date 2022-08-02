
#load in libraries for the rm ANOVA

library(nlme)
library(car)

#create the egg mass densities between the unconstrained and constrained categories
#this code requires the data from 001_eggmass_plots.r  If you're working with directories and
#run the code from 01_eggmass_plots.r the data will continue to this scriptbecause it remains in 
#the r environment.  In short, open both script in your directory, run the code from 01_eggmass_plots.r 
#then move to this script and the code should continue to work

eggdensity <- eggdata.master %>% 
  select(-Notes) %>% 
  mutate(hydropattern = if_else(Cell == "M1"|Cell == "M3", 
                                true = "unconstrained", false = "constrained")) %>% 
  group_by(Year,Month,Day,hydropattern,Cell) %>% 
  summarise(tot.eggs = sum(Count)) %>% 
  left_join(transectarea, by = "Cell") %>% 
  mutate(egg.ha = tot.eggs/tot.area.ha)%>% 
  ungroup() %>% 
  group_by(Year,hydropattern, Cell) %>% 
  summarise(n = n(),
            ave.egg.ha = mean(egg.ha, na.rm = T),
            sd.egg.ha = sd(egg.ha, na.rm = T)) %>%
  mutate(se = sd.egg.ha/sqrt(n))

# models all examining how density is affected by hydro-pattern treatment (Treatment), time (Session),
# and an interaction between hydropattern and time (Treatment*Session)
# Create an initial model to obtain Auto Correlation Function (ACF) value #

model.a <- lme(ave.egg.ha ~ hydropattern + Year + hydropattern:Year,
               random = ~1|Cell,
               data = eggdensity)


ACF <- ACF(model.a)

ACF[2,2]

## conduct rmANOVA on EggMass Density (Cray_Data$Density) data ##

model.b1 <- lme(ave.egg.ha ~ factor(hydropattern)*ordered(Year),
                random = ~1|Cell, correlation = corAR1(form = ~Year|Cell, value = ACF[2,2]),
                data = eggdensity, method = "REML")

model.b2 <- lme(ave.egg.ha ~ factor(hydropattern)*ordered(Year),
                random = ~1|Cell, correlation = corAR1(form = ~Year|Cell, value = ACF[3,2]),
                data = eggdensity, method = "REML")

model.b3 <- lme(ave.egg.ha ~ factor(hydropattern)*ordered(Year),
               random = ~1|Cell, correlation = corAR1(form = ~Year|Cell, value = ACF[2,2], ACF[3,2]),
               data = eggdensity, method = "REML")

summary(model.b1)
summary(model.b2)
summary(model.b3)
###AIC values are no different between model b1 and b2, 
### model b3  AIC not low enough to make up for added complexity, using model.b1 for ANOVA
anova(model.b1)
anova(model.b3)

# Checking for violations of assumptions #
# Test for normal distribution of residuals #
shapiro.test(residuals(model.b1))

# Plot the quantiles with an expected normal distribution qqline #
qqnorm(model.b1$residuals)
qqline(model.b1$residuals)
anova(model.b1)




