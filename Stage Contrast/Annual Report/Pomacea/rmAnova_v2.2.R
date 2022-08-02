
library(tidyverse)
library(readxl)
library(lubridate)
## read in .csv data file with mean crayfish, fish, and stem densities ##

data <- read_excel("Testscore_data.xlsx") 

## Notes on data: Densities quantified from summer sampling session throw traps from sloughs ##

# Treatment: deep = A, shallow = B (factor)
# Wetland: Macrocosms M1, M2, M3, M4 (random)
# Session: 1 = 2018, 2 = 2019, 3 = 2020 (ordered) 

library(nlme)
library(car)
# models all examining how density is affected by hydro-pattern treatment (Treatment), time (Session),
# and an interaction between hydropattern and time (Treatment*Session)
# Create an initial model to obtain Auto Correlation Function (ACF) value #

model.a <- lme(ave.egg.ha ~ hydropattern + year + hydropattern*year,
               random = ~1|Cell,
               data = EMdata)


ACF <- ACF(model.a)

ACF[2,2]

## conduct rmANOVA on EggMass Density (Cray_Data$Density) data ##

model.b1 <- lme(ave.egg.ha ~ factor(hydropattern)*ordered(year),
                random = ~1|Cell, correlation = corAR1(form = ~year|Cell, value = ACF[2,2]),
                data = EMdata, method = "REML")

model.b2 <- lme(ave.egg.ha ~ factor(hydropattern)*ordered(year),
                random = ~1|Cell, correlation = corAR1(form = ~year|Cell, value = ACF[3,2]),
                data = EMdata, method = "REML")

model.b3 <- lme(ave.egg.ha ~ factor(hydropattern)*ordered(year),
               random = ~1|Cell, correlation = corAR1(form = ~year|Cell, value = ACF[2,2], ACF[3,2]),
               data = EMdata, method = "REML")

summary(model.b1)
summary(model.b2)
summary(model.b3)
###AIC values are no different between model b1 and b2, 
### model b3 not low enough to make up for added complexity, using model.b1 for ANOVA
anova(model.b1)
anova(model.b3)

# Checking for violations of assumptions #
# Test for normal distribution of residuals #
shapiro.test(residuals(model.b1))

# Plot the quantiles with an expected normal distribution qqline #
qqnorm(model.b1$residuals)
qqline(model.b1$residuals)
anova(model.b1)




