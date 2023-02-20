### This code summarized throw trap data from LILA during the wet-season-stage contrast experiment ###
### that began in the wet season of 2018. ###

### data management help https://urldefense.com/v3/__https://bouchat.github.io/IntroDataMgmt20Jan.html*introduction_to_r__;Iw!!FjuHKAHQs5udqho!KplAcfmVaxfmgZi7JQBeSpv1KwYTV3WwZN41DByxf9OmOuEe-o-6GPBYEgzFpLxjqU-BRDeib2BRKEZDpTIJ$  
## data management help: Base R cheat sheet.

library(tidyverse)
library(readxl)
library(nlme)
library(car)
# tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats

### Section one:
# Load in raw crayfish  data from throw traps over multiple macrocosms at LILA and multiple seasons

Cray_SU_2018 <- read_csv("M:/LILA/LILA Data Entry/2018/LILA_TT_2018_CRAY_SUMMER.csv")
Cray_SP_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Throw Trapping/Spring/LILA_TT_2019_CRAY_SPRING.csv")
Cray_SU_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Throw Trapping/Summer/LILA_TT_2019_CRAY_SUMMER.csv")
Cray_SP_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Throw Trapping/Spring/LILA_TT_2020_CRAY_SPRING.csv")
Cray_SU_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Throw Trapping/Summer/LILA_TT_2020_CRAY_SUMMER.csv")
Cray_SP_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Throw Trapping/Spring/LILA_TT_2021_CRAY_SPRING.csv")
Cray_SU_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Throw Trapping/Summer/LILA_TT_2021_CRAY_SUMMER.csv")
Cray_SP_2022 <- read_excel("M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_CRAY_SPRING.xlsx")
Cray_SU_2022 <- read_excel("M:/LILA/LILA Data Entry/2022/Throw Trapping/Summer/LILA_TT_2022_CRAY_SUMMER.xlsx")
# raw crayfish data should have 15 variables: Session, Year, Month, Day, Wetland, Throw, Species,
#     Length, Sex, Form, Comments, Sorted By, Entered By, Checked By


#Merge all Cray data sets together into one dataframe called catch

catch <- rbind(Cray_SU_2018, Cray_SP_2019, Cray_SU_2019, Cray_SP_2020,
               Cray_SU_2020, Cray_SP_2021, Cray_SU_2021, Cray_SP_2022,
               Cray_SU_2022)

# Remove unnecessary data (Sorted By, Checked By, Enetered By) from the catch data frame
catch <- dplyr::select(catch,one_of(c('Session','Year','Month','Day','Wetland','Throw','Species','Length',
                             'Sex','Form','Comments')))

#Check to see that all data was imported and combined properly
summary(catch)
catch$Species <- as.factor(catch$Species)
catch$Sex <- as.factor(catch$Sex)
catch$Form <- as.factor(catch$Form)
catch$Comments <- as.factor(catch$Comments)
catch$Throw <- as.factor (catch$Throw)
levels(catch$Throw)

# Create Hydro-pattern Column (Hydro) where each Wetland is under 1 of 2 hydropattern treatments
#  Hydropattern treatment levels: Constrained ('C') and Unconstrained ('U')
catch$Hydro <- catch$Wetland
catch$Hydro <- as.factor(gsub("M1", "U",
                            gsub("M2", "C", 
                            gsub("M3", "U", 
                            gsub("M4", "C", catch$Hydro)))))

#Add in cumulative season column (Cumulative) Where each sampling season is given a numeric value.
catch$Cumulative <- catch$Session
catch$Cumulative <- as.numeric(gsub("Summer 2018", 1, 
                               gsub("Spring 2019", 2,
                               gsub("Summer 2019", 3,
                               gsub("Spring 2020", 4,
                               gsub("Summer 2020", 5, 
                               gsub("Spring 2021", 6, 
                               gsub("Summer 2021", 7, 
                               gsub("Spring 2022", 8,
                               gsub("Summer 2022", 9,
                               catch$Cumulative))))))))))

#Add in Season column indicating which season (Wet or Dry) in which sampling occurred
catch$Season <- catch$Session
catch$Season <- as.factor(sub("Summer 2018", "wet", 
                               gsub("Spring 2019", "dry",
                               gsub("Summer 2019", "wet",
                               gsub("Spring 2020", "dry",
                               gsub("Summer 2020", "wet", 
                               gsub("Spring 2021", "dry", 
                               gsub("Summer 2021", "wet", 
                               gsub("Spring 2022", "dry",
                               gsub("Summer 2022", "wet",
                               catch$Season))))))))))
# Add in Water year column (Wateryr) indicating the wateryr in which sampling occurred
# Water year is named in the fashion of SFWMD, with the water year corresponding to the
# calendar year of the dry season. The lab typically names water year off the calendar year of the
# wet season.
catch$Wateryr <- catch$Session
catch$Wateryr <- as.numeric(gsub("Summer 2018", 2019, 
                            gsub("Spring 2019", 2019,
                            gsub("Summer 2019", 2020,
                            gsub("Spring 2020", 2020,
                            gsub("Summer 2020", 2021, 
                            gsub("Spring 2021", 2021, 
                            gsub("Summer 2021", 2022, 
                            gsub("Spring 2022", 2022,
                            gsub("Summer 2022", 2023,
                            catch$Wateryr))))))))))

# Add in a Location column specifying the location (DS, SS, or CR) of a specific throw trap.
# DS = Deep Slough, SS = Shallow Slough, CR = Central Ridge
catch$Location <- catch$Throw
catch$Location <- as.character(gsub(1, "DS",
                               gsub(2, "DS",
                               gsub(3, "DS",
                               gsub(4, "DS",
                               gsub(5, "DS",
                               gsub(6, "DS",
                               gsub(7, "DS",
                               gsub(8, "DS",
                               gsub(9, "DS",
                               gsub(10, "DS",
                               gsub(11, "SS",
                               gsub(12, "SS",
                               gsub(13, "SS",
                               gsub(14, "SS",
                               gsub(15, "CR",
                               gsub(16, "CR",
                               gsub(17, "CR",
                               gsub(18, "CR",
                               gsub(19, "CR",
                               gsub(20, "CR",
                               gsub(21, "CR",
                               gsub(22, "CR",
                               catch$Location)))))))))))))))))))))))

# Change "." value in Length to 0
catch <- within(catch, Length [Length == "." & Species == "NOCRAY"] <- 0)

# Check data frame to make sure variables are factors, and ordered where the factors are ordered. 
summary(catch)
is.character(catch$Wetland)
is.character(catch$Location)
is.character(catch$Season)
is.numeric(catch$Wateryr)
is.numeric(catch$Cumulative)
is.character(catch$Species)
is.numeric(catch$Length)

# Length is not registering as a numeric so it must be converted to a numeric
catch$Length <- as.numeric(catch$Length)

# Crayfish Species caught in Throw Traps will be listed as PROFAL or PROSPP.
# ASSume all PROSPP = PROFAL as a PROALL has not been seen at LILA.
# PROFAL = Procambarus fallax, PROALL = Procambarus alleni, PROSPP = Procambarus species
# Reassess the above assumption when a PROALL is observed...

# Section One: Crayfish Counts
# crayfish caught in LILA TT = PROFAL and PROSPP
# First create counts of crayfish, using f(x) dplyr::if_else()
# if-else syntax: if_else(condition, true, falso, missing = NULL)
# In the case of calculating PROFAL catch (see below): 
# condition = catch$species == 'PROFAL', if true mark with 1, if false mark with 0.:
catch$PROFAL <- dplyr::if_else(catch$Species == 'PROFAL', 1, 0)
catch$PROSPP <- dplyr::if_else(catch$Species == 'PROSPP', 1, 0)
catch$NOCRAY <- dplyr::if_else(catch$Species == 'NOCRAY', 1, 0)


# Section Two: Crayfish Biomass and presence 
# Create Biomass column by calculating biomass using regressions
# Regression data for most species came from Loftus and Trexler
# Regressions for wet mass were multiplied by 0.2 to obtain cry mass
# Dry mass regression have no 0.2 multiplier
# Biomass is in mg dry mass

catch$PROFALbiom <- dplyr::if_else(catch$Species == 'PROFAL', (10^(-4.6578+3.2274*log10(catch$Length)))*1000, 0)
catch$PROSPPbiom <- dplyr::if_else(catch$Species == 'PROSPP', (10^(-4.6578+3.2274*log10(catch$Length)))*1000, 0)
catch$NOCRAYbiom <- dplyr::if_else(catch$Species == 'NOCRAY', 0, 0)

# Create a crayfish biomass column for purposes of summary data
catch$Craybiom <- catch$PROFALbiom + catch$PROSPPbiom

#create a crayfish presence column (not by species) for purposes of a count in the summary data
catch$CRAYPR<- catch$PROFAL + catch$PROSPP


# Section Three: This code manipulates variables and creates new dataframes that summarize species
# level biomass and counts. 
## Summaries of counts and biomass were made by:
### Cumulative, Wateryr, Season, Wetland, Hydro, Location:
### This summarizes biomass and counts by TT, Location (aka habitat), Wetland, Season.

summary(catch)

## Part A:
## Build the summary file
## Summarize mass total, by species, and counts at the trap scale using a pipe operator %>%
## calculate the  catch per trap for each throw trap by:
## Cumulative,  Wateryr, Season, Wetland, Hydro, Location)

trap_catch <- catch %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Throw, Location) %>% 
  summarize(Craymass = sum(Craybiom), Craycount = sum(CRAYPR), 
            PROFALmass = sum(PROFALbiom), PROFAL = sum(PROFAL),
            PROSPPmass = sum(PROSPPbiom), PROSPP = sum(PROSPP))

## Part B:
## Calculate the total catch per habitat (by Cumulative, Wateryr, Season, Wetland, Hydro)
## DS catch = sum of 10 TT (1-10), SS catch = sum pf 4 TT (11-14), and CR cath = sum of 8 TT (15-22)

sum_hab_catch <- trap_catch %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Location) %>% 
  summarize(Craymass = sum(Craymass), Craycount = sum(Craycount), 
            PROFALmass = sum(PROFALmass), PROFAL = sum(PROFAL),
            PROSPPmass = sum(PROSPPmass), PROSPP = sum(PROSPP))

## Part C: 
## Calculate the mean catch per habitat (by Cumulative, Wateryr, Season, Wetland, Hydro)
## DS catch = mean of 10 TT, SS catch = mean pf 4 TT, and CR ca th = mean of 8 throw traps

mean_hab_catch <- trap_catch %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Location) %>%
  summarize(Craymass = mean(Craymass), Craycount = mean(Craycount), 
            PROFALmass = mean(PROFALmass), PROFAL = mean(PROFAL),
            PROSPPmass = mean(PROSPPmass), PROSPP = mean(PROSPP))

## Part D:
## Calculate the total catch per wetland by:
## Cumulative, Wateryr, Season, Hydro

sum_macro_catch <- trap_catch %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(Craymass = sum(Craymass), Craycount = sum(Craycount), 
            PROFALmass = sum(PROFALmass), PROFAL = sum(PROFAL),
            PROSPPmass = sum(PROSPPmass), PROSPP = sum(PROSPP))

## Part E:
## Calculate the mean catch per wetland by:
## Cumulative, Wateryr, Season, Hydro

mean_macro_catch <- trap_catch %>%
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(Craymass = mean(Craymass), Craycount = mean(Craycount), 
            PROFALmass = mean(PROFALmass), PROFAL = mean(PROFAL),
            PROSPPmass = mean(PROSPPmass), PROSPP = mean(PROSPP))

## Part F:
## Calculate the mean catch of wetlands for catch only from sloughs
trap_catch_sloughs <- trap_catch[trap_catch$Location %in%  c("DS", "SS"),]
mean_slough_catch <- trap_catch_sloughs %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(Craymass = mean(Craymass), Craycount = mean(Craycount), 
            PROFALmass = mean(PROFALmass), PROFAL = mean(PROFAL),
            PROSPPmass = mean(PROSPPmass), PROSPP = mean(PROSPP))


## Section 4: Data analysis of summary data
## rmANOVA of hydropattern experiment
## Assumptions of rmANOVA: Normality, Sphericity

# Step 1:
# Create an initial models to obtain Auto Correlation Function (ACF) Values

count_model_wateryr_i <- lme(Craycount ~ Hydro + Wateryr + Hydro*Wateryr,
                                random = ~1|Wetland,
                                data = mean_slough_catch)

count_model_season_i <- lme(Craycount ~ Hydro + Season + Hydro*Season,
                            random = ~1|Wetland,
                            data = mean_slough_catch)

count_model_time_i <- lme(Craycount ~ Hydro + Cumulative + Hydro*Cumulative,
                            random = ~1|Wetland,
                            data = mean_slough_catch)

# Step 2:
# Run ACF function to obtain ACF values

# Run ACF for cumulative count model
ACF(count_model_wateryr_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_A_1 <- -0.12355690
ACF_A_2 <-  0.05452069
ACF_A_3 <- -0.11509032
ACF_A_4 <- -0.51699207
ACF_A_5 <- -0.50512979
ACF_A_6 <-  0.39384326
ACF_A_7 <- -0.29409028
ACF_A_8 <-  NA

# Run ACF for season count model
ACF(count_model_season_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_B_1 <-  0.27773704
ACF_B_2 <-  0.12923651
ACF_B_3 <- -0.06215935
ACF_B_4 <- -0.65377744
ACF_B_5 <- -0.43865916
ACF_B_6 <- -0.50783497
ACF_B_7 <- -0.32648562
ACF_B_8 <-  0.34439818

# Run ACF for season count model
ACF(count_model_time_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_C_1 <- -0.1596160
ACF_C_2 <-  0.1181472
ACF_C_3 <- -0.1719422
ACF_C_4 <- -0.4354346
ACF_C_5 <- -0.6086400
ACF_C_6 <-  0.4560312
ACF_C_7 <- -0.4359506
ACF_C_8 <-  NA

# Step 3:
# Run rmANOVAs to determine if hydro-pattern treatment has had an effect on crayfish

# Part 1: Model.a
# model.a.1 examines cumulative and Hydro effect on crayfish counts
model.a <- lme(Craycount ~ factor(Hydro)*ordered(Wateryr), 
                random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_A_1),
                data = mean_slough_catch, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 169.6311 and Complex AIC = 168.3009

summary(model.a)
anova(model.a)
#                                numDF denDF   F-value p-value
# (Intercept)                        1    24 161.61749  <.0001
# factor(Hydro)                      1     2  15.90099  0.0575
# ordered(Wateryr)                   4    24   7.36613  0.0005
# factor(Hydro):ordered(Wateryr)     4    24   5.72438  0.0022

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.a <- plot(resid(model.a))


#Check assumption of homogeneity of variance:
mean_slough_catch$resa <- residuals(model.a) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresa <- abs(mean_slough_catch$resa) #creates new column with absolute value of residuals
mean_slough_catch$resa2 <- mean_slough_catch$absresa^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.a <- lm(resa2 ~ Wetland, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.a) # P = 0.2642


# Test for normal distribution of residuals
shapiro.test(residuals(model.a)) # P = 0.2986
qqnorm(model.a$residuals)
qqline(model.a$residuals)
anova(model.a)


# Part 2: model.b
# model.b.1 examines season and Hydro effect on crayfish counts
model.b <- lme(Craycount ~ factor(Hydro)*ordered(Season), 
                 random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_B_1),
                 data = mean_slough_catch, method = "REML")

summary(model.b)
anova(model.b)
#                               numDF denDF  F-value p-value
# (Intercept)                       1    30 32.24387  <.0001
# factor(Hydro)                     1     2  4.42482  0.1701
# ordered(Season)                   1    30 13.01961  0.0011
# factor(Hydro):ordered(Season)     1    30  0.03653  0.8497


# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.b <- plot(resid(model.b))


#Check assumption of homogeneity of variance:
mean_slough_catch$resb <- residuals(model.b) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresb <- abs(mean_slough_catch$resb) #creates new column with absolute value of residuals
mean_slough_catch$resb2 <- mean_slough_catch$absresb^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.b <- lm(resb2 ~ Wetland, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.b) # P = 0.007 


# Test for normal distribution of residuals
shapiro.test(residuals(model.b)) # P = 0.6361
qqnorm(model.b$residuals)
qqline(model.b$residuals)
anova(model.b)


# Part 1: Model.c
# model.c.1 examines cumulative and Hydro effect on crayfish counts
model.c <- lme(Craycount ~ factor(Hydro)*ordered(Cumulative), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_C_1),
               data = mean_slough_catch, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 169.6311 and Complex AIC = 168.3009

summary(model.c)
anova(model.c)
#                                   numDF denDF   F-value p-value
# (Intercept)                           1    16 153.63517  <.0001
# factor(Hydro)                         1     2  17.62870  0.0523
# ordered(Cumulative)                   8    16   8.20356  0.0002
# factor(Hydro):ordered(Cumulative)     8    16   3.94740  0.0094

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.c <- plot(resid(model.c))


#Check assumption of homogeneity of variance:
mean_slough_catch$resc <- residuals(model.c) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresc <- abs(mean_slough_catch$resc) #creates new column with absolute value of residuals
mean_slough_catch$resc2 <- mean_slough_catch$absresc^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.c <- lm(resc2 ~ Wetland, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.c) # P = 0.04368 


# Test for normal distribution of residuals
shapiro.test(residuals(model.c)) # P = 0.6171
qqnorm(model.c$residuals)
qqline(model.c$residuals)
anova(model.c)

### model c needs to be rerun with craycount transformed (sqrt transformed since count data)
mean_slough_catch$craycount_trans <- sqrt(mean_slough_catch$Craycount)


model.c_t <- lme(craycount_trans ~ factor(Hydro)*ordered(Cumulative), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_C_1),
               data = mean_slough_catch, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 169.6311 and Complex AIC = 168.3009

summary(model.c_t)
anova(model.c_t)
#                                   numDF denDF   F-value p-value
# (Intercept)                           1    16 403.9828  <.0001
# factor(Hydro)                         1     2  12.1092  0.0736
# ordered(Cumulative)                   8    16   9.5155  0.0001
# factor(Hydro):ordered(Cumulative)     8    16   3.4864  0.0160

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.c_t <- plot(resid(model.c_t))


#Check assumption of homogeneity of variance:
mean_slough_catch$resc_t <- residuals(model.c_t) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresc_t <- abs(mean_slough_catch$resc_t) #creates new column with absolute value of residuals
mean_slough_catch$resc_t2 <- mean_slough_catch$absresc_t^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.c_t <- lm(resc_t2 ~ Wetland, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.c_t) # P = 0.3476 


# Test for normal distribution of residuals
shapiro.test(residuals(model.c_t)) # P = 0.7095
qqnorm(model.c_t$residuals)
qqline(model.c_t$residuals)
anova(model.c_t)


### Section 5: Exporting Data Frames as .csv files
### This is when you adjust the below code to export your final data frames to more easily support figure construction


### Export mean_slough_catch data for use in Sommer et al. paper.

write.csv(mean_slough_catch, "M:\\LILA\\Stage Contrast Study\\Annual Reports\\2022 Annual Report\\Data\\CMD_MEAN_SLOUGH_CATCH_18-22.csv", row.names = FALSE)

