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
Cray_SP_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Spring/LILA_TT_2019_CRAY_SPRING.csv")
Cray_SU_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Summer/LILA_TT_2019_CRAY_SUMMER.csv")
Cray_SP_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Spring/LILA_TT_2020_CRAY_SPRING.csv")
Cray_SU_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Summer/LILA_TT_2020_CRAY_SUMMER.csv")
Cray_SP_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Spring/LILA_TT_2021_CRAY_SPRING.csv")
Cray_SU_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Summer/LILA_TT_2021_CRAY_SUMMER.csv")
Cray_SP_2022 <- read_excel("M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_CRAY_SPRING.xlsx")

# raw crayfish data should have 15 variables: Session, Year, Month, Day, Wetland, Throw, Species,
#     Length, Sex, Form, Comments, Sorted By, Entered By, Checked By


#Merge all Cray data sets together into one dataframe called catch

catch <- rbind(Cray_SU_2018, Cray_SP_2019, Cray_SU_2019, Cray_SP_2020,
               Cray_SU_2020, Cray_SP_2021, Cray_SU_2021, Cray_SP_2022)

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
                               catch$Cumulative)))))))))

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
                               catch$Season)))))))))
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
                            catch$Wateryr)))))))))

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

# Step 2:
# Run ACF function to obtain ACF values

# Run ACF for cumulative count model
ACF(count_model_wateryr_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_A_1 <-  0.02073329
ACF_A_2 <- -0.17330691
ACF_A_3 <- -0.30680072
ACF_A_4 <- -0.43875217
ACF_A_5 <- -0.25728127
ACF_A_6 <-  0.78952814
ACF_A_7 <-  0.09134290

# Run ACF for season count model
ACF(count_model_season_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_B_1 <-  0.1703690
ACF_B_2 <- -0.2503504
ACF_B_3 <- -0.3016725
ACF_B_4 <- -0.5866229
ACF_B_5 <- -0.1610230
ACF_B_6 <-  0.5172624
ACF_B_7 <-  0.3889395


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
#                                  numDF denDF   F-value p-value
#(Intercept)                           1    22 187.58689  <.0001
#factor(Hydro)                         1     2   7.27842  0.1143
#ordered(Cumulative)                   3    22   9.75042  0.0003
#factor(Hydro):ordered(Cumulative)     3    22   1.58702  0.2210

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
anova(Levene.model.a) # P = 0.84


# Test for normal distribution of residuals
shapiro.test(residuals(model.a)) # P = 0.3436
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
#                              numDF denDF  F-value p-value
#(Intercept)                       1    26 60.31690  <.0001
#factor(Hydro)                     1     2  2.79494  0.2365
#ordered(Season)                   1    26  3.44731  0.0747
#factor(Hydro):ordered(Season)     1    26  0.77127  0.3879


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
shapiro.test(residuals(model.b)) # P = 0.9558
qqnorm(model.b$residuals)
qqline(model.b$residuals)
anova(model.b)