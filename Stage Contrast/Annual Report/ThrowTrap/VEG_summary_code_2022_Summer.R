### This code summarized throw trap data from LILA during the wet-season-stage contrast experiment ###
### that began in the wet season of 2018. ###

rm(list = ls())

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

Veg_SU_2018 <- read_csv("M:/LILA/LILA Data Entry/2018/LILA_TT_2018_Veg_SUMMER.csv")
Veg_SP_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Throw Trapping/Spring/LILA_TT_2019_Veg_SPRING.csv")
Veg_SU_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Throw Trapping/Summer/LILA_TT_2019_Veg_SUMMER.csv")
Veg_SP_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Throw Trapping/Spring/LILA_TT_2020_Veg_SPRING.csv")
Veg_SU_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Throw Trapping/Summer/LILA_TT_2020_Veg_SUMMER.csv")
Veg_SP_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Throw Trapping/Spring/LILA_TT_2021_Veg_SPRING.csv")
Veg_SU_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Throw Trapping/Summer/LILA_TT_2021_Veg_SUMMER.csv")
Veg_SP_2022 <- read_excel("M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_Veg_SPRING.xlsx")
Veg_SU_2022 <- read_excel("M:/LILA/LILA Data Entry/2022/Throw Trapping/Summer/LILA_TT_2022_Veg_SUMMER.xlsx")

# raw crayfish data should have 15 variables: Session, Year, Month, Day, Wetland, Throw, Species,
#     Length, Sex, Form, Comments, Sorted By, Entered By, Checked By


#Merge all Veg data sets together into one dataframe called stems

stems <- rbind(Veg_SU_2018, Veg_SP_2019, Veg_SU_2019, Veg_SP_2020, Veg_SU_2020, 
               Veg_SP_2021, Veg_SU_2021, Veg_SP_2022, Veg_SU_2022)

summary(stems)

# Remove unnecessary data (Sorted By, Checked By, Enetered By) from the catch data frame
stems <- dplyr::select(stems,one_of(c('Session','Year','Month','Day','Wetland','Throw','Species','Density',
                                      'Comments')))

# Create Hydro-pattern Column (Hydro) where each Wetland is under 1 of 2 hydropattern treatments
#  Hydropattern treatment levels: Constrained ('C') and Unconstrained ('U')
stems$Hydro <- stems$Wetland
stems$Hydro <- as.factor(gsub("M1", "U",
                        gsub("M2", "C", 
                        gsub("M3", "U", 
                        gsub("M4", "C", stems$Hydro)))))

#Add in cumulative season column (Cumulative) Where each sampling season is given a numeric value.
stems$Cumulative <- stems$Session
stems$Cumulative <- as.numeric(gsub("Summer 2018", 1, 
                              gsub("Spring 2019", 2,
                              gsub("Summer 2019", 3,
                              gsub("Spring 2020", 4,
                              gsub("Summer 2020", 5, 
                              gsub("Spring 2021", 6, 
                              gsub("Summer 2021", 7, 
                              gsub("Spring 2022", 8, 
                                  gsub("Summer 2022", 9, 
                              stems$Cumulative))))))))))

#Add in Season column indicating which season (Wet or Dry) in which sampling occurred
stems$Season <- stems$Session
stems$Season <- as.factor(sub("Summer 2018", "wet", 
                              gsub("Spring 2019", "dry",
                              gsub("Summer 2019", "wet",
                              gsub("Spring 2020", "dry",
                              gsub("Summer 2020", "wet", 
                              gsub("Spring 2021", "dry", 
                              gsub("Summer 2021", "wet", 
                              gsub("Spring 2022", "dry", 
                                   gsub("Summer 2022", "wet",
                              stems$Season))))))))))
# Add in Water year column (Wateryr) indicating the wateryr in which sampling occurred
# Water year is named in the fashion of SFWMD, with the water year corresponding to the
# calendar year of the dry season. The lab typically names water year off the calendar year of the
# wet season.
stems$Wateryr <- stems$Session
stems$Wateryr <- as.numeric(gsub("Summer 2018", 2019, 
                            gsub("Spring 2019", 2019,
                            gsub("Summer 2019", 2020,
                            gsub("Spring 2020", 2020,
                            gsub("Summer 2020", 2021, 
                            gsub("Spring 2021", 2021, 
                            gsub("Summer 2021", 2022, 
                            gsub("Spring 2022", 2022, 
                                 gsub("Summer 2022", 2023,
                                 stems$Wateryr))))))))))

# Add in a Location column specifying the location (DS, SS, or CR) of a specific throw trap.
# DS = Deep Slough, SS = Shallow Slough, CR = Central Ridge
stems$Location <- stems$Throw
stems$Location <- as.character(gsub(1, "DS",
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
                                   stems$Location)))))))))))))))))))))))

# Section One: Stem Counts
# First create presence record of Macrophyte stems, using f(x) dplyr::if_else()
# if-else syntax: if_else(condition, true, falso, missing = NULL)
# In the case of calculating PROFAL catch (see below): 
# condition = catch$species == 'ELECEL', if true mark with 1, if false mark with 0.

stems$CLAJAM <- dplyr::if_else(stems$Species == 'CLAJAM', 1, 0)
stems$CRIAME <- dplyr::if_else(stems$Species == 'CRIAME', 1, 0)
stems$ELECEL <- dplyr::if_else(stems$Species == 'ELECEL', 1, 0)
stems$ELEELO <- dplyr::if_else(stems$Species == 'ELEELO', 1, 0)
stems$ELEINT <- dplyr::if_else(stems$Species == 'ELEINT', 1, 0)
stems$ELESPP <- dplyr::if_else(stems$Species == 'ELESPP', 1, 0)
stems$GRASS  <- dplyr::if_else(stems$Species == 'GRASS' , 1, 0)
stems$JUSANG <- dplyr::if_else(stems$Species == 'JUSANG', 1, 0)
stems$LEEHEX <- dplyr::if_else(stems$Species == 'LEEHEX', 1, 0)
stems$NUPADV <- dplyr::if_else(stems$Species == 'NUPADV', 1, 0)
stems$NYMODO <- dplyr::if_else(stems$Species == 'NYMODO', 1, 0)
stems$PANHEM <- dplyr::if_else(stems$Species == 'PANHEM', 1, 0)
stems$PASGEM <- dplyr::if_else(stems$Species == 'PASGEM', 1, 0)
stems$PONCOR <- dplyr::if_else(stems$Species == 'PONCOR', 1, 0)
stems$RHYSPP <- dplyr::if_else(stems$Species == 'RHYSPP', 1, 0)
stems$SAGLAN <- dplyr::if_else(stems$Species == 'SAGLAN', 1, 0)
stems$TYPSPP <- dplyr::if_else(stems$Species == 'TYPSPP', 1, 0)
stems$UNKDIC <- dplyr::if_else(stems$Species == 'UNKDIC', 1, 0)
stems$URELOB <- dplyr::if_else(stems$Species == 'URELOB', 1, 0)

# Section Two: Stem Density and presence 
# Create Density column for each species e.g. CLAJAMden...
stems$CLAJAMden <- dplyr::if_else(stems$Species == "CLAJAM", stems$Density, 0)
stems$CRIAMEden <- dplyr::if_else(stems$Species == 'CRIAME', stems$Density, 0)
stems$ELECELden <- dplyr::if_else(stems$Species == "ELECEL", stems$Density, 0)
stems$ELEELOden <- dplyr::if_else(stems$Species == "ELEELO", stems$Density, 0)
stems$ELEINTden <- dplyr::if_else(stems$Species == "ELEINT", stems$Density, 0)
stems$ELESPPden <- dplyr::if_else(stems$Species == "ELESPP", stems$Density, 0)
stems$GRASSden  <- dplyr::if_else(stems$Species ==  "GRASS", stems$Density, 0)
stems$JUSANGden <- dplyr::if_else(stems$Species == 'JUSANG', stems$Density, 0)
stems$LEEHEXden <- dplyr::if_else(stems$Species == 'LEEHEX', stems$Density, 0)
stems$NUPADVden <- dplyr::if_else(stems$Species == "NUPADV", stems$Density, 0)
stems$NYMODOden <- dplyr::if_else(stems$Species == "NYMODO", stems$Density, 0)
stems$PANHEMden <- dplyr::if_else(stems$Species == "PANHEM", stems$Density, 0)
stems$PASGEMden <- dplyr::if_else(stems$Species == "PASGEM", stems$Density, 0)
stems$PONCORden <- dplyr::if_else(stems$Species == "PONCOR", stems$Density, 0)
stems$RHYSPPden <- dplyr::if_else(stems$Species == "RHYSPP", stems$Density, 0)
stems$SAGLANden <- dplyr::if_else(stems$Species == "SAGLAN", stems$Density, 0)
stems$TYPSPPden <- dplyr::if_else(stems$Species == "TYPSPP", stems$Density, 0)
stems$UNKDICden <- dplyr::if_else(stems$Species == "UNKDIC", stems$Density, 0)
stems$URELOBden <- dplyr::if_else(stems$Species == "URELOB", stems$Density, 0)


stems$STEMDEN <- stems$CLAJAMden + stems$CRIAMEden + stems$ELECELden + stems$ELEELOden + stems$ELEINTden +
                 stems$ELESPPden + stems$GRASSden  + stems$JUSANGden + stems$LEEHEXden + stems$NUPADVden +
                 stems$NYMODOden + stems$PANHEMden + stems$PASGEMden + stems$PONCORden + stems$RHYSPPden +
                 stems$SAGLANden + stems$TYPSPPden + stems$UNKDICden + stems$URELOBden


# Section Three: This code manipulates variables and creates new dataframes that summarize species level density
### summaries of counts and biomass were made by:
### Cumulative, Wateryr, Season, Wetland, Hydro, and Location
### This summarizes stem density by TT, Location (aka habitat), wetland, season.

summary(stems)

# Check to see if Wateryr is a numeric and Season is a factor
is.numeric(stems$Wateryr)
is.factor(stems$Season)
# both came back as true no fixing needed.

## Part A: Build the summary file
## Build the summary file
## Summarize stem total, by species, and counts at the trap scale using a pipe operator %>%
## calculate the  catch per trap for each throw trap by:
## Cumulative,  Wateryr, Season, Wetland, Hydro, Location)
trap_stems<- stems %>%
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Throw, Location) %>% 
  summarize(STEMS = sum(STEMDEN), CLAJAM = sum(CLAJAMden), CRIAME = sum(CRIAMEden), ELECEL = sum(ELECELden),
            ELEELO = sum(ELEELOden), ELEINT = sum(ELEINTden), ELESPP = sum(ELESPPden), GRASS = sum(GRASSden, PANHEMden, PASGEMden),
            JUSANG = sum(JUSANGden), LEEHEX = sum(LEEHEXden), NUPADV = sum(NUPADVden), NYMODO = sum(NYMODOden), 
            PONCOR = sum(PONCORden), RHYSPP = sum(RHYSPPden), SAGLAN = sum(SAGLANden), TYPSPP = sum(TYPSPPden), 
            UNKDIC = sum(UNKDICden), URELOB = sum(URELOBden))

## Part B:
## Calculate the total stems per wetland by:
## Cumulative, Wateryr, Season, Wetland, Hydro

sum_macro_stems <- trap_stems %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(STEMS = sum(STEMS), CLAJAM = sum(CLAJAM), CRIAME = sum(CRIAME), ELECEL = sum(ELECEL), ELEELO = sum(ELEELO),
            ELEINT = sum(ELEINT), ELESPP = sum(ELESPP), GRASS = sum(GRASS), JUSANG = sum(JUSANG), LEEHEX = sum(LEEHEX),
            NUPADV = sum(NUPADV), NYMODO = sum(NYMODO), PONCOR = sum(PONCOR), RHYSPP = sum(RHYSPP), SAGLAN = sum(SAGLAN), 
            TYPSPP = sum(TYPSPP), UNKDIC = sum(UNKDIC), URELOB = sum(URELOB))
## Part C:
## Calculate the mean stems per wetland by:
## Cumulative, Wateryr, Season, Wetland, Hydro

mean_macro_stems <- trap_stems %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>%
  summarize(STEMS = mean(STEMS), CLAJAM = mean(CLAJAM), CRIAME = mean(CRIAME), ELECEL = mean(ELECEL), ELEELO = mean(ELEELO),
            ELEINT = mean(ELEINT), ELESPP = mean(ELESPP), GRASS = mean(GRASS), JUSANG = mean(JUSANG), LEEHEX = mean(LEEHEX),
            NUPADV = mean(NUPADV), NYMODO = mean(NYMODO), PONCOR = mean(PONCOR), RHYSPP = mean(RHYSPP), SAGLAN = mean(SAGLAN), 
            TYPSPP = mean(TYPSPP), UNKDIC = mean(UNKDIC), URELOB = mean(URELOB))

## Part D:
## Calculate the total stems per habitat by Cumulative, Wateryr, Season, Wetland, Hydro)
## DS catch = sum of 10 TT (1-10), SS catch - sum of 4 TT (11-14), and CR catch = sum of 8 TT (15-22)

sum_hab_stems <- trap_stems %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Location) %>% 
  summarize(STEMS = sum(STEMS), CLAJAM = sum(CLAJAM), CRIAME = sum(CRIAME), ELECEL = sum(ELECEL), ELEELO = sum(ELEELO),
            ELEINT = sum(ELEINT), ELESPP = sum(ELESPP), GRASS = sum(GRASS), JUSANG = sum(JUSANG), LEEHEX = sum(LEEHEX),
            NUPADV = sum(NUPADV), NYMODO = sum(NYMODO), PONCOR = sum(PONCOR), RHYSPP = sum(RHYSPP), SAGLAN = sum(SAGLAN), 
            TYPSPP = sum(TYPSPP), UNKDIC = sum(UNKDIC), URELOB = sum(URELOB))

## Part E:
## Calculate the mean stems per habitat (by Cumulative, Wateryr, Season, Wetland, Hydro)
## DS catch = mean of 10 TT, SS catch = mean pf 4 TT, and CR catch = mean of 8 throw traps

mean_hab_stems <- trap_stems %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Location) %>%
  summarize(STEMS = mean(STEMS), CLAJAM = mean(CLAJAM), CRIAME = mean(CRIAME), ELECEL = mean(ELECEL), ELEELO = mean(ELEELO),
            ELEINT = mean(ELEINT), ELESPP = mean(ELESPP), GRASS = mean(GRASS), JUSANG - mean(JUSANG), LEEHEX = mean(LEEHEX),
            NUPADV = mean(NUPADV), NYMODO = mean(NYMODO), PONCOR = mean(PONCOR), RHYSPP = mean(RHYSPP), SAGLAN = mean(SAGLAN), 
            TYPSPP = mean(TYPSPP), UNKDIC = mean(UNKDIC), URELOB = mean(URELOB))

## Part F:
## Calculate the mean catch of wetlands for catch only from sloughs
## Cumulative, Wateryr, Season, Hydro
trap_stems_sloughs <- trap_stems[trap_stems$Location %in%  c("DS", "SS"),]

mean_slough_stems <- trap_stems_sloughs %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(STEMS = mean(STEMS), CLAJAM = mean(CLAJAM), CRIAME = mean(CRIAME), ELECEL = mean(ELECEL), ELEELO = mean(ELEELO),
            ELEINT = mean(ELEINT), ELESPP = mean(ELESPP), GRASS = mean(GRASS), JUSANG = mean(JUSANG), LEEHEX = mean(LEEHEX),
            NUPADV = mean(NUPADV), NYMODO = mean(NYMODO), PONCOR = mean(PONCOR), RHYSPP = mean(RHYSPP), SAGLAN = mean(SAGLAN), 
            TYPSPP = mean(TYPSPP), UNKDIC = mean(UNKDIC), URELOB = mean(URELOB))

## Section 4: Data analysis of summary data
## rmANOVA of hydropattern experiment
## Assumptions of rmANOVA: Normality, Sphericity

# Step 1:
# Create an initial models to obtain Auto Correlation Function (ACF) Values

stemden_model_wateryr_i <- lme(STEMS ~ Hydro + Wateryr + Hydro*Wateryr,
                             random = ~1|Wetland,
                             data = mean_slough_stems)

stemden_model_season_i <- lme(STEMS ~ Hydro + Season + Hydro*Season,
                            random = ~1|Wetland,
                            data = mean_slough_stems)

stemden_model_time_i <- lme(STEMS ~ Hydro + Cumulative + Hydro*Cumulative,
                              random = ~1|Wetland,
                              data = mean_slough_stems)

# Step 2:
# Run ACF function to obtain ACF values

# Run ACF for cumulative count model
ACF(stemden_model_wateryr_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_A_1 <- -0.23748043
ACF_A_2 <- -0.09522966
ACF_A_3 <-  0.02908670
ACF_A_4 <- -0.11932869
ACF_A_5 <- -0.15903078
ACF_A_6 <-  0.12075760
ACF_A_7 <-  0.23044723
ACF_A_8 <-  0.06944147

# Run ACF for season count model
ACF(stemden_model_season_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_B_1 <-  0.24982178
ACF_B_2 <-  0.01199467
ACF_B_3 <-  0.19072665
ACF_B_4 <- -0.41354050
ACF_B_5 <- -0.44445552
ACF_B_6 <- -0.42271807
ACF_B_7 <- -0.25294346
ACF_B_8 <-  0.05228629

# Run ACF for season count model
ACF(stemden_model_time_i)
ACF_C_1 <- -0.27703766
ACF_C_2 <- -0.06335845
ACF_C_3 <-  0.01094750
ACF_C_4 <- -0.06685046
ACF_C_5 <- -0.19386927
ACF_C_6 <-  0.23134553
ACF_C_7 <-  0.13034143
ACF_C_8 <-  0.09479453
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1


# Step 3:
# Run rmANOVAs to determine if hydro-pattern treatment has had an effect on crayfish

# Part 1: Model.a
# model.a examines cumulative and Hydro effect on stem density
model.a <- lme(STEMS ~ factor(Hydro)*ordered(Wateryr), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_A_1, ACF_A_2),
               data = mean_slough_stems, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 376.976 and Complex AIC = 375.039 (1,2 had lowest AIC value)

summary(model.a)
anova(model.a)
#                                numDF denDF   F-value p-value
# (Intercept)                        1    24 139.96512  <.0001
# factor(Hydro)                      1     2  15.67607  0.0583
# ordered(Wateryr)                   4    24   8.45175  0.0002
# factor(Hydro):ordered(Wateryr)     4    24   1.71702  0.1791

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.a <- plot(resid(model.a))


#Check assumption of homogeneity of variance:
mean_slough_stems$resa <- residuals(model.a) #extracts residual and places them in a new column in dataframe
mean_slough_stems$absresa <- abs(mean_slough_stems$resa) #creates new column with absolute value of residuals
mean_slough_stems$resa2 <- mean_slough_stems$absresa^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.a <- lm(resa2 ~ Wetland, data = mean_slough_stems) #anova of the squared residuals
anova(Levene.model.a) # P = 0.761


# Test for normal distribution of residuals
shapiro.test(residuals(model.a)) # P = 0.8019
qqnorm(model.a$residuals)
qqline(model.a$residuals)
anova(model.a)


# Part 2: model.b
# model.b.1 examines season and Hydro effect on crayfish counts

# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 447.7018 and Complex AIC = 445.705 (1,2 had lowest AIC value)
model.b <- lme(STEMS ~ factor(Hydro)*ordered(Season), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_B_1, ACF_B_2),
               data = mean_slough_stems, method = "REML")

summary(model.b)
anova(model.b)

#                               numDF denDF   F-value p-value
# (Intercept)                       1    30 101.28790  <.0001
# factor(Hydro)                     1     2  13.13217  0.0684
# ordered(Season)                   1    30   1.17330  0.2874
# factor(Hydro):ordered(Season)     1    30   5.60010  0.0246


# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.b <- plot(resid(model.b))


#Check assumption of homogeneity of variance:
mean_slough_stems$resb <- residuals(model.b) #extracts residual and places them in a new column in dataframe
mean_slough_stems$absresb <- abs(mean_slough_stems$resb) #creates new column with absolute value of residuals
mean_slough_stems$resb2 <- mean_slough_stems$absresb^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.b <- lm(resb2 ~ Wetland, data = mean_slough_stems) #anova of the squared residuals
anova(Levene.model.b) # P = 0.5661

# Test for normal distribution of residuals
shapiro.test(residuals(model.b)) # P = 0.4536
qqnorm(model.b$residuals)
qqline(model.b$residuals)
anova(model.b)

#### Part 3: Model.c

# model.c.1 examines cumulative and Hydro effect on stem counts
model.c <- lme(STEMS ~ factor(Hydro)*ordered(Cumulative), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_C_1),
               data = mean_slough_stems, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 169.6311 and Complex AIC = 168.3009

summary(model.c)
anova(model.c)
#                                   numDF denDF   F-value p-value
# (Intercept)                           1    16 137.14286  <.0001
# factor(Hydro)                         1     2  15.74357  0.0580
# ordered(Cumulative)                   8    16   5.43161  0.0020
# factor(Hydro):ordered(Cumulative)     8    16   1.75767  0.1604

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.c <- plot(resid(model.c))


#Check assumption of homogeneity of variance:
mean_slough_stems$resc <- residuals(model.c) #extracts residual and places them in a new column in dataframe
mean_slough_stems$absresc <- abs(mean_slough_stems$resc) #creates new column with absolute value of residuals
mean_slough_stems$resc2 <- mean_slough_stems$absresc^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.c <- lm(resc2 ~ Wetland, data = mean_slough_stems) #anova of the squared residuals
anova(Levene.model.c) # P = 0.04368 


# Test for normal distribution of residuals
shapiro.test(residuals(model.c)) # P = 0.6171
qqnorm(model.c$residuals)
qqline(model.c$residuals)
anova(model.c)



### Section 5: Exporting Data Frames as .csv files
### This is when you adjust the below code to export your final data frames to more easily support figure construction


### Export mean_slough_catch data for use in Sommer et al. paper.

write.csv(mean_slough_stems, "M:\\LILA\\LILA QAQC_data\\2022\\4_Data_ANALYSIS\\STEMS_MEAN_SLOUGH_DENSITY.csv", row.names = FALSE)


