#This code summarized CPUE data from fyke nets and hoop nets set in LILA during the wet-season-stage contrast 
    # experiment that started in 2018. The code creates standardized catch rates of fish > 5 cm SL 
    # (mean of hoop +mean of fyke) each night and then averages the two or three nights.  
    # It also creates biomass CPUE for all fish using regressions.

# data management help https://bouchat.github.io/IntroDataMgmt20Jan.html#introduction_to_r 
# data management help: Base R cheat sheet.

install.packages('tidyverse')

library(tidyverse)
# tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats
library(readxl)
#readxel allows for reading of .xls and .xlsx spreadsheets into R
library(car)
library(nlme)

### Section one:
# Load in raw fish data from CPUE NETTING from multiple macrocosms at LILA over multiple seasons

CPUE_2019_SU <- read_excel("M:/LILA/LILA Data Entry/2019/CPUE Netting/LILA_CPUE_2019_SUMMER.xlsx")
CPUE_2020_SP <- read_excel("M:/LILA/LILA Data Entry/2020/CPUE Netting/LILA_CPUE_2020_SPRING.xlsx")
CPUE_2020_SU <- read_excel("M:/LILA/LILA Data Entry/2020/CPUE Netting/LILA_CPUE_2020_SUMMER.xlsx")
CPUE_2021_SP <- read_excel("M:/LILA/LILA Data Entry/2021/CPUE Netting/LILA_CPUE_2021_SPRING.xlsx")
CPUE_2021_SU <- read_excel("M:/LILA/LILA Data Entry/2021/CPUE Netting/LILA_CPUE_2021_SUMMER.xlsx")
CPUE_2022_SP <- read_excel("M:/LILA/LILA Data Entry/2022/CPUE Netting/LILA_CPUE_2022_SPRING.xlsx", na = ".")
CPUE_2022_SU <- read_excel("M:/LILA/LILA Data Entry/2022/CPUE Netting/LILA_CPUE_2022_SUMMER.xlsx", na = ".")

# In order to merge the files together into one data frame the "session", "Entered By", "Checked By"
# Columns need to be dropped from the data 2022 data set.
CPUE_2022_SP <- dplyr::select(CPUE_2022_SP,one_of(c('Date','Wateryr','Night','Macrocosm','Net Type',
                                                    'Net Location','Species Code','Stand Length',
                                                    'Good Set','Notes')))
CPUE_2022_SU <- dplyr::select(CPUE_2022_SU,one_of(c('Date','Wateryr','Night','Macrocosm','Net Type',
                                                    'Net Location','Species Code','Stand Length',
                                                    'Good Set','Notes')))

#Merge all Cray data sets together into one dataframe called catch 
# make sure all seasons of data that you eant to use in your analysis are included below
#currently Summer 2022 data is not included
catch <- rbind(CPUE_2019_SU, CPUE_2020_SP, CPUE_2020_SU, CPUE_2021_SP,
               CPUE_2021_SU, CPUE_2022_SP)
View(catch)
#Change column name "good set" to "Setqual" = set quality and make Setqual a factor
names(catch)[9] <-paste("Setqual")
catch$Setqual <- as.factor(catch$Setqual)
# Create Hydro-pattern Column (Hydro) where each Wetland is under 1 of 2 hydropattern treatments
#  Hydropattern treatment levels: Constrained ('C') and Unconstrained ('U')
catch$Hydropattern <- catch$Macrocosm
catch$Hydropattern <- as.factor(gsub("M1", "U",
                              gsub("M2", "C", 
                                   gsub("M3", "U", 
                                        gsub("M4", "C", catch$Hydropattern)))))

#Add in Season column indicating which season (Wet or Dry) in which sampling occurred
catch$Season <- catch$Date
catch$Season <- as.factor(sub('2019-05-23', "wet", 
                          gsub('2019-05-24', "wet",
                          gsub('2019-06-06', "wet",
                          gsub('2019-06-07', "wet",
                              gsub('2020-02-25', "dry",
                              gsub('2020-02-26', "dry", 
                              gsub('2020-02-27', "dry", 
                              gsub('2020-02-11', "dry",
                              gsub('2020-02-12', "dry",
                              gsub('2020-02-13', "dry",
                                  gsub('2020-06-02', "wet",
                                  gsub('2020-06-03', "wet",
                                  gsub('2020-06-09', "wet",
                                  gsub('2020-06-10', "wet",
                                      gsub('2021-04-01', "dry",
                                      gsub('2021-03-30', "dry",
                                      gsub('2021-03-31', "dry",
                                      gsub('2021-04-13', "dry",
                                      gsub('2021-04-14', "dry",
                                      gsub('2021-04-15', "dry",
                                          gsub('2021-06-30', "wet",
                                          gsub('2021-07-01', "wet",
                                          gsub('2021-07-02', "wet",
                                          gsub('2021-07-14', "wet",
                                          gsub('2021-07-15', "wet",
                                          gsub('2021-07-16', "wet",
                                              gsub('2022-03-29', "dry",
                                              gsub('2022-03-30', "dry",
                                              gsub('2022-03-31', "dry",
                                              gsub('2022-04-12', "dry",
                                              gsub('2022-04-13', "dry",
                                              gsub('2022-04-14', "dry",
                                                  gsub('2022-06-15', "wet",
                                                  gsub('2022-06-16', "wet",
                                                  gsub('2022-06-17', "wet",
                                                  gsub('2022-06-21', "wet",
                                                  gsub('2022-06-22', "wet",
                                                  gsub('2022-06-23', "wet",
                                                       catch$Season)))))))))))))))))))))))))))))))))))))))

#Add in a Cumlative column indicating which order in which sampling occured for ease of data organization
catch$Cumulative <- catch$Date
catch$Cumulative <- as.factor(sub('2019-05-23', "1", 
                         gsub('2019-05-24', "1",
                         gsub('2019-06-06', "1",
                         gsub('2019-06-07', "1",
                              gsub('2020-02-25', "2",
                              gsub('2020-02-26', "2", 
                              gsub('2020-02-27', "2", 
                              gsub('2020-02-11', "2",
                              gsub('2020-02-12', "2",
                              gsub('2020-02-13', "2",
                                    gsub('2020-06-02', "3",
                                    gsub('2020-06-03', "3",
                                    gsub('2020-06-09', "3",
                                    gsub('2020-06-10', "3",
                                        gsub('2021-04-01', "4",
                                        gsub('2021-03-30', "4",
                                        gsub('2021-03-31', "4",
                                        gsub('2021-04-13', "4",
                                        gsub('2021-04-14', "4",
                                        gsub('2021-04-15', "4",
                                            gsub('2021-06-30', "5",
                                            gsub('2021-07-01', "5",
                                            gsub('2021-07-02', "5",
                                            gsub('2021-07-14', "5",
                                            gsub('2021-07-15', "5",
                                            gsub('2021-07-16', "5",
                                                gsub('2022-03-29', "6",
                                                gsub('2022-03-30', "6",
                                                gsub('2022-03-31', "6",
                                                gsub('2022-04-12', "6",
                                                gsub('2022-04-13', "6",
                                                gsub('2022-04-14', "6",
                                                    gsub('2022-06-15', "7",
                                                    gsub('2022-06-16', "7",
                                                    gsub('2022-06-17', "7",
                                                    gsub('2022-06-21', "7",
                                                    gsub('2022-06-22', "7",
                                                    gsub('2022-06-23', "7",
                                                         catch$Cumulative)))))))))))))))))))))))))))))))))))))))

#run summary and then check to see if variablest that shoudl be factors or numeric are, if not correct
summary(catch)
catch$Macrocosm <- as.factor(catch$Macrocosm)
catch$`Stand Length` <- as.numeric(catch$`Stand Length`)
catch$Setqual <- as.factor(catch$Setqual)
is.numeric(catch$Wateryr) # make sure wateryr is a numeric


#First create counts for fish and herps
catch$ALLMIS <- if_else(catch$`Species Code` == 'ALLMIS', 1, 0)
catch$AMENAT <- if_else(catch$`Species Code` == 'AMENAT', 1, 0)
catch$AMENEB <- if_else(catch$`Species Code` == 'AMENEB', 1, 0)
catch$AMICAL <- if_else(catch$`Species Code` == 'AMICAL', 1, 0)
catch$AMPMEA <- if_else(catch$`Species Code` == 'AMPMEA', 1, 0)
catch$CICURO <- if_else(catch$`Species Code` == 'CICURO', 1, 0)
catch$ENNGLO <- if_else(catch$`Species Code` == 'ENNGLO', 1, 0)
catch$ERISUC <- if_else(catch$`Species Code` == 'ERISUC', 1, 0)
catch$FROG   <- if_else(catch$`Species Code` == 'FROG',   1, 0)
catch$LEPGUL <- if_else(catch$`Species Code` == 'LEPGUL', 1, 0)
catch$LEPMAC <- if_else(catch$`Species Code` == 'LEPMAC', 1, 0)
catch$LEPMAR <- if_else(catch$`Species Code` == 'LEPMAR', 1, 0)
catch$LEPMIC <- if_else(catch$`Species Code` == 'LEPMIC', 1, 0)
catch$LEPPLA <- if_else(catch$`Species Code` == 'LEPPLA', 1, 0)
catch$LEPPUN <- if_else(catch$`Species Code` == 'LEPPUN', 1, 0)
catch$LEPSPP <- if_else(catch$`Species Code` == 'LEPSPP', 1, 0)
catch$MICSAL <- if_else(catch$`Species Code` == 'MICSAL', 1, 0)
catch$NERSPP <- if_else(catch$`Species Code` == 'NERSPP', 1, 0)
catch$NOCATCH <- if_else(catch$`Species Code` == 'NOCATCH', 1, 0)
catch$NOFISH <- if_else(catch$`Species Code` == 'NOFISH', 1, 0)
catch$POMMAC <- if_else(catch$`Species Code` == 'POMMAC', 1, 0)
catch$POMPAL <- if_else(catch$`Species Code` == 'POMPAL', 1, 0)
catch$PROFAL <- if_else(catch$`Species Code` == 'PROFAL', 1, 0)
catch$RANATP <- if_else(catch$`Species Code` == 'RANATP', 1, 0)
catch$SIRLAC <- if_else(catch$`Species Code` == 'SIRLAC', 1, 0)
catch$TESTUD <- if_else(catch$`Species Code` == 'TESTUD', 1, 0)

#creating biomass for fish
# all Length weight regressions convert length in mm -> mg or g
# all length measurements in CPUE data were taken in cm
# need to convert from cm -> mm

#regression data for most species came from Loftus Kushlan and Trexler regressions (regressions for wet mass *0.2)
# Loftus Kushlan and Trexler regressions are mm -> g 
# Dorn Lab regressions are mm -> mg
#biomass is in g dry mass


catch$AMENATbiom <- if_else(catch$`Species Code` == 'AMENAT', 0.2*(10^(-4.736+3.046*log10(10*catch$`Stand Length`))), 0)
catch$AMENEBbiom <- if_else(catch$`Species Code` == 'AMENEB', 0.2*(10^(-4.736+3.046*log10(10*catch$`Stand Length`))), 0)
catch$AMICALbiom <- if_else(catch$`Species Code` == 'AMICAL', 0.2*(10^(-4.971+2.989*log10(10*catch$`Stand Length`))), 0)
catch$CICURObiom <- if_else(catch$`Species Code` == 'CICURO', (10^(-2.55676+3.30058*log10(10*catch$`Stand Length`)))/1000, 0)
catch$ENNGLObiom <- if_else(catch$`Species Code` == 'ENNGLO', 0.2*(10^(-4.624+3.113*log10(10*catch$`Stand Length`))), 0)
catch$ERISUCbiom <- if_else(catch$`Species Code` == 'ERISUC', 0.2*(10^(-5.236+3.305*log10(10*catch$`Stand Length`))), 0)
catch$LEPGULbiom <- if_else(catch$`Species Code` == 'LEPGUL', (10^(-2.9269+3.4467*log10(10*catch$`Stand Length`)))/1000, 0)
catch$LEPMACbiom <- if_else(catch$`Species Code` == 'LEPMAC', 0.2*(10^(-5.100+3.325*log10(10*catch$`Stand Length`))), 0)
catch$LEPMARbiom <- if_else(catch$`Species Code` == 'LEPMAR', (10^(-2.5949+3.3278*log10(10*catch$`Stand Length`)))/1000, 0)
catch$LEPMICbiom <- if_else(catch$`Species Code` == 'LEPMIC', 0.2*(10^(-4.876+3.198*log10(10*catch$`Stand Length`))), 0)
catch$LEPPLAbiom <- if_else(catch$`Species Code` == 'LEPPLA', 0.2*(10^(-6.344+3.408*log10(10*(catch$`Stand Length`*1.25)))), 0)
catch$LEPPUNbiom <- if_else(catch$`Species Code` == 'LEPPUN', 0.2*(10^(-4.807+3.222*log10(10*catch$`Stand Length`))), 0)
catch$LEPSPPbiom <- if_else(catch$`Species Code` == 'LEPSPP', (10^(-1.7881+2.8052*log10(10*catch$`Stand Length`)))/1000, 0)
catch$MICSALbiom <- if_else(catch$`Species Code` == 'MICSAL', 0.2*(10^(-5.203+3.244*log10(10*catch$`Stand Length`))), 0)

## create a predatory fish biomass column  (not by species) for purposes of summary
catch$Predfishbiom <- catch$AMENATbiom + catch$AMENEBbiom + catch$AMICALbiom  + catch$CICURObiom + 
  catch$ENNGLObiom + catch$LEPGULbiom + catch$LEPMACbiom + catch$LEPMARbiom + catch$LEPMICbiom + 
  catch$LEPPLAbiom + catch$LEPPUNbiom + catch$LEPSPPbiom + catch$MICSALbiom

#create a fish presence column (not by species) for purposes of a count in the summary
catch$Predfishpres <- catch$AMENAT + catch$AMENEB  + catch$AMICAL + catch$CICURO + catch$ENNGLO + 
  catch$LEPGUL + catch$LEPMAC + catch$LEPMAR + catch$LEPMIC + catch$LEPPLA + catch$LEPPUN +
  catch$LEPSPP + catch$MICSAL

#Create non fish Crayfish predator presence column (not by species) for purposes of count in the summary
catch$Craypred <- catch$TESTUD + catch$SIRLAC + catch$AMPMEA

#Section 2: Build summary data frames of CPUE data of Large Predatory Fish
#this code manipulates variables and creates a new file that summarizes species level biomass and counts
# first Night, and Loc were all switched to factors and night and cum were ordered
# summaries of counts and biomass were made 
# by Wateryr, Season, Macrocosm, Hydropattern, Net Type, Net Location, summarizes biomass and counts by trap, night, season
# Net Location = physical location in the wetland (1, 2, 3, 4, 5)
# Wateryr is indicated with the wateryr starting in the wet season, Lab annotated wateryr. 
# Hydropattern = 'C' or 'U'; constrained (low) or unconstrained (high summer depths)
# Season = 'wet' or 'dry' season (June or February)
# Macrocosm = M1, M2, M3, M4
# Night = '1', '2', '3'
# Net Type = 'H' (black hoop) or 'F' (green fyke)

# remove the nets with bad sets or alligators entering traps
catchfilt <- filter(catch, Setqual == 'Y')

#build the summary file
# summarize mass total, by species, and counts at the scale of nets using a pipe operator %>%

catchsum <- catchfilt %>% 
  group_by (Cumulative, Wateryr, Season, Macrocosm, Hydropattern, Night, `Net Type`, `Net Location`, Date) %>% 
  summarize (Predfishmass = sum(Predfishbiom), Predfishcount = sum(Predfishpres), AMENATmass = sum(AMENATbiom),
             AMENEBmass = sum(AMENEBbiom), AMICALmass = sum(AMICALbiom), CICUROmass = sum(CICURObiom), 
             ENNGLOmass = sum(ENNGLObiom), ERiSUCmass = sum(ERISUCbiom), LEPGULmass = sum(LEPGULbiom),
             LEPMACmass = sum(LEPMACbiom), LEPMARmass = sum(LEPMARbiom), LEPMICmass = sum(LEPMICbiom),
             LEPPLAmass = sum(LEPPLAbiom), LEPPUNmass = sum(LEPPUNbiom), LEPSPPmass = sum(LEPSPPbiom),
             MICSALmass = sum(MICSALbiom),
             AMENAT = sum(AMENAT), AMENEB = sum(AMENEB), AMICAL = sum(AMICAL), AMPMEA = sum(AMPMEA), 
             CICURO = sum(CICURO), ENNGLO = sum(ENNGLO), ERISUC = sum(ERISUC), FROG = sum(FROG), 
             LEPGUL = sum(LEPGUL), LEPMAC = sum(LEPMAC), LEPMAR = sum(LEPMAR), LEPMIC = sum(LEPMIC), 
             LEPPLA = sum(LEPPLA), LEPPUN = sum(LEPPUN), LEPSPP = sum(LEPSPP), MICSAL = sum(MICSAL), 
             NERSPP = sum(NERSPP), NOCATCH = sum(NOCATCH), NOFISH = sum(NOFISH), SIRLAC = sum(SIRLAC),
             TESTUD = sum(TESTUD), Predators = sum(Craypred + Predfishpres), Salamanders = sum(SIRLAC + AMPMEA))


#calculate the mean catch per net type for each given night (by year, season, macrocosm, hydropattern, etc.)

catch_by_type <- catchsum %>% 
  group_by (Cumulative, Wateryr, Season, Macrocosm, Hydropattern, Night, `Net Type`) %>% 
  summarize (Predfishmass = mean(Predfishmass), Predfishcount = mean(Predfishcount), AMENATmass = mean(AMENATmass),
             AMENEBmass = mean(AMENEBmass), AMICALmass = mean(AMICALmass), CICUROmass = mean(CICUROmass),
             ENNGLOmass = mean(ENNGLOmass), ERiSUCmass = mean(ERiSUCmass), LEPGULmass = mean(LEPGULmass),
             LEPMACmass = mean(LEPMACmass), LEPMARmass = mean(LEPMARmass), LEPMICmass = mean(LEPMICmass),
             LEPPLAmass = mean(LEPPLAmass), LEPPUNmass = mean(LEPPUNmass), LEPSPPmass = mean(LEPSPPmass),
             MICSALmass = mean(MICSALmass), AMENAT = mean(AMENAT), AMENEB = mean(AMENEB), AMICAL = mean(AMICAL), 
             AMPMEA = mean(AMPMEA), CICURO = mean(CICURO), ENNGLO = mean(ENNGLO), ERISUC = mean(ERISUC), 
             FROG = mean(FROG), LEPGUL = mean(LEPGUL), LEPMAC = mean(LEPMAC), LEPMAR = mean(LEPMAR), 
             LEPMIC = mean(LEPMIC), LEPPLA = mean(LEPPLA), LEPPUN = mean(LEPPUN), LEPSPP = mean(LEPSPP),
             MICSAL = mean(MICSAL), NERSPP = mean(NERSPP), NOCATCH = mean(NOCATCH), NOFISH = mean(NOFISH),
             SIRLAC = mean(SIRLAC), TESTUD = mean(TESTUD), Predators = mean(Predators), 
             Salamanders = mean(Salamanders))

# sum the means of the two nets types for each night

catch_by_night <- catch_by_type %>% 
  group_by (Cumulative, Wateryr, Season, Macrocosm, Hydropattern, Night) %>% 
  summarize (Predfishmass = sum(Predfishmass), Predfishcount = sum(Predfishcount), AMENATmass = sum(AMENATmass),
             AMENEBmass = sum(AMENEBmass), AMICALmass = sum(AMICALmass), CICUROmass = sum(CICUROmass),
             ENNGLOmass = sum(ENNGLOmass), ERiSUCmass = sum(ERiSUCmass), LEPGULmass = sum(LEPGULmass),
             LEPMACmass = sum(LEPMACmass), LEPMARmass = sum(LEPMARmass), LEPMICmass = sum(LEPMICmass),
             LEPPLAmass = sum(LEPPLAmass), LEPPUNmass = sum(LEPPUNmass), LEPSPPmass = sum(LEPSPPmass),
             MICSALmass = sum(MICSALmass), AMENAT = sum(AMENAT), AMENEB = sum(AMENEB), AMICAL = sum(AMICAL), 
             AMPMEA = sum(AMPMEA), CICURO = sum(CICURO), ENNGLO = sum(ENNGLO), ERISUC = sum(ERISUC), 
             FROG = sum(FROG), LEPGUL = sum(LEPGUL), LEPMAC = sum(LEPMAC), LEPMAR = sum(LEPMAR), 
             LEPMIC = sum(LEPMIC), LEPPLA = sum(LEPPLA), LEPPUN = sum(LEPPUN), LEPSPP = sum(LEPSPP),
             MICSAL = sum(MICSAL), NERSPP = sum(NERSPP), NOCATCH = sum(NOCATCH), NOFISH = sum(NOFISH),
             SIRLAC = sum(SIRLAC), TESTUD = sum(TESTUD), Predators = sum(Predators), Salamanders = sum(Salamanders))



# average and calculate standard deviation of the standardized catches of the multiple nights each season
# TO gind SD you must run the SD separately from the data.

catch_standardized <- catch_by_night %>% 
  group_by (Cumulative, Wateryr, Season, Macrocosm, Hydropattern) %>% 
  summarize (Predfishmass = mean(Predfishmass), Predfishcount = mean(Predfishcount), AMENATmass = mean(AMENATmass),
             AMENEBmass = mean(AMENEBmass), AMICALmass = mean(AMICALmass), CICUROmass = mean(CICUROmass),
             ENNGLOmass = mean(ENNGLOmass), ERiSUCmass = mean(ERiSUCmass), LEPGULmass = mean(LEPGULmass),
             LEPMACmass = mean(LEPMACmass), LEPMARmass = mean(LEPMARmass), LEPMICmass = mean(LEPMICmass),
             LEPPLAmass = mean(LEPPLAmass), LEPPUNmass = mean(LEPPUNmass), LEPSPPmass = mean(LEPSPPmass),
             MICSALmass = mean(MICSALmass), AMENAT = mean(AMENAT), AMENEB = mean(AMENEB), AMICAL = mean(AMICAL), 
             AMPMEA = mean(AMPMEA), CICURO = mean(CICURO), ENNGLO = mean(ENNGLO), ERISUC = mean(ERISUC), 
             FROG = mean(FROG), LEPGUL = mean(LEPGUL), LEPMAC = mean(LEPMAC), LEPMAR = mean(LEPMAR), 
             LEPMIC = mean(LEPMIC), LEPPLA = mean(LEPPLA), LEPPUN = mean(LEPPUN), LEPSPP = mean(LEPSPP),
             MICSAL = mean(MICSAL), NERSPP = mean(NERSPP), NOCATCH = mean(NOCATCH), NOFISH = mean(NOFISH),
             SIRLAC = mean(SIRLAC), TESTUD = mean(TESTUD), Predators = mean(Predators), 
             Salamanders = mean(Salamanders))

catch_standardized_SD <- catch_by_night %>% 
  group_by (Cumulative, Wateryr, Season, Macrocosm, Hydropattern) %>% 
  summarize (Predfishmass_sd = sd(Predfishmass), Predfishcount_sd = sd(Predfishcount), AMENATmass_sd = sd(AMENATmass),
             AMENEBmass_sd = sd(AMENEBmass), AMICALmass_sd = sd(AMICALmass), CICUROmass_sd = sd(CICUROmass),
             ENNGLOmass_sd = sd(ENNGLOmass), ERiSUCmass_sd = sd(ERiSUCmass), LEPGULmass_sd = sd(LEPGULmass),
             LEPMACmass_sd = sd(LEPMACmass), LEPMARmass_sd = sd(LEPMARmass), LEPMICmass_sd = sd(LEPMICmass),
             LEPPLAmass_sd = sd(LEPPLAmass), LEPPUNmass_sd = sd(LEPPUNmass), LEPSPPmass_sd = sd(LEPSPPmass),
             MICSALmass_sd = sd(MICSALmass), AMENAT_sd = sd(AMENAT), AMENEB_sd = sd(AMENEB), AMICAL_sd = sd(AMICAL), 
             AMPMEA_sd = sd(AMPMEA), CICURO_sd = sd(CICURO), ENNGLO_sd = sd(ENNGLO), ERISUC_sd = sd(ERISUC), 
             FROG_sd = sd(FROG), LEPGUL_sd = sd(LEPGUL), LEPMAC_sd = sd(LEPMAC), LEPMAR_sd = sd(LEPMAR), 
             LEPMIC_sd = sd(LEPMIC), LEPPLA_sd = sd(LEPPLA), LEPPUN_sd = sd(LEPPUN), LEPSPP_sd = sd(LEPSPP),
             MICSAL_sd = sd(MICSAL), NERSPP_sd = sd(NERSPP), NOCATCH_sd = sd(NOCATCH), NOFISH_sd = sd(NOFISH),
             SIRLAC_sd = sd(SIRLAC), TESTUD_sd = sd(TESTUD), Predators_sd = sd(Predators), 
             Salamanders_sd = sd(Salamanders))

###################################################################################################################################


#################                SECTION 3: STATISTICAL ANALYSIS OF CPUE DATA OF LPFA                           ###################


###################################################################################################################################


#ANALYSIS 1
###          A: TREATMENT + WATERYEAR + TREATMENT*WATERYEAR
###          B: TREATMENT + WATERYEAR + TREATMENT*WATERYEAR + SEASON
###          C: TREATMENT + WATERYEAR + TREATMENT*WATERYEAR + SEASON + TREATMENT*SEASON

#Section A:  MODEL.tw =  TREATMENT + WATERYEAR + TREATMENT*WATERYEAR

model.LPFA_tw <- lme(Predfishcount ~ Hydropattern + Wateryr + Hydropattern*Wateryr,
                             random = ~1|Macrocosm,
                             data = catch_standardized)


# Run ACF for modeltw
ACF(model.LPFA_tw)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_A_1 <- -0.4153745
ACF_A_2 <- -0.1606383
ACF_A_3 <-  0.6445236
ACF_A_4 <- -0.5393235
ACF_A_5 <-  0.6568278


# run the rmANOVA

model.tw <- lme(Predfishcount ~ factor(Hydropattern)*ordered(Wateryr), 
               random = ~1|Macrocosm, correlation = corAR1(form = ~1|Macrocosm, value = ACF_A_1, ACF_A_2),
               data = catch_standardized, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 103.502 and Complex AIC = 101.7286

summary(model.tw)
anova(model.tw)
#                                  numDF denDF   F-value p-value
#(Intercept)                               1    16 130.44375  <.0001
#factor(Hydropattern)                      1     2   4.47670  0.1686
#ordered(Wateryr)                          2    16   5.22599  0.0179
#factor(Hydropattern):ordered(Wateryr)     2    16   1.04344  0.3750

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.tw <- plot(resid(model.tw))


#Check assumption of homogeneity of variance:
catch_standardized$restw <- residuals(model.tw) #extracts residual and places them in a new column in dataframe
catch_standardized$absrestw <- abs(catch_standardized$restw) #creates new column with absolute value of residuals
catch_standardized$restw2 <- catch_standardized$absrestw^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.tw <- lm(restw2 ~ Macrocosm, data = catch_standardized) #anova of the squared residuals
anova(Levene.model.tw) # P = 0.3172


# Test for normal distribution of residuals
shapiro.test(residuals(model.tw)) # P = 0.6831
qqnorm(model.tw$residuals)
qqline(model.tw$residuals)
anova(model.tw)

############################################ LOG TRANSFORM FOR MODEL COMPARISON  ####################################################

model.LPFA_tw_log <- lme(log_Predfishcount ~ Hydropattern + Wateryr + Hydropattern*Wateryr,
                     random = ~1|Macrocosm,
                     data = catch_standardized)


# Run ACF for modeltw
ACF(model.LPFA_tw_log)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_A_1_log <- -0.3470672
ACF_A_2_log <- -0.2007724
ACF_A_3_log <-  0.4380313
ACF_A_4_log <- -0.4643277
ACF_A_5_log <-  0.4463311


# run the rmANOVA

model.tw_log <- lme(log_Predfishcount ~ factor(Hydropattern)*ordered(Wateryr), 
                random = ~1|Macrocosm, correlation = corAR1(form = ~1|Macrocosm, value = ACF_A_1_log, ACF_A_2_log),
                data = catch_standardized, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = XXXXXXXX and Complex AIC = 55.65274 

summary(model.tw_log)
anova(model.tw)
#                                  numDF denDF   F-value p-value
#(Intercept)                               1    16 130.44375  <.0001
#factor(Hydropattern)                      1     2   4.47670  0.1686
#ordered(Wateryr)                          2    16   5.22599  0.0179
#factor(Hydropattern):ordered(Wateryr)     2    16   1.04344  0.3750

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.tw_log <- plot(resid(model.tw_log))


#Check assumption of homogeneity of variance:
catch_standardized$restwlog <- residuals(model.tw_log) #extracts residual and places them in a new column in dataframe
catch_standardized$absrestwlog <- abs(catch_standardized$restwlog) #creates new column with absolute value of residuals
catch_standardized$restwlog2 <- catch_standardized$absrestwlog^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.tw_log <- lm(restwlog2 ~ Macrocosm, data = catch_standardized) #anova of the squared residuals
anova(Levene.model.tw_log) # P = 0.1962


# Test for normal distribution of residuals
shapiro.test(residuals(model.tw_log)) # P = 0.3978
qqnorm(model.tw_log$residuals)
qqline(model.tw_log$residuals)
anova(model.tw_log)

################################################################################################################################

# SECTION B: TREATMENT + WATERYEAR + TREATMENT*WATERYEAR + SEASON 


model.LPFA_tws <- lme(Predfishcount ~ factor(Hydropattern)*ordered(Wateryr) + factor(Season),
                     random = ~1|Macrocosm,
                     data = catch_standardized)


# Run ACF for model tws
ACF(model.LPFA_tws)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_B_1 <- -0.3620456
ACF_B_2 <- -0.1595930
ACF_B_3 <-  0.5323835
ACF_B_4 <- -0.5416300
ACF_B_5 <-  0.7873312

anova(model.LPFA_tws)

# run the rmANOVA

model.tws <- lme(Predfishcount ~ factor(Hydropattern)*ordered(Wateryr) + factor(Season), 
                 random = ~1|Macrocosm, correlation = corAR1(form = ~1|Macrocosm, value = ACF_B_1, ACF_B_2),
                 data = catch_standardized, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 103.6322 and Complex AIC = 101.6329 

summary(model.tws)
anova(model.tws)
#                                      numDF denDF   F-value p-value
#(Intercept)                               1    15 128.13086  <.0001
#factor(Hydropattern)                      1     2   4.53677  0.1669
#ordered(Wateryr)                          2    15   4.41870  0.0310
#factor(Season)                            1    15   0.00286  0.9581
#factor(Hydropattern):ordered(Wateryr)     2    15   0.88307  0.4339

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.tws <- plot(resid(model.tws))


#Check assumption of homogeneity of variance:
catch_standardized$restws <- residuals(model.tws) #extracts residual and places them in a new column in dataframe
catch_standardized$absrestws <- abs(catch_standardized$restws) #creates new column with absolute value of residuals
catch_standardized$restws2 <- catch_standardized$absrestws^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.tws <- lm(restws2 ~ Macrocosm, data = catch_standardized) #anova of the squared residuals
anova(Levene.model.tws) # P = 0.01


# Test for normal distribution of residuals
shapiro.test(residuals(model.tws)) # P = 0.5171
qqnorm(model.tws$residuals)
qqline(model.tws$residuals)
anova(model.tws)
#Section C:  MODEL.twst =  TREATMENT + WATERYEAR + TREATMENT*WATERYEAR + SEASON + TRETMENT*SEASON

model.LPFA_twst <- lme(Predfishcount ~ Hydropattern + Wateryr + Hydropattern*Wateryr + Season + Hydropattern*Season,
                     random = ~1|Macrocosm,
                     data = catch_standardized)



###############         ERRORRRR          ####################

# model.tws failed levene's test of homogeneity of variance so data must be transformed. 
# log transform predatory fish data and rerun analysis

catch_standardized$log_Predfishcount <- log(catch_standardized$Predfishcount)


#RUN treatment + Wateryr + Treatmetn*Wateryr + Season model with log trasnformed data
model.LPFA_tws_log <- lme(log_Predfishcount ~ factor(Hydropattern)*ordered(Wateryr) + factor(Season),
                      random = ~1|Macrocosm,
                      data = catch_standardized)


# Run ACF for model tws
ACF(model.LPFA_tws_log)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_B_1_log <- -0.2542312
ACF_B_2_log <- -0.1997330
ACF_B_3_log <-  0.3660122
ACF_B_4_log <- -0.4172468
ACF_B_5_log <-  0.5452984

anova(model.LPFA_tws)

# run the rmANOVA

model.tws_log <- lme(log_Predfishcount ~ factor(Hydropattern)*ordered(Wateryr) + factor(Season), 
                 random = ~1|Macrocosm, correlation = corAR1(form = ~1|Macrocosm, value = ACF_B_2_log, ACF_B_3_log),
                 data = catch_standardized, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 59. and Complex AIC = 57.87692 

summary(model.tws_log)
anova(model.tws_log)

#                                      numDF denDF   F-value p-value
#(Intercept)                               1    15 104.30515  <.0001
#factor(Hydropattern)                      1     2   4.07806  0.1809
#ordered(Wateryr)                          2    15   4.58354  0.0280
#factor(Season)                            1    15   0.11949  0.7344
#factor(Hydropattern):ordered(Wateryr)     2    15   1.21287  0.3249

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.tws_log <- plot(resid(model.tws_log))


#Check assumption of homogeneity of variance:
catch_standardized$restws_log <- residuals(model.tws_log) #extracts residual and places them in a new column in dataframe
catch_standardized$absrestws_log <- abs(catch_standardized$restws_log) #creates new column with absolute value of residuals
catch_standardized$restws2_log <- catch_standardized$absrestws_log^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.tws_log <- lm(restws2_log ~ Macrocosm, data = catch_standardized) #anova of the squared residuals
anova(Levene.model.tws_log) # P = 0.1984


# Test for normal distribution of residuals
shapiro.test(residuals(model.tws_log)) # P = 0.7652
qqnorm(model.tws_log$residuals)
qqline(model.tws_log$residuals)
anova(model.tws_log)


#Section C:  MODEL.twst =  TREATMENT + WATERYEAR + TREATMENT*WATERYEAR + SEASON + TRETMENT*SEASON

model.LPFA_twst <- lme(Predfishcount ~ Hydropattern + Wateryr + Hydropattern*Wateryr + Season + Hydropattern*Season,
                       random = ~1|Macrocosm,
                       data = catch_standardized)




# Run ACF for model twst
ACF(model.LPFA_twst)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_C_1 <- -0.4028871
ACF_C_2 <- -0.1899145
ACF_C_3 <-  0.6551319
ACF_C_4 <- -0.5056734
ACF_C_5 <-  0.6239362

# run the rmANOVA

model.twts <- lme(Predfishcount ~ factor(Hydropattern)*ordered(Wateryr) + factor(Hydropattern)*factor(Season), 
                random = ~1|Macrocosm, correlation = corAR1(form = ~1|Macrocosm, value = ACF_C_1, ACF_C_2),
                data = catch_standardized, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 103.502 and Complex AIC = 101.7286

summary(model.twts)
anova(model.twts)


#                                      numDF denDF   F-value p-value
#(Intercept)                               1    14 129.90815  <.0001
#factor(Hydropattern)                      1     2   4.49054  0.1682
#ordered(Wateryr)                          2    14   4.46656  0.0316
#factor(Season)                            1    14   0.00045  0.9834
#factor(Hydropattern):ordered(Wateryr)     2    14   0.89192  0.4319
#factor(Hydropattern):factor(Season)       1    14   0.00178  0.9670

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.twts <- plot(resid(model.twts))


#Check assumption of homogeneity of variance:
catch_standardized$restwts <- residuals(model.twts) #extracts residual and places them in a new column in dataframe
catch_standardized$absrestwts <- abs(catch_standardized$restwts) #creates new column with absolute value of residuals
catch_standardized$restwts2 <- catch_standardized$absrestwts^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.twts <- lm(restwts2 ~ Macrocosm, data = catch_standardized) #anova of the squared residuals
anova(Levene.model.twts) # P = 0.01


# Test for normal distribution of residuals
shapiro.test(residuals(model.twts)) # P = 0.479
qqnorm(model.twts$residuals)
qqline(model.twts$residuals)
anova(model.twts)


###############         ERRORRRR          ####################

# model.twst failed levene's test of homogeneity of variance so data must be transformed. 
# log transform predatory fish data and rerun analysis

#RUN treatment + Wateryr + Treatmetn*Wateryr + Season model with log trasnformed data
model.LPFA_twst_log <- lme(log_Predfishcount ~ factor(Hydropattern)*ordered(Wateryr) + factor(Hydropattern)*factor(Season),
                          random = ~1|Macrocosm,
                          data = catch_standardized)


# Run ACF for model tws
ACF(model.LPFA_twst_log)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_C_1_log <- -0.1955982
ACF_C_2_log <- -0.3001255
ACF_C_3_log <-  0.4382224
ACF_C_4_log <- -0.4033362
ACF_C_5_log <-  0.5423312

anova(model.LPFA_tws)

# run the rmANOVA

model.twst_log <- lme(log_Predfishcount ~ factor(Hydropattern)*ordered(Wateryr) + factor(Season), 
                     random = ~1|Macrocosm, correlation = corAR1(form = ~1|Macrocosm, value = ACF_C_1_log, ACF_C_2_log),
                     data = catch_standardized, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 59.84416 and Complex AIC = 57.87239 

summary(model.twst_log)
anova(model.twst_log)
#                                      numDF denDF   F-value p-value
#(Intercept)                               1    15 104.28314  <.0001
#factor(Hydropattern)                      1     2   4.08646  0.1806
#ordered(Wateryr)                          2    15   4.54461  0.0286
#factor(Season)                            1    15   0.12197  0.7318
#factor(Hydropattern):ordered(Wateryr)     2    15   1.20191  0.3280

# Check for violations of assumptions
# Assumption Testing:
# Assumption 1: Linearity of data (Explanatory variables are related linearly to the response variable/s)
#         Explained: plot residuals, if there is obvious patterning in the plot this assumption is VIOLATED!!!
plot.model.twst <- plot(resid(model.twst_log))


#Check assumption of homogeneity of variance:
catch_standardized$restwst_log <- residuals(model.twst_log) #extracts residual and places them in a new column in dataframe
catch_standardized$absrestwst_log <- abs(catch_standardized$restwst_log) #creates new column with absolute value of residuals
catch_standardized$restwst2_log <- catch_standardized$absrestwst_log^2 #squares ths absolute value of residuals to provide more robust estimate
Levene.model.twst_log <- lm(restwst2_log ~ Macrocosm, data = catch_standardized) #anova of the squared residuals
anova(Levene.model.twst_log) # P = 0.1986


# Test for normal distribution of residuals
shapiro.test(residuals(model.twst_log)) # P = 0.7685
qqnorm(model.twst_log$residuals)
qqline(model.twst_log$residuals)
anova(model.twst_log)
