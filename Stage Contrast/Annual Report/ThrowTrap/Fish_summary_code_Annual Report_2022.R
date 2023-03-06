### This code summarized throw trap data from LILA during the wet-season-stage contrast experiment ###
### that began in the wet season of 2018. ###

### data management help https://urldefense.com/v3/__https://bouchat.github.io/IntroDataMgmt20Jan.html*introduction_to_r__;Iw!!FjuHKAHQs5udqho!KplAcfmVaxfmgZi7JQBeSpv1KwYTV3WwZN41DByxf9OmOuEe-o-6GPBYEgzFpLxjqU-BRDeib2BRKEZDpTIJ$  
## data management help: Base R cheat sheet.

library(tidyverse)
library(readxl)
library(nlme)
library(car)
library(emmeans)
library(ggplot2)

# tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats

### Section one:
# Load in raw fish  data from throw traps over multiple macrocosms at LILA and multiple seasons

Fish_SU_2018 <- read_csv("M:/LILA/LILA Data Entry/2018/LILA_TT_2018_FISH_SUMMER.csv", na = ".")
Fish_SP_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Throw Trapping/Spring/LILA_TT_2019_FISH_SPRING.csv", na = ".")
Fish_SU_2019 <- read_csv("M:/LILA/LILA Data Entry/2019/Throw Trapping/Summer/LILA_TT_2019_FISH_SUMMER.csv", na = ".")
Fish_SP_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Throw Trapping/Spring/LILA_TT_2020_FISH_SPRING.csv", na = ".")
Fish_SU_2020 <- read_csv("M:/LILA/LILA Data Entry/2020/Throw Trapping/Summer/LILA_TT_2020_FISH_SUMMER.csv", na = ".")
Fish_SP_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Throw Trapping/Spring/LILA_TT_2021_FISH_SPRING.csv", na = ".")
Fish_SU_2021 <- read_csv("M:/LILA/LILA Data Entry/2021/Throw Trapping/Summer/LILA_TT_2021_FISH_SUMMER.csv", na = ".")
Fish_SP_2022 <- read_excel("M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_FISH_SPRING.xlsx", na = ".")
Fish_SU_2022 <- read_excel("M:/LILA/LILA Data Entry/2022/Throw Trapping/Summer/LILA_TT_2022_FISH_SUMMER.xlsx", na = ".")
# Raw Fish data should have 15 variables: Session, Year, Month, Day, Wetland, Throw, Species,
#     Length, Sex, Form, Comments, Sorted By, Entered By, Checked By

#Merge all Cray data sets together into one dataframe called catch
catch <- rbind(Fish_SU_2018, Fish_SP_2019, Fish_SU_2019, Fish_SP_2020,
               Fish_SU_2020, Fish_SP_2021, Fish_SU_2021, Fish_SP_2022,
               Fish_SU_2022)

# Remove unnecessary data (Sorted By, Checked By, Enetered By) from the catch data frame
catch <- dplyr::select(catch,one_of(c('Session','Year','Month','Day','Wetland','Throw','Species','Length',
                                      'Sex','Comments')))

#Check to see that all data was imported and combined properly
summary(catch)
catch$Species <- as.factor(catch$Species)
catch$Sex <- as.factor(catch$Sex)
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
                              gsub("Summer 2022", "dry",
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
catch <- within(catch, Length [Length == "." & Species == "NOFISH"] <- 0)

# Check data frame to make sure variables are factors, and ordered where the factors are ordered. 
summary(catch)
is.character(catch$Wetland)
is.character(catch$Location)
is.factor(catch$Season)
is.numeric(catch$Wateryr)
is.numeric(catch$Cumulative)
is.character(catch$Species)
is.numeric(catch$Length)

# Length is not registering as a numeric so it must be converted to a numeric
catch$Length <- as.numeric(catch$Length, na.action = 0)

# Section One: Fish Counts
# First create counts of Fish, using f(x) dplyr::if_else()
# if-else syntax: if_else(condition, true, falso, missing = NULL)
# In the case of calculating PROFAL catch (see below): 
# condition = catch$species == 'GAMHOL', if true mark with 1, if false mark with 0.


catch$CICURO <- dplyr::if_else(catch$Species == 'CICURO', 1, 0) 
#CICURO = cichlasoma uropthalmus, mayan cichlid
catch$CLABAT <- dplyr::if_else(catch$Species == 'CLABAT', 1, 0)
#CLABAT = clarius batrachus, ealking catfish
catch$ELAEVE <- dplyr::if_else(catch$Species == 'ELAEVE', 1, 0) 
#ELAEVE = elassoma evergladei, everglades pygmy sunfish
catch$ENNGLO <- dplyr::if_else(catch$Species == 'ENNGLO', 1, 0) 
#ENNGLO = ennecanthus gloriosus, bluespotted sunfish
catch$ERISUC <- dplyr::if_else(catch$Species == 'ERISUC', 1, 0) 
#ERISUC = erimyzon succetta, lake chubsucker
catch$ETHFUS <- dplyr::if_else(catch$Species == 'ETHFUS', 1, 0) 
#ETHFUS = etheostoma fusiforme, swamp darter
catch$FUNCHR <- dplyr::if_else(catch$Species == 'FUNCHR', 1, 0) 
#FUNCHR = fundulus chrysotus, golden topminnow
catch$FUNCON <- dplyr::if_else(catch$Species == 'FUNCON', 1, 0)
#FUNCON = fundulus confluentus, marsh killifish
catch$GAMHOL <- dplyr::if_else(catch$Species == 'GAMHOL', 1, 0) 
#GAMHOL = gambusia holbrooki, eastern mosquitofish
catch$HETFOR <- dplyr::if_else(catch$Species == 'HETFOR', 1, 0) 
#HETFOR = heterandria formosa, least killifish
catch$JORFLO <- dplyr::if_else(catch$Species == 'JORFLO', 1, 0)
#JORFLO = jordanella floridae, flagfish
catch$LABSIC <- dplyr::if_else(catch$Species == 'LABSIC', 1, 0)
#LABSIC = labidesthes sicculus, brook silverside
catch$LEPGUL <- dplyr::if_else(catch$Species == 'LEPGUL', 1, 0) 
#LEPGUL= lepomis gulosos, warmouth
catch$LEPMAC <- dplyr::if_else(catch$Species == 'LEPMAC', 1, 0) 
#LEPMAC= lepomis macrochirus, bluegill sunfish
catch$LEPMAR <- dplyr::if_else(catch$Species == 'LEPMAR', 1, 0) 
#LEPMAR = lepomis marginatus, dollar sunfish 
catch$LEPPUN <- dplyr::if_else(catch$Species == 'LEPPUN', 1, 0) 
#LEPPUN = lepomis punctatus, spotted sunfish
catch$LEPMIC <- dplyr::if_else(catch$Species == 'LEPMIC', 1, 0) 
#LEPMIC = lepomis microlophus, redear sunfish
catch$LEPSPP <- dplyr::if_else(catch$Species == 'LEPSPP', 1, 0) 
#LEPSPP = lepomis species, sunfishes that were unable to be identifeid to species
catch$LUCGOO <- dplyr::if_else(catch$Species == 'LUCGOO', 1, 0) 
#LUCGOO = lucania goodei, bluefin killifish
catch$MICSAL <- dplyr::if_else(catch$Species == 'MICSAL', 1, 0) 
#MICSAL = micropterus salmoides = largemouth bass
catch$NOFISH <- dplyr::if_else(catch$Species == 'NOFISH', 1, 0)
#NOFISH = No fish caught in throw trap
catch$NOTCRY <- dplyr::if_else(catch$Species == 'NOTCRY', 1, 0) 
#NOTCRY = notemigonus crysoleucas, golden shiner
catch$OREAUR <- dplyr::if_else(catch$Species == 'OREAUR', 1, 0) 
#OREAUR = Oreochromis aureus, blue tilapia
catch$POELAT <- dplyr::if_else(catch$Species == 'POELAT', 1, 0) 
#POELAT = poecilia latipinna, sailfin molly
catch$UNKSPP <- dplyr::if_else(catch$Species == 'UNKSPP', 1, 0)


# Section Two: Fish Biomass and presence 
# Create Biomass column by calculating biomass using regressions
# Regression data for most species came from Loftus and Trexler
# Regressions for wet mass were multiplied by 0.2 to obtain cry mass
# Dry mass regression have no 0.2 multiplier
# Biomass is in mg dry mass

catch$CICURObiom <- dplyr::if_else(catch$Species == 'CICURO', 0.2*(10^(-4.114+2.912*log10(catch$Length)))*1000, 0) 
catch$CLABATbiom <- dplyr::if_else(catch$Species == 'CLABAT', 0.2*(10^(-4.844+2.92*log10(catch$Length)))*1000, 0)
catch$ELAEVEbiom <- dplyr::if_else(catch$Species == 'ELAEVE', 0.2*(10^(-4.581+3.031*log10(catch$Length)))*1000, 0) 
catch$ENNGLObiom <- dplyr::if_else(catch$Species == 'ENNGLO', 0.2*(10^(-4.624+3.113*log10(catch$Length)))*1000, 0) 
catch$ERISUCbiom <- dplyr::if_else(catch$Species == 'ERISUC', 0.2*(10^(-5.236+3.305*log10(catch$Length)))*1000, 0) 
catch$ETHFUSbiom <- dplyr::if_else(catch$Species == 'ETHFUS', 0.2*(10^(-5.686+3.453*log10(catch$Length)))*1000, 0) 
catch$FUNCHRbiom <- dplyr::if_else(catch$Species == 'FUNCHR', (10^(-2.9311+3.3635*log10(catch$Length))), 0) 
catch$FUNCONbiom <- dplyr::if_else(catch$Species == 'FUNCON', (10^(-2.7335+3.2479*log10(catch$Length))), 0)
catch$GAMHOLbiom <- dplyr::if_else(catch$Species == 'GAMHOL', (10^(-3.1538+3.5955*log10(catch$Length))), 0) 
catch$HETFORbiom <- dplyr::if_else(catch$Species == 'HETFOR', (10^(-2.053+2.7391*log10(catch$Length))), 0) 
catch$JORFLObiom <- dplyr::if_else(catch$Species == 'JORFLO', (10^(-3.325+3.9214*log10(catch$Length))), 0)
catch$LABSICbiom <- dplyr::if_else(catch$Species == 'LABSIC', 0.2*(10^(-5.29+3.065*log10(catch$Length)))*1000, 0)
catch$LEPGULbiom <- dplyr::if_else(catch$Species == 'LEPGUL', 0.2*(10^(-4.889+3.224*log10(catch$Length)))*1000, 0) 
catch$LEPMACbiom <- dplyr::if_else(catch$Species == 'LEPMAC', (10^(-1.7881+2.8052*log10(catch$Length))), 0) 
catch$LEPMARbiom <- dplyr::if_else(catch$Species == 'LEPMAR', (10^(-2.5949+3.3278*log10(catch$Length))), 0) 
catch$LEPPUNbiom <- dplyr::if_else(catch$Species == 'LEPPUN', (10^(-1.7881+2.8052*log10(catch$Length))), 0) 
catch$LEPMICbiom <- dplyr::if_else(catch$Species == 'LEPMIC', (10^(-1.7881+2.8052*log10(catch$Length))), 0) 
catch$LEPSPPbiom <- dplyr::if_else(catch$Species == 'LEPSPP', (10^(-1.7881+2.8052*log10(catch$Length))), 0) 
catch$LUCGOObiom <- dplyr::if_else(catch$Species == 'LUCGOO', (10^(-2.3608+2.9747*log10(catch$Length))), 0) 
catch$MICSALbiom <- dplyr::if_else(catch$Species == 'MICSAL', 0.2*(10^(-5.203+3.244*log10(catch$Length)))*1000, 0) 
catch$NOFISHbiom <- dplyr::if_else(catch$Species == 'NOFISH', 0, 0)
catch$NOTCRYbiom <- dplyr::if_else(catch$Species == 'NOTCRY', 0.2*(10^(-4.974+3.104*log10(catch$Length)))*1000, 0) 
catch$OREAURbiom <- dplyr::if_else(catch$Species == 'OREAUR', 0.2*(10^(-4.114+2.912*log10(catch$Length)))*1000, 0) 
catch$POELATbiom <- dplyr::if_else(catch$Species == 'POELAT', (10^(-2.7355+3.368*log10(catch$Length))), 0) 
catch$UNKSPPbiom <- dplyr::if_else(catch$Species == 'UNKSPP', 0, 0)

# Create a fish biomass column (not by species) for purposes of summary data
catch$Fishbiom <- catch$CICURObiom + catch$CLABATbiom + catch$ELAEVEbiom + catch$ENNGLObiom + catch$ERISUCbiom + 
                  catch$ETHFUSbiom + catch$FUNCHRbiom + catch$FUNCONbiom + catch$GAMHOLbiom + catch$HETFORbiom + 
                  catch$JORFLObiom + catch$LABSICbiom + catch$LEPGULbiom + catch$LEPMACbiom + catch$LEPMARbiom + 
                  catch$LEPPUNbiom + catch$LEPMICbiom + catch$LEPSPPbiom + catch$LUCGOObiom + catch$MICSALbiom + 
                  catch$NOFISHbiom + catch$NOTCRYbiom + catch$OREAURbiom + catch$POELATbiom + catch$UNKSPPbiom

#Create a centrarchid biomass column for purposes of summary data
catch$CENTbiom <- catch$LEPGULbiom + catch$LEPMACbiom + catch$LEPMARbiom + catch$LEPMICbiom + 
                  catch$LEPPUNbiom + catch$LEPSPPbiom + catch$MICSALbiom + catch$ENNGLObiom

#create a fish presence column (not by species) for purposes of a count in the summary data
catch$Fishpres <- catch$CICURO + catch$CLABAT + catch$ELAEVE + catch$ENNGLO + catch$ERISUC +
                  catch$ETHFUS + catch$FUNCHR + catch$FUNCON + catch$GAMHOL + catch$HETFOR +
                  catch$JORFLO + catch$LABSIC + catch$LEPGUL + catch$LEPMAC + catch$LEPMAR +
                  catch$LEPPUN + catch$LEPMIC + catch$LEPSPP + catch$LUCGOO + catch$MICSAL +
                  catch$NOFISH + catch$NOTCRY + catch$OREAUR + catch$POELAT + catch$UNKSPP 
  
#create a centrarchid presence column (not by species) for purposes of a count in the summary data
catch$CENTPR <-  catch$LEPGUL + catch$LEPMAC + catch$LEPMAR + catch$LEPPUN + 
                 catch$LEPMIC + catch$LEPSPP + catch$MICSAL + catch$ENNGLO


# Section Three: This code manipulates variables and creates new dataframes that summarize species level biomass and counts
### summaries of counts and biomass were made by:
### Cumulative, Wateryr, Season, Wetland, Hydro, and Location
### This summarizes bimass and counts by TT, Location (aka habitat), wetland, season.

summary(catch)

# Check to see if Wateryr is a numeric and Season is a factor
is.numeric(catch$Wateryr)
is.factor(catch$Season)
# both came back as true no fixing needed.

## Part A: Build the summary file
## Build the summary file
## Summarize mass total, by species, and counts at the trap scale using a pipe operator %>%
## calculate the  catch per trap for each throw trap by:
## Cumulative,  Wateryr, Season, Wetland, Hydro, Location)

trap_catch <- catch %>%
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Throw, Location) %>% 
  summarize(Fishmass = sum(Fishbiom), Fishcount = sum(Fishpres), CICUROmass = sum(CICURObiom), CLABATmass = sum(CLABATbiom),
            ELAEVEmass = sum(ELAEVEbiom), ENNGLOmass = sum(ENNGLObiom), ERISUCmass = sum(ERISUCbiom), ETHFUSmass = sum(ETHFUSbiom),
            FUNCHRmass = sum(FUNCHRbiom), FUNCONmass = sum(FUNCONbiom), GAMHOLmass = sum(GAMHOLbiom), HETFORmass = sum(HETFORbiom),
            JORFLOmass = sum(JORFLObiom), LABSICmass = sum(LABSICbiom), LEPGULmass = sum(LEPGULbiom), LEPMACmass = sum(LEPMACbiom),
            LEPMARmass = sum(LEPMARbiom), LEPPUNmass = sum(LEPPUNbiom), LEPMICmass = sum(LEPMICbiom), LEPSPPmass = sum(LEPSPPbiom),
            LUCGOOmass = sum(LUCGOObiom), MICSALmass = sum(MICSALbiom), NOFISHmass = sum(NOFISHbiom), NOTCRYmass = sum(NOTCRYbiom),
            OREAURmass = sum(OREAURbiom), POELATmass = sum(POELATbiom), UNKSPPmass = sum(UNKSPPbiom),
            CICURO = sum(CICURO), CLABAT = sum(CLABAT), ELAEVE = sum(ELAEVE), ENNGLO = sum(ENNGLO), ERISUC = sum(ERISUC), 
            ETHFUS = sum(ETHFUS), FUNCHR = sum(FUNCHR), FUNCON = sum(FUNCON), GAMHOL = sum(GAMHOL), HETFOR = sum(HETFOR),
            JORFLO = sum(JORFLO), LABSIC = sum(LABSIC), LEPGUL = sum(LEPGUL), LEPMAC = sum(LEPMAC), LEPMAR = sum(LEPMAR),
            LEPPUN = sum(LEPPUN), LEPMIC = sum(LEPMIC), LEPSPP = sum(LEPSPP), LUCGOO = sum(LUCGOO), MICSAL = sum(MICSAL),
            NOFISH = sum(NOFISH), NOTCRY = sum(NOTCRY), OREAUR = sum(OREAUR), POELAT = sum(POELAT), UNKSPP = sum(UNKSPP))

## Part B:
## Calculate the total catch per habitat by Cumulative, Wateryr, Season, Wetland, Hydro)
## DS catch = sum of 10 TT (1-10), SS catch - sum of 4 TT (11-14), and CR catch = sum of 8 TT (15-22)

sum_hab_catch <- trap_catch %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Location) %>% 
  summarize(Fishmass = sum(Fishmass), Fishcount = sum(Fishcount), CICUROmass = sum(CICUROmass), CLABATmass = sum(CLABATmass),
            ELAEVEmass = sum(ELAEVEmass), ENNGLOmass = sum(ENNGLOmass), ERISUCmass = sum(ERISUCmass), ETHFUSmass = sum(ETHFUSmass),
            FUNCHRmass = sum(FUNCHRmass), FUNCONmass = sum(FUNCONmass), GAMHOLmass = sum(GAMHOLmass), HETFORmass = sum(HETFORmass),
            JORFLOmass = sum(JORFLOmass), LABSICmass = sum(LABSICmass), LEPGULmass = sum(LEPGULmass), LEPMACmass = sum(LEPMACmass),
            LEPMARmass = sum(LEPMARmass), LEPPUNmass = sum(LEPPUNmass), LEPMICmass = sum(LEPMICmass), LEPSPPmass = sum(LEPSPPmass),
            LUCGOOmass = sum(LUCGOOmass), MICSALmass = sum(MICSALmass), NOFISHmass = sum(NOFISHmass), NOTCRYmass = sum(NOTCRYmass),
            OREAURmass = sum(OREAURmass), POELATmass = sum(POELATmass), UNKSPPmass = sum(UNKSPPmass),
            CICURO = sum(CICURO), CLABAT = sum(CLABAT), ELAEVE = sum(ELAEVE), ENNGLO = sum(ENNGLO), ERISUC = sum(ERISUC), 
            ETHFUS = sum(ETHFUS), FUNCHR = sum(FUNCHR), FUNCON = sum(FUNCON), GAMHOL = sum(GAMHOL), HETFOR = sum(HETFOR),
            JORFLO = sum(JORFLO), LABSIC = sum(LABSIC), LEPGUL = sum(LEPGUL), LEPMAC = sum(LEPMAC), LEPMAR = sum(LEPMAR),
            LEPPUN = sum(LEPPUN), LEPMIC = sum(LEPMIC), LEPSPP = sum(LEPSPP), LUCGOO = sum(LUCGOO), MICSAL = sum(MICSAL),
            NOFISH = sum(NOFISH), NOTCRY = sum(NOTCRY), OREAUR = sum(OREAUR), POELAT = sum(POELAT), UNKSPP = sum(UNKSPP))

## Part C:
## Calculate the mean catch per habitat (by Cumulative, Wateryr, Season, Wetland, Hydro)
## DS catch = mean of 10 TT, SS catch = mean pf 4 TT, and CR catch = mean of 8 throw traps

mean_hab_catch <- trap_catch %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro, Location) %>%
  summarize(Fishmass = mean(Fishmass), Fishcount = mean(Fishcount), CICUROmass = mean(CICUROmass), CLABATmass = mean(CLABATmass),
            ELAEVEmass = mean(ELAEVEmass), ENNGLOmass = mean(ENNGLOmass), ERISUCmass = mean(ERISUCmass), ETHFUSmass = mean(ETHFUSmass),
            FUNCHRmass = mean(FUNCHRmass), FUNCONmass = mean(FUNCONmass), GAMHOLmass = mean(GAMHOLmass), HETFORmass = mean(HETFORmass),
            JORFLOmass = mean(JORFLOmass), LABSICmass = mean(LABSICmass), LEPGULmass = mean(LEPGULmass), LEPMACmass = mean(LEPMACmass),
            LEPMARmass = mean(LEPMARmass), LEPPUNmass = mean(LEPPUNmass), LEPMICmass = mean(LEPMICmass), LEPSPPmass = mean(LEPSPPmass),
            LUCGOOmass = mean(LUCGOOmass), MICSALmass = mean(MICSALmass), NOFISHmass = mean(NOFISHmass), NOTCRYmass = mean(NOTCRYmass),
            OREAURmass = mean(OREAURmass), POELATmass = mean(POELATmass), UNKSPPmass = mean(UNKSPPmass),
            CICURO = mean(CICURO), CLABAT = mean(CLABAT), ELAEVE = mean(ELAEVE), ENNGLO = mean(ENNGLO), ERISUC = mean(ERISUC), 
            ETHFUS = mean(ETHFUS), FUNCHR = mean(FUNCHR), FUNCON = mean(FUNCON), GAMHOL = mean(GAMHOL), HETFOR = mean(HETFOR),
            JORFLO = mean(JORFLO), LABSIC = mean(LABSIC), LEPGUL = mean(LEPGUL), LEPMAC = mean(LEPMAC), LEPMAR = mean(LEPMAR),
            LEPPUN = mean(LEPPUN), LEPMIC = mean(LEPMIC), LEPSPP = mean(LEPSPP), LUCGOO = mean(LUCGOO), MICSAL = mean(MICSAL),
            NOFISH = mean(NOFISH), NOTCRY = mean(NOTCRY), OREAUR = mean(OREAUR), POELAT = mean(POELAT), UNKSPP = mean(UNKSPP))

## Part D:
## Calculate the total catch per wetland by:
## Cumulative, Wateryr, Season, Hydro

sum_macro_catch <- trap_catch %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(Fishmass = sum(Fishmass), Fishcount = sum(Fishcount), CICUROmass = sum(CICUROmass), CLABATmass = sum(CLABATmass),
            ELAEVEmass = sum(ELAEVEmass), ENNGLOmass = sum(ENNGLOmass), ERISUCmass = sum(ERISUCmass), ETHFUSmass = sum(ETHFUSmass),
            FUNCHRmass = sum(FUNCHRmass), FUNCONmass = sum(FUNCONmass), GAMHOLmass = sum(GAMHOLmass), HETFORmass = sum(HETFORmass),
            JORFLOmass = sum(JORFLOmass), LABSICmass = sum(LABSICmass), LEPGULmass = sum(LEPGULmass), LEPMACmass = sum(LEPMACmass),
            LEPMARmass = sum(LEPMARmass), LEPPUNmass = sum(LEPPUNmass), LEPMICmass = sum(LEPMICmass), LEPSPPmass = sum(LEPSPPmass),
            LUCGOOmass = sum(LUCGOOmass), MICSALmass = sum(MICSALmass), NOFISHmass = sum(NOFISHmass), NOTCRYmass = sum(NOTCRYmass),
            OREAURmass = sum(OREAURmass), POELATmass = sum(POELATmass), UNKSPPmass = sum(UNKSPPmass),
            CICURO = sum(CICURO), CLABAT = sum(CLABAT), ELAEVE = sum(ELAEVE), ENNGLO = sum(ENNGLO), ERISUC = sum(ERISUC), 
            ETHFUS = sum(ETHFUS), FUNCHR = sum(FUNCHR), FUNCON = sum(FUNCON), GAMHOL = sum(GAMHOL), HETFOR = sum(HETFOR),
            JORFLO = sum(JORFLO), LABSIC = sum(LABSIC), LEPGUL = sum(LEPGUL), LEPMAC = sum(LEPMAC), LEPMAR = sum(LEPMAR),
            LEPPUN = sum(LEPPUN), LEPMIC = sum(LEPMIC), LEPSPP = sum(LEPSPP), LUCGOO = sum(LUCGOO), MICSAL = sum(MICSAL),
            NOFISH = sum(NOFISH), NOTCRY = sum(NOTCRY), OREAUR = sum(OREAUR), POELAT = sum(POELAT), UNKSPP = sum(UNKSPP))

## Part E:
## Calculate the mean catch per wetland by:
## Cumulative, Wateryr, Season, Hydro

mean_macro_catch <- trap_catch %>%
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(Fishmass = mean(Fishmass), Fishcount = mean(Fishcount), CICUROmass = mean(CICUROmass), CLABATmass = mean(CLABATmass),
            ELAEVEmass = mean(ELAEVEmass), ENNGLOmass = mean(ENNGLOmass), ERISUCmass = mean(ERISUCmass), ETHFUSmass = mean(ETHFUSmass),
            FUNCHRmass = mean(FUNCHRmass), FUNCONmass = mean(FUNCONmass), GAMHOLmass = mean(GAMHOLmass), HETFORmass = mean(HETFORmass),
            JORFLOmass = mean(JORFLOmass), LABSICmass = mean(LABSICmass), LEPGULmass = mean(LEPGULmass), LEPMACmass = mean(LEPMACmass),
            LEPMARmass = mean(LEPMARmass), LEPPUNmass = mean(LEPPUNmass), LEPMICmass = mean(LEPMICmass), LEPSPPmass = mean(LEPSPPmass),
            LUCGOOmass = mean(LUCGOOmass), MICSALmass = mean(MICSALmass), NOFISHmass = mean(NOFISHmass), NOTCRYmass = mean(NOTCRYmass),
            OREAURmass = mean(OREAURmass), POELATmass = mean(POELATmass), UNKSPPmass = mean(UNKSPPmass),
            CICURO = mean(CICURO), CLABAT = mean(CLABAT), ELAEVE = mean(ELAEVE), ENNGLO = mean(ENNGLO), ERISUC = mean(ERISUC), 
            ETHFUS = mean(ETHFUS), FUNCHR = mean(FUNCHR), FUNCON = mean(FUNCON), GAMHOL = mean(GAMHOL), HETFOR = mean(HETFOR),
            JORFLO = mean(JORFLO), LABSIC = mean(LABSIC), LEPGUL = mean(LEPGUL), LEPMAC = mean(LEPMAC), LEPMAR = mean(LEPMAR),
            LEPPUN = mean(LEPPUN), LEPMIC = mean(LEPMIC), LEPSPP = mean(LEPSPP), LUCGOO = mean(LUCGOO), MICSAL = mean(MICSAL),
            NOFISH = mean(NOFISH), NOTCRY = mean(NOTCRY), OREAUR = mean(OREAUR), POELAT = mean(POELAT), UNKSPP = mean(UNKSPP))
## Part F:
## Calculate the mean catch of wetlands for catch only from sloughs
trap_catch_sloughs <- trap_catch[trap_catch$Location %in%  c("DS", "SS"),]
mean_slough_catch <- trap_catch_sloughs %>% 
  group_by(Cumulative, Wateryr, Season, Wetland, Hydro) %>% 
  summarize(Fishmass = mean(Fishmass), Fishcount = mean(Fishcount), CICUROmass = mean(CICUROmass), CLABATmass = mean(CLABATmass),
            ELAEVEmass = mean(ELAEVEmass), ENNGLOmass = mean(ENNGLOmass), ERISUCmass = mean(ERISUCmass), ETHFUSmass = mean(ETHFUSmass),
            FUNCHRmass = mean(FUNCHRmass), FUNCONmass = mean(FUNCONmass), GAMHOLmass = mean(GAMHOLmass), HETFORmass = mean(HETFORmass),
            JORFLOmass = mean(JORFLOmass), LABSICmass = mean(LABSICmass), LEPGULmass = mean(LEPGULmass), LEPMACmass = mean(LEPMACmass),
            LEPMARmass = mean(LEPMARmass), LEPPUNmass = mean(LEPPUNmass), LEPMICmass = mean(LEPMICmass), LEPSPPmass = mean(LEPSPPmass),
            LUCGOOmass = mean(LUCGOOmass), MICSALmass = mean(MICSALmass), NOFISHmass = mean(NOFISHmass), NOTCRYmass = mean(NOTCRYmass),
            OREAURmass = mean(OREAURmass), POELATmass = mean(POELATmass), UNKSPPmass = mean(UNKSPPmass),
            CICURO = mean(CICURO), CLABAT = mean(CLABAT), ELAEVE = mean(ELAEVE), ENNGLO = mean(ENNGLO), ERISUC = mean(ERISUC), 
            ETHFUS = mean(ETHFUS), FUNCHR = mean(FUNCHR), FUNCON = mean(FUNCON), GAMHOL = mean(GAMHOL), HETFOR = mean(HETFOR),
            JORFLO = mean(JORFLO), LABSIC = mean(LABSIC), LEPGUL = mean(LEPGUL), LEPMAC = mean(LEPMAC), LEPMAR = mean(LEPMAR),
            LEPPUN = mean(LEPPUN), LEPMIC = mean(LEPMIC), LEPSPP = mean(LEPSPP), LUCGOO = mean(LUCGOO), MICSAL = mean(MICSAL),
            NOFISH = mean(NOFISH), NOTCRY = mean(NOTCRY), OREAUR = mean(OREAUR), POELAT = mean(POELAT), UNKSPP = mean(UNKSPP))

## Section 4: Data analysis of summary data
## rmANOVA of hydropattern experiment
## Assumptions of rmANOVA: Normality, Sphericity

# Step 1:
# Create an initial models to obtain Auto Correlation Function (ACF) Values

count_model_wateryr_i <- lme(Fishcount ~ Hydro + Wateryr + Hydro*Wateryr,
                             random = ~1|Wetland,
                             data = mean_slough_catch)

count_model_season_i <- lme(Fishcount ~ Hydro + Season + Hydro*Season,
                            random = ~1|Wetland,
                            data = mean_slough_catch)


count_model_time_i <- lme(Fishcount ~ Hydro + Cumulative + Hydro*Cumulative,
                            random = ~1|Wetland,
                            data = mean_slough_catch)

# Step 2:
# Run ACF function to obtain ACF values

# Run ACF for cumulative count model
ACF(count_model_wateryr_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_A_1 <- -0.40406722
ACF_A_2 <-  0.10406045
ACF_A_3 <- -0.18491469
ACF_A_4 <-  0.06750833
ACF_A_5 <- -0.19509570
ACF_A_6 <-  0.24966435
ACF_A_7 <- -0.26628518
ACF_A_8 <-  0.29078610

# Run ACF for season count model
ACF(count_model_season_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_B_1 <-  0.21075515
ACF_B_2 <-  0.05758812
ACF_B_3 <-  0.01415478
ACF_B_4 <- -0.18763732
ACF_B_5 <- -0.30806599
ACF_B_6 <- -0.60862185
ACF_B_7 <- -0.67077578
ACF_B_8 <- -0.21261195

ACF(count_model_time_i)
# Store ACF outputs below, add lines as necessary
# only store values > 0 or < 1
ACF_C_1 <- -0.4187408
ACF_C_2 <-  0.1527788
ACF_C_3 <- -0.2284346
ACF_C_4 <-  0.0770907
ACF_C_5 <- -0.2258347
ACF_C_6 <-  0.2055251
ACF_C_7 <- -0.1841173
ACF_C_8 <-  0.3814383

# Step 3:
# Run rmANOVAs to determine if hydro-pattern treatment has had an effect on small marsh fish

# Part 1: Model.a
# model.a.1 examines cumulative and Hydro effect on crayfish counts
model.a <- lme(Fishcount ~ factor(Hydro)*ordered(Wateryr), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_A_1),
               data = mean_slough_catch, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 169.6311 and Complex AIC = 168.3009

summary(model.a)
anova(model.a)
#                                  numDF denDF   F-value p-value
#(Intercept)                           1    22 157.27923  <.0001
#factor(Hydro)                         1     2   5.09382  0.1524
#ordered(Wateryr)                      3    22  10.54726  <.0001
#factor(Hydro):ordered(Wateryr)        3    22   1.51015  0.2348

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
anova(Levene.model.a) # P = 0.5135


# Test for normal distribution of residuals
shapiro.test(residuals(model.a)) # P = 0.6064
qqnorm(model.a$residuals)
qqline(model.a$residuals)
anova(model.a)


# Part 2: model.b
# model.b.1 examines season and Hydro effect on crayfish counts
model.b <- lme(Fishcount ~ factor(Hydro)*ordered(Season), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_B_1),
               data = mean_slough_catch, method = "REML")

summary(model.b)
anova(model.b)
#                              numDF denDF   F-value p-value
#(Intercept)                       1    26 163.88092  <.0001
#factor(Hydro)                     1     2   4.12511  0.2096
#ordered(Season)                   1    26   0.05636  0.7035
#factor(Hydro):ordered(Season)     1    26   15.2203  0.0002


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
anova(Levene.model.b) # P = 0.2663

# Test for normal distribution of residuals
shapiro.test(residuals(model.b)) # P = 0.9314
qqnorm(model.b$residuals)
qqline(model.b$residuals)
anova(model.b)


# Part 3: model.c
# model.c.1 examines cumulative and Hydro effect on fish counts
model.c <- lme(Fishcount ~ factor(Hydro)*ordered(Cumulative), 
               random = ~1|Wetland, correlation = corAR1(form = ~1|Wetland, value = ACF_C_1),
               data = mean_slough_catch, method = "REML")
# Ran all iterations of ACF values within the model (not included in code)
# two AIC value outputs came from simple (1 ACF value) and complex (2 ACF values) 
# Simple AIC = 169.6311 and Complex AIC = 168.3009

summary(model.c)
anova(model.c)
#                                  numDF denDF   F-value p-value
#(Intercept)                           1    16 170.15381  <.0001
#factor(Hydro)                         1     2   4.83976  0.1588
#ordered(Cumulative)                   8    16   4.53850  0.0049
#factor(Hydro):ordered(Cumulative)     8    16   2.62736  0.0476

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
anova(Levene.model.c) # P = 0.2741 


# Test for normal distribution of residuals
shapiro.test(residuals(model.c)) # P = 0.1139
qqnorm(model.c$residuals)
qqline(model.c$residuals)
anova(model.c)


### Section 5: Exporting Data Frames as .csv files
### This is when you adjust the below code to export your final data frames to more easily support figure construction


### Export mean_slough_catch data for use in Sommer et al. paper.

write.csv(mean_slough_catch, "M:\\LILA\\Stage Contrast Study\\Annual Reports\\2022 Annual Report\\Data\\SMFD_MEAN_SLOUGH_CATCH_18-22.csv", row.names = FALSE)







