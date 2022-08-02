#this code summarized CPUE data from fyke nets and hoop nets set in LILA during the wet-season-stage contrast experiment  
#that started in 2018.  The code creates standardized catch rates of fish > 5 cm SL (mean of hoop +mean of fyke) each night and then averages
#the two nights.  It also creates biomass CPUE for all fish using regressions.

# data management help https://bouchat.github.io/IntroDataMgmt20Jan.html#introduction_to_r 
# data management help: Base R cheat sheet.

install.packages('tidyverse')

library(dplyr)

#loading in raw catch data from fyke nets and hoop nets over multiple macrocosms at LILA and multiple seasons
#raw catch should have 13 variables, entry, cum, date, season, wateryr, Night, Macro, hydropattern, Net, Loc, species, 
#length, setqual

catch<-read.csv("C:/NateDorn/FAU_research/contracts/LILA stage contrast 2018-2021/TrapNet data/Summary codes/CPUE.csv")

#run summary and then check to see if variables are factors or numeric

summary(catch)
is.factor(catch$Macro)
is.factor(catch$Night)
is.factor(catch$Loc)
is.ordered(catch$cum)
is.ordered(catch$Night_cat)

#notice night and location will need to be switched to factors during summary or a new variable will have to be created.



#First create counts for fish and herps
catch$lepgul <- if_else(catch$species == 'Lepgul', 1, 0)
catch$lepmar <- if_else(catch$species == 'Lepmar', 1, 0)
catch$lepmic <- if_else(catch$species == 'Lepmic', 1, 0)  
catch$leppun <- if_else(catch$species == 'Leppun', 1, 0)  
catch$lepmac <- if_else(catch$species == 'Lepmac', 1, 0)  
catch$leppla <- if_else(catch$species == 'Leppla', 1, 0)  
catch$micsal <- if_else(catch$species == 'Micsal', 1, 0)
catch$funchr <- if_else(catch$species == 'Funchr', 1, 0)
catch$erisuc <- if_else(catch$species == 'Erisuc', 1, 0)
catch$cicuro <- if_else(catch$species == 'Cicuro', 1, 0)
catch$ampmea <- if_else(catch$species == 'Ampmea', 1, 0)
catch$sirlac <- if_else(catch$species == 'Sirlac', 1, 0)
catch$allmis <- if_else(catch$species == 'Alligator', 1, 0)
catch$nerspp <- if_else(catch$species == 'snake', 1, 0)

#creating biomass for fish
#regression data for most species came from Loftus and Trexler regressions (regressions for wet mass *0.2)
#biomass is in g dry mass

catch$lepgulbiom <- if_else(catch$species == 'Lepgul', 0.2*(10^(-4.889+3.224*log10(10*catch$length))), 0)
catch$lepmarbiom <- if_else(catch$species == 'Lepmar', 0.2*(10^(-4.8111+3.225*log10(10*catch$length))), 0)
catch$lepmicbiom <- if_else(catch$species == 'Lepmic', 0.2*(10^(-4.876+3.198*log10(10*catch$length))), 0)
catch$leppunbiom <- if_else(catch$species == 'Leppun', 0.2*(10^(-4.807+3.222*log10(10*catch$length))), 0)
catch$lepmacbiom <- if_else(catch$species == 'Lepmac', 0.2*(10^(-5.100+3.325*log10(10*catch$length))), 0)
catch$lepplabiom <- if_else(catch$species == 'Leppla', 0.2*(10^(-6.344+3.408*log10(10*(catch$length*1.25)))), 0)
catch$micsalbiom <- if_else(catch$species == 'Micsal', 0.2*(10^(-5.203+3.244*log10(10*catch$length))), 0)
catch$funchrbiom <- if_else(catch$species == 'Funchr', 0.2*(10^(-4.876+3.131*log10(10*catch$length))), 0)
catch$erisucbiom <- if_else(catch$species == 'Erisuc', 0.2*(10^(-5.236+3.305*log10(10*catch$length))), 0)
catch$cicurobiom <- if_else(catch$species == 'Cicuro', 0.2*(10^(-4.114+2.912*log10(10*catch$length))), 0)

# create a fish biomass column  (not by species) for purposes of summary
catch$fishbiom <- catch$lepgulbiom + catch$lepmarbiom  + catch$lepmacbiom + catch$lepmicbiom + catch$leppunbiom + catch$lepplabiom + catch$micsalbiom + catch$cicurobiom + catch$erisucbiom + catch$funchrbiom

#create a fish presence column (not by species) for purposes of a count in the summary
catch$fishpres <- catch$lepgul + catch$lepmar  + catch$lepmac + catch$lepmic + catch$leppun + catch$leppla + catch$micsal + catch$cicuro + catch$erisuc + catch$funchr

#this code manipulates variables and creates a new file that summarizes species level biomass and counts
# first cum, Night, and Loc were all switched to factors and night and cum were ordered
# summaries of counts and biomass were made 
# by cum, wateryr, season, macro, hydropattern, net, loc, summarizes biomass and counts by trap, night, season
# loc = physical location in the wetland (switched to L1, L2, L3, L4, L5)
# cum = sample season in order starting with wet season 2019 = 1, dry season 2020 = 2, etc.
# hydropattern = 'C' or 'U'; constrained (low) or unconstrained (high summer depths)
# season = 'wet' or 'dry' season (June or February)
# Macro = M1, M2, M3, M4
# night = '1', '2', '3'
# net = 'H' (black hoop) or 'F' (green fyke)

#  change location (Loc) and Night to factors and re-label, 
catch$Loc <- factor(catch$Loc, 
                    levels = c(1,2,3,4,5),
                    labels = c("L1", "L2", "L3", "L4", "L5")) #following order of levels

#change Night to factor and order it
catch$Night <- factor(catch$Night, ordered = TRUE,
                      levels = c(1, 2, 3)) # follow the order of the levels

#check to see if Night is a factor and if it is ordered
is.factor(catch$Night)
is.ordered(catch$Night)

# change cum and wateryr to ordered factors
catch$cum <- factor(catch$cum, 
                    levels = c(1, 2), ordered = TRUE) 
catch$wateryr <- factor(catch$wateryr,
                        levels = c(2019), ordered = TRUE)

#check to see if cum is an ordered factor, then check to see if wateryr is a factor                       
is.ordered(catch$cum)
is.factor(catch$cum)
is.factor(catch$wateryr)

# remove the nets with alligators entering traps
catchfilt <- filter(catch, setqual == 'Y')


#build the summary file
# summarize mass total, by species, and counts at the scale of nets using a pipe operator %>%

catchsum <- catchfilt %>% 
  group_by (cum, wateryr, season, hydropattern, Night, Loc, Macro, Net) %>% 
  summarize (fishmass = sum(fishbiom), fishcount = sum(fishpres), cicuromass = sum(cicurobiom), lepgulmass = sum(lepgulbiom), 
             lepmarmass = sum(lepmarbiom), lepmacmass = sum(lepmacbiom), lepmicmass = sum(lepmicbiom), micsalmass = sum(micsalbiom),
             leppunmass = sum(leppunbiom), lepplamass = sum(lepplabiom), erisucmass = sum(erisucbiom), funchrmass = sum(funchrbiom), 
              cicuro = sum(cicuro), lepgul = sum(lepgul), 
              lepmar = sum(lepmar), lepmac = sum(lepmac), lepmic = sum(lepmic), micsal = sum(micsal),
              leppun = sum(leppun), leppla = sum(leppla), erisuc = sum(erisuc), funchr = sum(funchr)) 

#calculate the mean catch per net type for each given night (by year, season, macrocosm, hydropattern, etc.)

catch_by_type <- catchsum %>% 
  group_by (cum, wateryr, season, hydropattern, Night, Macro, Net) %>% 
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount), cicuromass = mean(cicuromass), lepgulmass = mean(lepgulmass), 
             lepmarmass = mean(lepmarmass), lepmacmass = mean(lepmacmass), lepmicmass = mean(lepmicmass), micsalmass = mean(micsalmass),
             leppunmass = mean(leppunmass), lepplamass = mean(lepplamass), erisucmass = mean(erisucmass), funchrmass = mean(funchrmass), 
             cicuro = mean(cicuro), lepgul = mean(lepgul), 
             lepmar = mean(lepmar), lepmac = mean(lepmac), lepmic = mean(lepmic), micsal = mean(micsal),
             leppun = mean(leppun), leppla = mean(leppla), erisuc = mean(erisuc), funchr = mean(funchr))

# sum the means of the two nets types for each night

catch_by_night <- catch_by_type %>% 
  group_by (cum, wateryr, season, hydropattern, Macro, Night) %>% 
  summarize (fishmass = sum(fishmass), fishcount = sum(fishcount), cicuromass = sum(cicuromass), lepgulmass = sum(lepgulmass), 
             lepmarmass = sum(lepmarmass), lepmacmass = sum(lepmacmass), lepmicmass = sum(lepmicmass), micsalmass = sum(micsalmass),
             leppunmass = sum(leppunmass), lepplamass = sum(lepplamass), erisucmass = sum(erisucmass), funchrmass = sum(funchrmass), 
             cicuro = sum(cicuro), lepgul = sum(lepgul), lepmar = sum(lepmar), lepmac = sum(lepmac), lepmic = sum(lepmic), micsal = sum(micsal),
             leppun = sum(leppun), leppla = sum(leppla), erisuc = sum(erisuc), funchr = sum(funchr))

# average and calculate standard deviation of the standardized catches of the multiple nights each season

catch_standardized <- catch_by_night %>% 
  group_by (cum, wateryr, season, hydropattern, Macro) %>% 
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount), cicuromass = mean(cicuromass), lepgulmass = mean(lepgulmass), 
             lepmarmass = mean(lepmarmass), lepmacmass = mean(lepmacmass), lepmicmass = mean(lepmicmass), micsalmass = mean(micsalmass),
             leppunmass = mean(leppunmass), lepplamass = mean(lepplamass), erisucmass = mean(erisucmass), funchrmass = mean(funchrmass), 
             cicuro = mean(cicuro), lepgul = mean(lepgul), lepmar = mean(lepmar), lepmac = mean(lepmac), lepmic = mean(lepmic), micsal = mean(micsal),
             leppun = mean(leppun), leppla = mean(leppla), erisuc = mean(erisuc), funchr = mean(funchr),
 
###### CAUTION: this part of the code didn't work on August 7th... I did not find the code to calculate  the night-night SDev.             
             sdfishmass = sd(fishmass), sdfishcount = sd(fishcount), sdcicuromass = sd(cicuromass), sdlepgulmass = sd(lepgulmass), 
             sdlepmarmass = sd(lepmarmass), sdlepmacmass = sd(lepmacmass), sdlepmicmass = sd(lepmicmass), sdmicsalmass = sd(micsalmass),
             sdleppunmass = sd(leppunmass), sdlepplamass = sd(lepplamass), sderisucmass = sd(erisucmass), sdfunchrmass = sd(funchrmass), 
             sdcicuro = sd(cicuro), sdlepgul = sd(lepgul), 
             sdlepmar = sd(lepmar), sdlepmac = sd(lepmac), sdlepmic = sd(lepmic), sdmicsal = sd(micsal),
             sdleppun = sd(leppun), sdleppla = sd(leppla), sderisuc = sd(erisuc), sdfunchr = sd(funchr))

# calculating standardized total sunfish mass and sunfish counts
          
catch_standardized$lepsppmass = catch_standardized$lepgulmass + catch_standardized$lepmarmass + catch_standardized$lepmicmass + 
  catch_standardized$lepmacmass + catch_standardized$leppunmass
catch_standardized$lepsppcount = catch_standardized$lepgul + catch_standardized$lepmar + catch_standardized$lepmic + catch_standardized$lepmac + catch_standardized$leppun
