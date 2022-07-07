### This code summarized throw trap data from LILA during the wet-season-stage contrast experiment ###
### that began in the wet season of 2018. ###

## data management help https://bouchat.github.io/IntroDataMgmt20Jan.html#introduction_to_r 
## data management help: Base R cheat sheet.

install.packages('tidyverse')
library(tidyverse)
library(dplyr)


# Load in raw catch data from throw traps over multiple macrocosms at LILA and multiple seasons
# raw catch should have 12 variables: entry, cum, season, wateryr, hydropattern, hab, macro, TT,
# species, length, sex, form

catch <- read_csv("~/JSommer_files/Jeff's Files/Crayfish Research/Throw Trap Data/Fish and Crayfish data/Fish and Crayfish Lengths file.csv")
View(catch)

summary(catch)
is.factor(catch$macro)
is.factor(catch$hab)
is.factor(catch$season)
is.ordered(catch$cum)

#location will need to be switched to factors during summary or a new variable will have to be created.


#First create counts for fish and crayfish
catch$lepgul <- if_else(catch$species == 'Lepgul', 1, 0)
catch$lepmar <- if_else(catch$species == 'Lepmar', 1, 0)
catch$lepmic <- if_else(catch$species == 'Lepmic', 1, 0)
catch$leppun <- if_else(catch$species == 'Leppun', 1, 0)
catch$lepmac <- if_else(catch$species == 'Lepmac', 1, 0)
catch$micsal <- if_else(catch$species == 'Micsal', 1, 0)
catch$funchr <- if_else(catch$species == 'Funchr', 1, 0)
catch$erisuc <- if_else(catch$species == 'Erisuc', 1, 0)
catch$cicuro <- if_else(catch$species == 'Cicuro', 1, 0)
catch$profal <- if_else(catch$species == 'Profal', 1, 0)
catch$lucgoo <- if_else(catch$species == 'Lucgoo', 1, 0)
catch$gamhol <- if_else(catch$species == 'Gamhol', 1, 0)
catch$hetfor <- if_else(catch$species == 'Hetfor', 1, 0)
catch$notcry <- if_else(catch$species == 'Notcry', 1, 0)
catch$clabat <- if_else(catch$species == 'Clabat', 1, 0)
catch$poelat <- if_else(catch$species == 'Poelat', 1, 0)
catch$jorflo <- if_else(catch$species == 'Jorflo', 1, 0)
catch$labsic <- if_else(catch$species == 'Labsic', 1, 0)
catch$ennglo <- if_else(catch$species == 'Ennglo', 1, 0)
catch$lepspp <- if_else(catch$species == 'Lepspp', 1, 0)
catch$ethfus <- if_else(catch$species == 'Ethfus', 1, 0)
catch$elaeve <- if_else(catch$species == 'Elaeve', 1, 0)

  
#notice night and location will need to be switched to factors during summary or a new variable will have to be created.

#creating biomass for fish and crayfish
#regression data for most species came from Loftus and Trex;er (regressions for wet mass *0.2)
#biomass is in g dry mass

catch$lepgulbiom <- if_else(catch$species == 'Lepgul', 0.2*(10^(-4.889+3.224*log10(10*catch$length))), 0)
catch$lepmarbiom <- if_else(catch$species == 'Lepmar', 0.2*(10^(-4.8111+3.225*log10(10*catch$length))), 0)
catch$lepmicbiom <- if_else(catch$species == 'Lepmic', 0.2*(10^(-4.876+3.198*log10(10*catch$length))), 0)
catch$leppunbiom <- if_else(catch$species == 'Leppun', 0.2*(10^(-4.807+3.222*log10(10*catch$length))), 0)
catch$lepmacbiom <- if_else(catch$species == 'Lepmac', 0.2*(10^(-5.100+3.325*log10(10*catch$length))), 0)
catch$micsalbiom <- if_else(catch$species == 'Micsal', 0.2*(10^(-5.203+3.244*log10(10*catch$length))), 0)
catch$funchrbiom <- if_else(catch$species == 'Funchr', 0.2*(10^(-4.876+3.131*log10(10*catch$length))), 0)
catch$erisucbiom <- if_else(catch$species == 'Erisuc', 0.2*(10^(-5.236+3.305*log10(10*catch$length))), 0)
catch$cicurobiom <- if_else(catch$species == 'Cicuro', 0.2*(10^(-4.114+2.912*log10(10*catch$length))), 0)
catch$profalbiom <- if_else(catch$species == 'Profal', 0.2*(10^(-4.6578+3.2274*log10(10*catch$length))), 0)
catch$lucgoobiom <- if_else(catch$species == 'Lucgoo', 0.2*(10^(-2.3608+2.9747*log10(10*catch$length))), 0)
catch$gamholbiom <- if_else(catch$species == 'Gamhol', 0.2*(10^(-3.1538+3.5955*log10(10*catch$length))), 0)
catch$hetforbiom <- if_else(catch$species == 'Hetfor', 0.2*(10^(-2.053+2.7391*log10(10*catch$length))), 0)
catch$notcrybiom <- if_else(catch$species == 'Notcry', (10^(-4.974+3.104*log10(10*catch$length))), 0)
catch$clabatbiom <- if_else(catch$species == 'Clabat', (10^(-4.844+2.92*log10(10*catch$length))), 0)
catch$poelatbiom <- if_else(catch$species == 'Poelat', 0.2*(10^(-2.7355+3.368*log10(10*catch$length))), 0)
catch$jorflobiom <- if_else(catch$species == 'Jorflo', 0.2*(10^(-3.325+3.9214*log10(10*catch$length))), 0)
catch$labsicbiom <- if_else(catch$species == 'Labsic', (10^(-5.29+3.065*log10(10*catch$length))), 0)
catch$ennglobiom <- if_else(catch$species == 'Ennglo', (10^(-4.624+3.113*log10(10*catch$length))), 0)
catch$lepsppbiom <- if_else(catch$species == 'Lepspp', 0.2*(10^(-1.7881+2.8052*log10(10*catch$length))), 0)
catch$ethfusbiom <- if_else(catch$species == 'Ethfus', (10^(-5.686+3.453*log10(10*catch$length))), 0)
catch$elaevebiom <- if_else(catch$species == 'Elaeve', (10^(-4.581+3.031*log10(10*catch$length))), 0)

#create a fish biomass column (not by species) for purposes of summary
catch$fishbiom <- catch$lepgulbiom + catch$lepmarbiom + catch$lepmicbiom + catch$leppunbiom + catch$lepmacbiom + catch$micsalbiom +
  catch$funchrbiom + catch$erisucbiom + catch$cicurobiom + catch$profalbiom + catch$lucgoobiom + catch$gamholbiom +
  catch$hetforbiom + catch$notcrybiom + catch$clabatbiom + catch$poelatbiom + catch$jorflobiom + catch$labsicbiom +
  catch$ennglobiom + catch$lepsppbiom + catch$ethfusbiom + catch$elaevebiom

#create a fish presence column (not by species) for purposes of a count in the summary
catch$fishpres <- catch$lepgul + catch$lepmar + catch$lepmic + catch$leppun + catch$lepmac + catch$micsal + catch$funchr +
  catch$erisuc + catch$cicuro + catch$profal + catch$lucgoo + catch$gamhol + catch$hetfor + catch$notcry + catch$clabat +
  catch$poelat + catch$jorflo + catch$labsic + catch$ennglo + catch$lepspp + catch$ethfus + catch$elaevel


### This code manipulates variables and creates a new file that summarizes species level biomass and counts
## first cum was switched to a factor and ordered
## summaries of counts and biomass were made
## by cum, wateryr, season, macro, hydropattern, hab, summarizes biomass and counts by TT, habitat (hab), macrocosm, season, 
## hab = habitat where a throw trap was deployed [Deep Slough (DS), Shallow Slough (SS), or Central Ridge (CR)]
## cum = sample season in order starting with wet season 2018 = 1, dry season 2018 = 2, ...etc...
## hydropattern = 'C' or 'U'; constrained (low summer depths) or unconstrained (high summer depths)
## season = 'wet' or 'dry' season (July or Jan-Apr)
## macro = M1, M2, M3, M4


# change cum and wateryr to ordered factors
catch$cum <- factor(catch$cum,
                    levels = c(1, 2, 3, 4, 5), ordered = TRUE)

catch$wateryr <- factor(catch$wateryr,
                        levels = c(2018, 2019, 2020), ordered = TRUE)

# check to see if cum is an ordered factor, then check to see if wateryr is an ordered factor
is.ordered(catch$cum)
is.factor(catch$cum)
is.ordered(catch$wateryr)
is.factor(catch$wateryr)

## build the summary file
# summarize mass total, by species, and counts at the trap scale using a pipe operator %>%

sum_catch <-catchfilt %>%
  group_by(cum, wateryr, season, macro, hydropattern, hab) %>%
  summarize(fishmass = sum(fishbiom), fishcount = sum(fishpres), lepgulmass = sum(legulbiom), 
            lepmarmass = sum(lepmarbiom), lepmicmass = sum(lepmicbiom), leppunmass = sum(leppunbiom),
            lepmacmass = sum(lepmacbiom), micsalmass = sum(micsalbiom), funchrmass = sum(funchrbiom),
            erisucmass = sum(erisucbiom), cicuromass = sum(cicurobiom), profalmass = sum(profalbiom),
            lucgoomass = sum(lucgoobiom), gamholmass = sum(gamholbiom), hetformass = sum(hetforbiom),
            notcrymass = sum(notcrybiom), clabatmass = sum(clabatbiom), poelatmass = sum(poelatbiom),
            jorflomass = sum(jorflobiom), labsicmass = sum(labsicbiom), ennglomass = sum(ennglobiom),
            lepsppmass = sum(lepsppbiom), ethfusmass = sum(ethfusbiom), elaevemass = sum(elaevebiom),
            lepgul = sum(lepgul), lepmar = sum(lepmar), lepmic = sum(lepmic), leppun = sum(leppun),
            lepmac = sum(lepmac), micsal = sum(micsal), funchr = sum(funchr), erisuc = sum(erisuc),
            cicuro = sum(cicuro), profal = sum(profal), lucgoo = sum(lucgoo), gamhol = sum(gamhol),
            hetfor = sum(hetfor), notcry = sum(notcry), clabat = sum(clabat), poelat = sum(poelat),
            jorflo = sum(jorflo), labsic = sum(labsic), ennglo = sum(ennglo), lepspp = sum(lepspp),
            ethfus = sum(ethfus), elaeve = sum(elaeve)
            )

# calcualte the mean catch per trap for each throw trap (by year, season, macrocosm, hydropattern, etc.)

catch_by_type <- sum_catch %>%
  group_by (cum, wateryr, season, macro, hydropattern, hab) %>%
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount), lepgulmass = mean(lepgulmass), lepmarmass = mean(lepmarmass),
             lepmicmass = mean(lepmicmass), leppunmass = mean(leppunmass), lepmacmass = mean(lepmacmass), micsalmass = mean(micaslmass),
             funchrmass = mean(funchrmass), erisucmass = mean(erisucmass), cicuromass = mean(cicuromass), profalmass = mean(profalmass),
             lucgoomass = mean(lucgoomass), gamholmass = mean(gamholmass), hetformass = mean(hetformass), notcrymass = mean(notcrymass),
             clabatmass = mean(clabatmass), poelatmass = mean(poelatmass), jorflomass = mean(jorflomass), labsicmass = mean(labsicmass),
             ennglomass = mean(ennglomass), lepsppmass = mean(lepsppmass), ethfusmass = mean(ethfusmass), elaevemass = mean(elaevemass),
             lepgul = mean(lepgul), lepmar = mean(lepmar), lepmic = mean(lepmic), leppun = mean(leppun),
             lepmac = mean(lepmac), micsal = mean(micsal), funchr = mean(funchr), erisuc = mean(erisuc),
             cicuro = mean(cicuro), profal = mean(profal), lucgoo = mean(lucgoo), gamhol = mean(gamhol),
             hetfor = mean(hetfor), notcry = mean(notcry), clabat = mean(clabat), poelat = mean(poelat),
             jorflo = mean(jorflo), labsic = mean(labsic), ennglo = mean(ennglo), lepspp = mean(lepspp),
             ethfus = mean(ethfus), elaeve = mean(elaeve))



