### This code summarized throw trap data from LILA during the wet-season-stage contrast experiment ###
### that began in the wet season of 2018. ###

## data management help https://urldefense.com/v3/__https://bouchat.github.io/IntroDataMgmt20Jan.html*introduction_to_r__;Iw!!FjuHKAHQs5udqho!KplAcfmVaxfmgZi7JQBeSpv1KwYTV3WwZN41DByxf9OmOuEe-o-6GPBYEgzFpLxjqU-BRDeib2BRKEZDpTIJ$  
## data management help: Base R cheat sheet.

library(dplyr)
#dplyr is a library which provides tools for eficiently manipulating datasets 
library(tidyverse)
# tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats


### Section one:
# Load in raw catch data from throw traps over multiple macrocosms at LILA and multiple seasons
# raw catch should have 13 variables: entry, cum, season, wateryr, hydropattern, session, macro, tt, hab,
# species, length, sex, form

catch <- read.csv("~/Documents/FAU/Thesis/Crayfish Research/Throw Trap Data/Data and analyses for publication/Fish and Crayfish counts_v4_2021AR.csv")
View(catch)

summary(catch)
is.factor(catch$macro)
is.factor(catch$hab)
is.factor(catch$season)
is.factor(catch$wateryr)
is.factor(catch$cum)
is.ordered(catch$wateryr)
is.ordered(catch$cum)
levels(catch$species) # shows the fish species caught in throw traps

#cum and wateryr will need to be switched to ordered factors for summary or a new variable will have to be created.

#fish and crayfish species caught in TT: lepgul, lepmar, lepmic, leppun, lepmac, micsal, funchr, erisuc, cicuro, profal, lucgoo, gamhol,
#hetfor, notcry, clabat, poelat, jorflo, labsic, ennglo, lepspp, ethfus, elaeve. noctch = no catch 
#one throw trap over all five sampling sessions had no fish or crayfish in it (2018 Wet season, M1, TT5).
#First create counts for fish and crayfish, using if_else() f(x) from dplyr package
#if-else syntax: if_else(condition, true, falso, missing = NULL)
#in the case of calculating lepgul catch (see below): condition = catch$species == 'lepgul, if true mark with 1, if false mark with 0.

catch$lepgul <- if_else(catch$species == 'Lepgul', 1, 0) #lepgul= lepomis gulosos, warmouth
catch$lepmar <- if_else(catch$species == 'Lepmar', 1, 0) #lepmar = lepomis marginatus, dollar sunfish 
catch$lepmic <- if_else(catch$species == 'Lepmic', 1, 0) #lepmic = lepomis microlophus, redear sunfish
catch$leppun <- if_else(catch$species == 'Leppun', 1, 0) #leppun = lepomis punctatus, spotted sunfish
catch$lepmac <- if_else(catch$species == 'Lepmac', 1, 0) #lepmac= lepomis macrochirus, bluegill sunfish
catch$micsal <- if_else(catch$species == 'Micsal', 1, 0) #micsal = micropterus salmoides = largemouth bass
catch$funchr <- if_else(catch$species == 'Funchr', 1, 0) #funchr = fundulus chrysotus, golden topminnow
catch$erisuc <- if_else(catch$species == 'Erisuc', 1, 0) #erisuc = erimyzon succetta, lake chubsucker
catch$cicuro <- if_else(catch$species == 'Cicuro', 1, 0) #cicuro = cichlasoma uropthalmus, mayan cichlid
catch$profal <- if_else(catch$species == 'Profal', 1, 0) #profal = procambarus fallax, slough crayfish
catch$lucgoo <- if_else(catch$species == 'Lucgoo', 1, 0) #lucgoo = lucania goodei, bluefin killifish
catch$gamhol <- if_else(catch$species == 'Gamhol', 1, 0) #gamhol = gambusia holbrooki, eastern mosquitofish
catch$hetfor <- if_else(catch$species == 'Hetfor', 1, 0) #hetfor = heterandria formosa, least killifish
catch$notcry <- if_else(catch$species == 'Notcry', 1, 0) #notcry = notemigonus crysoleucas, golden shiner
catch$clabat <- if_else(catch$species == 'Clabat', 1, 0) #clabat = clarius batrachus, ealking catfish
catch$poelat <- if_else(catch$species == 'Poelat', 1, 0) #poelat = poecilia latipinna, sailfin molly
catch$jorflo <- if_else(catch$species == 'Jorflo', 1, 0) #jorflo = jordanella floridae, flagfish
catch$labsic <- if_else(catch$species == 'Labsic', 1, 0) #labsic = labidesthes sicculus, brook silverside
catch$ennglo <- if_else(catch$species == 'Ennglo', 1, 0) #ennglo = ennecanthus gloriosus, bluespotted sunfish
catch$lepspp <- if_else(catch$species == 'Lepspp', 1, 0) #lepspp = lepomis species, sunfishes that were unable to be identifeid to species
catch$ethfus <- if_else(catch$species == 'Ethfus', 1, 0) #ethfus = etheostoma fusiforme, swamp darter
catch$elaeve <- if_else(catch$species == 'Elaeve', 1, 0) #elaeve = elassoma evergladei, everglades pygmy sunfish
catch$oreaur <- if_else(catch$species == 'Oreaur', 1, 0) #oreaur = Oreochromis aureus, blue tilapia
catch$noctch <- if_else(catch$species == 'Noctch', 1, 0) #noctch = no catch, throw trap yielded no specimens


#creating biomass for fish and crayfish, by calculating biomass using regressions
#regression data for most species came from Loftus and Trexler
#regressions for wet mass were miultiplied by 0.2 to obtain dry mass
#dry mass regression have no 0.2 multiplier
#biomass is in mg dry mass

catch$lepgulbiom <- if_else(catch$species == 'Lepgul', 0.2*(10^(-4.889+3.224*log10(catch$length)))*1000, 0)
catch$lepmarbiom <- if_else(catch$species == 'Lepmar', (10^(-2.5949+3.3278*log10(catch$length))), 0)
catch$lepmicbiom <- if_else(catch$species == 'Lepmic', (10^(-1.7881+2.8052*log10(catch$length))), 0)
catch$leppunbiom <- if_else(catch$species == 'Leppun', (10^(-1.7881+2.8052*log10(catch$length))), 0)
catch$lepmacbiom <- if_else(catch$species == 'Lepmac', (10^(-1.7881+2.8052*log10(catch$length))), 0)
catch$micsalbiom <- if_else(catch$species == 'Micsal', 0.2*(10^(-5.203+3.244*log10(catch$length)))*1000, 0)
catch$funchrbiom <- if_else(catch$species == 'Funchr', (10^(-2.9311+3.3635*log10(catch$length))), 0)
catch$erisucbiom <- if_else(catch$species == 'Erisuc', 0.2*(10^(-5.236+3.305*log10(catch$length)))*1000, 0)
catch$cicurobiom <- if_else(catch$species == 'Cicuro', 0.2*(10^(-4.114+2.912*log10(catch$length)))*1000, 0)
catch$profalbiom <- if_else(catch$species == 'Profal', (10^(-4.6578+3.2274*log10(catch$length)))*1000, 0)
catch$lucgoobiom <- if_else(catch$species == 'Lucgoo', (10^(-2.3608+2.9747*log10(catch$length))), 0)
catch$gamholbiom <- if_else(catch$species == 'Gamhol', (10^(-3.1538+3.5955*log10(catch$length))), 0)
catch$hetforbiom <- if_else(catch$species == 'Hetfor', (10^(-2.053+2.7391*log10(catch$length))), 0)
catch$notcrybiom <- if_else(catch$species == 'Notcry', 0.2*(10^(-4.974+3.104*log10(catch$length)))*1000, 0)
catch$clabatbiom <- if_else(catch$species == 'Clabat', 0.2*(10^(-4.844+2.92*log10(catch$length)))*1000, 0)
catch$poelatbiom <- if_else(catch$species == 'Poelat', (10^(-2.7355+3.368*log10(catch$length))), 0)
catch$jorflobiom <- if_else(catch$species == 'Jorflo', (10^(-3.325+3.9214*log10(catch$length))), 0)
catch$labsicbiom <- if_else(catch$species == 'Labsic', 0.2*(10^(-5.29+3.065*log10(catch$length)))*1000, 0)
catch$ennglobiom <- if_else(catch$species == 'Ennglo', 0.2*(10^(-4.624+3.113*log10(catch$length)))*1000, 0)
catch$lepsppbiom <- if_else(catch$species == 'Lepspp', (10^(-1.7881+2.8052*log10(catch$length))), 0)
catch$ethfusbiom <- if_else(catch$species == 'Ethfus', 0.2*(10^(-5.686+3.453*log10(catch$length)))*1000, 0)
catch$elaevebiom <- if_else(catch$species == 'Elaeve', 0.2*(10^(-4.581+3.031*log10(catch$length)))*1000, 0)
catch$oreaurbiom <- if_else(catch$species == 'Oreaur', 0.2*(10^(-4.114+2.912*log(catch$length)))*1000,0)
catch$noctchbiom <- if_else(catch$species == 'Noctch', 0, 0)

#create a fish biomass column (not by species) for purposes of summary data
catch$fishbiom <- catch$lepgulbiom + catch$lepmarbiom + catch$lepmicbiom + catch$leppunbiom + catch$lepmacbiom + catch$micsalbiom +
  catch$funchrbiom + catch$erisucbiom + catch$cicurobiom + catch$lucgoobiom + catch$gamholbiom +
  catch$hetforbiom + catch$notcrybiom + catch$clabatbiom + catch$poelatbiom + catch$jorflobiom + catch$labsicbiom +
  catch$ennglobiom + catch$lepsppbiom + catch$ethfusbiom + catch$elaevebiom + catch$oreaurbiom

#create a crayfish biomass column for purposes of summary data
catch$craybiom <- catch$profalbiom 

#Create a centrarchid biomass column for purposes of summary data
catch$centbiom <- catch$lepgulbiom + catch$lepmacbiom + catch$lepmarbiom + catch$lepmicbiom + 
  catch$leppunbiom + catch$lepsppbiom + catch$micsalbiom + catch$ennglobiom

#create a fish presence column (not by species) for purposes of a count in the summary data
catch$fishpres <- catch$lepgul + catch$lepmar + catch$lepmic + catch$leppun + catch$lepmac + catch$micsal +
  catch$funchr + catch$erisuc + catch$cicuro + catch$lucgoo + catch$gamhol +
  catch$hetfor + catch$notcry + catch$clabat + catch$poelat + catch$jorflo + catch$labsic +
  catch$ennglo + catch$lepspp + catch$ethfus + catch$elaeve + catch$oreaur

#create a crayfish presence column (not by species) for purposes of a count in the summary data
catch$craypres <- catch$profal

#create a centrarchid presence column (not by species) for purposes of a count in the summary data
catch$centpres <- catch$lepgul + catch$lepmac + catch$lepmar + catch$lepmic + 
  catch$leppun + catch$lepspp + catch$micsal + catch$ennglo

### Section two:
### This code manipulates variables and creates a new data frame that summarizes species level biomass and counts
## first cum and wateryer were made ordered factors
## summaries of counts and biomass were made
## by cum, wateryr, season, macro, hydropattern, hab, summarizes biomass and counts by TT, habitat (hab), macrocosm, season, 
## hab = habitat where a throw trap was deployed [Deep Slough (DS), Shallow Slough (SS), or Central Ridge (CR)]
## cum = sample season in order starting with wet season 2018 = 1, dry season 2018 = 2, ...etc...
## hydropattern = 'C' or 'U'; constrained (low summer depths) or unconstrained (high summer depths)
## season = 'wet' or 'dry' season (July or Jan-Apr)
## macro = M1, M2, M3, M4
## tt = throwtrap number
## biomass (mass) data is in mg


# change cum and wateryr to ordered factors
catch$cum <- factor(catch$cum,
                    levels = c(1, 2, 3, 4, 5, 6, 7), ordered = TRUE)

catch$wateryr <- factor(catch$wateryr,
                        levels = c(2018, 2019, 2020, 2021), ordered = TRUE)

# check to see if cum is an ordered factor, then check to see if wateryr is an ordered factor
is.factor(catch$cum)
is.ordered(catch$cum)
is.factor(catch$wateryr)
is.ordered(catch$wateryr)

summary(catch)

## build the summary file
# summarize mass total, by species, and counts at the trap scale using a pipe operator %>%
# calcualte the  catch per trap for each throw trap (by cum,  wateryr, season, macrocosm, hydropattern, hab)
trap_catch <- catch %>%
  group_by(cum, wateryr, season, macro, hydropattern, tt, hab,) %>%
  summarize(fishmass = sum(fishbiom), fishcount = sum(fishpres), craymass = sum(craybiom), craycount = sum(craypres), centmass = sum(centbiom), centcount = sum(centpres),lepgulmass = sum(lepgulbiom), 
            lepmarmass = sum(lepmarbiom), lepmicmass = sum(lepmicbiom), leppunmass = sum(leppunbiom),
            lepmacmass = sum(lepmacbiom), micsalmass = sum(micsalbiom), funchrmass = sum(funchrbiom),
            erisucmass = sum(erisucbiom), cicuromass = sum(cicurobiom), profalmass = sum(profalbiom),
            lucgoomass = sum(lucgoobiom), gamholmass = sum(gamholbiom), hetformass = sum(hetforbiom),
            notcrymass = sum(notcrybiom), clabatmass = sum(clabatbiom), poelatmass = sum(poelatbiom),
            jorflomass = sum(jorflobiom), labsicmass = sum(labsicbiom), ennglomass = sum(ennglobiom),
            lepsppmass = sum(lepsppbiom), ethfusmass = sum(ethfusbiom), elaevemass = sum(elaevebiom), oreaurmass = sum(oreaurbiom),
            lepgul = sum(lepgul), lepmar = sum(lepmar), lepmic = sum(lepmic), leppun = sum(leppun),
            lepmac = sum(lepmac), micsal = sum(micsal), funchr = sum(funchr), erisuc = sum(erisuc),
            cicuro = sum(cicuro), profal = sum(profal), lucgoo = sum(lucgoo), gamhol = sum(gamhol),
            hetfor = sum(hetfor), notcry = sum(notcry), clabat = sum(clabat), poelat = sum(poelat),
            jorflo = sum(jorflo), labsic = sum(labsic), ennglo = sum(ennglo), lepspp = sum(lepspp),
            ethfus = sum(ethfus), elaeve = sum(elaeve), oreaur = sum(oreaur)
            )

#Create a smaller version of the trap catch data frame to more easily visually view and inspect the data
#Smaller dataframe only includes: cum, wateryr, season, macro, hydropattern, tt, hab, fishmass, fishcount, craymass, craycount, centmass, and centcount 
trap_catch_small <- trap_catch[,c(1,2,3,4,5,6,7,8,9,10,11,12,13)]

#Check normaility of craycount, craymass, fishcount, and fish mass data in trap_catch
qqnorm(trap_catch$craycount)
qqline(trap_catch$craycount)
shapiro.test(trap_catch$craycount)
shapiro.test(trap_catch$fishcount)
shapiro.test(trap_catch$craymass)
shapiro.test(trap_catch$fishmass)
#craycount is non-normal with high numbers of zeroes
#fishcount is non- normal
#craymass is also non-normal with high numbers of zeroes
#fishmass is non-normal


#calculate the total catch per habitat (by cum, wateryr, season, macrocosm, hydropattern)
#DS catch = sum of 10 TT, SS catch = sum pf 4 TT, and CR cath = sum of 8 throwtraps
sum_hab_catch <- trap_catch %>%
  group_by(cum, wateryr, season, macro, hydropattern, hab,) %>%
  summarize (fishmass = sum(fishmass), fishcount = sum(fishcount), craymass = sum(craymass), craycount = sum(craycount), centmass = sum(centmass), centcount = sum(centcount),lepgulmass = sum(lepgulmass), 
             lepmarmass = sum(lepmarmass), lepmicmass = sum(lepmicmass), leppunmass = sum(leppunmass),
             lepmacmass = sum(lepmacmass), micsalmass = sum(micsalmass), funchrmass = sum(funchrmass),
             erisucmass = sum(erisucmass), cicuromass = sum(cicuromass), profalmass = sum(profalmass),
             lucgoomass = sum(lucgoomass), gamholmass = sum(gamholmass), hetformass = sum(hetformass),
             notcrymass = sum(notcrymass), clabatmass = sum(clabatmass), poelatmass = sum(poelatmass),
             jorflomass = sum(jorflomass), labsicmass = sum(labsicmass), ennglomass = sum(ennglomass),
             lepsppmass = sum(lepsppmass), ethfusmass = sum(ethfusmass), elaevemass = sum(elaevemass), oreaurmass = sum(oreaurmass),
             lepgul = sum(lepgul), lepmar = sum(lepmar), lepmic = sum(lepmic), leppun = sum(leppun),
             lepmac = sum(lepmac), micsal = sum(micsal), funchr = sum(funchr), erisuc = sum(erisuc),
             cicuro = sum(cicuro), profal = sum(profal), lucgoo = sum(lucgoo), gamhol = sum(gamhol),
             hetfor = sum(hetfor), notcry = sum(notcry), clabat = sum(clabat), poelat = sum(poelat),
             jorflo = sum(jorflo), labsic = sum(labsic), ennglo = sum(ennglo), lepspp = sum(lepspp),
             ethfus = sum(ethfus), elaeve = sum(elaeve), oreaur = sum(oreaur)
             )
# calculate the mean catch per habitat (by cum, wateryr, season, macrocosm, hydropattern)
#DS catch = mean of 10 TT, SS catch = mean pf 4 TT, and CR cath = mean of 8 throwtraps
mean_hab_catch <- trap_catch %>%
  group_by (cum, wateryr, season, macro, hydropattern, hab) %>%
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount), craymass = mean(craymass), craycount = mean(craycount), centmass = mean(centmass), centcount = mean(centcount), lepgulmass = mean(lepgulmass), lepmarmass = mean(lepmarmass),
             lepmicmass = mean(lepmicmass), leppunmass = mean(leppunmass), lepmacmass = mean(lepmacmass), micsalmass = mean(micsalmass),
             funchrmass = mean(funchrmass), erisucmass = mean(erisucmass), cicuromass = mean(cicuromass), profalmass = mean(profalmass),
             lucgoomass = mean(lucgoomass), gamholmass = mean(gamholmass), hetformass = mean(hetformass), notcrymass = mean(notcrymass),
             clabatmass = mean(clabatmass), poelatmass = mean(poelatmass), jorflomass = mean(jorflomass), labsicmass = mean(labsicmass),
             ennglomass = mean(ennglomass), lepsppmass = mean(lepsppmass), ethfusmass = mean(ethfusmass), elaevemass = mean(elaevemass), oreaurmass = mean(oreaurmass),
             lepgul = mean(lepgul), lepmar = mean(lepmar), lepmic = mean(lepmic), leppun = mean(leppun),
             lepmac = mean(lepmac), micsal = mean(micsal), funchr = mean(funchr), erisuc = mean(erisuc),
             cicuro = mean(cicuro), profal = mean(profal), lucgoo = mean(lucgoo), gamhol = mean(gamhol),
             hetfor = mean(hetfor), notcry = mean(notcry), clabat = mean(clabat), poelat = mean(poelat),
             jorflo = mean(jorflo), labsic = mean(labsic), ennglo = mean(ennglo), lepspp = mean(lepspp),
             ethfus = mean(ethfus), elaeve = mean(elaeve), oreaur = mean(oreaur)
             )

#reduce mean hab catch data frame to small verison with cum, wateryr, season, macro, mydropattern, hab, fishmass, fishcount, craymass, craycount, centmass, centcount
mean_hab_catch_small <- mean_hab_catch[,c(1,2,3,4,5,6,7,8,9,10,11,12)]


#calculate the total catch per macrocosm by (cum, wateryr, season, and hydropattern)
sum_macro_catch <- trap_catch %>%
  group_by(cum, wateryr, season, macro, hydropattern) %>%
  summarize (fishmass = sum(fishmass), fishcount = sum(fishcount), craymass = sum(craymass), craycount = sum(craycount), centmass = sum(centmass), centcount = sum(centcount), lepgulmass = sum(lepgulmass), 
             lepmarmass = sum(lepmarmass), lepmicmass = sum(lepmicmass), leppunmass = sum(leppunmass),
             lepmacmass = sum(lepmacmass), micsalmass = sum(micsalmass), funchrmass = sum(funchrmass),
             erisucmass = sum(erisucmass), cicuromass = sum(cicuromass), profalmass = sum(profalmass),
             lucgoomass = sum(lucgoomass), gamholmass = sum(gamholmass), hetformass = sum(hetformass),
             notcrymass = sum(notcrymass), clabatmass = sum(clabatmass), poelatmass = sum(poelatmass),
             jorflomass = sum(jorflomass), labsicmass = sum(labsicmass), ennglomass = sum(ennglomass),
             lepsppmass = sum(lepsppmass), ethfusmass = sum(ethfusmass), elaevemass = sum(elaevemass), oreaurmass = sum(oreaurmass),
             lepgul = sum(lepgul), lepmar = sum(lepmar), lepmic = sum(lepmic), leppun = sum(leppun),
             lepmac = sum(lepmac), micsal = sum(micsal), funchr = sum(funchr), erisuc = sum(erisuc),
             cicuro = sum(cicuro), profal = sum(profal), lucgoo = sum(lucgoo), gamhol = sum(gamhol),
             hetfor = sum(hetfor), notcry = sum(notcry), clabat = sum(clabat), poelat = sum(poelat),
             jorflo = sum(jorflo), labsic = sum(labsic), ennglo = sum(ennglo), lepspp = sum(lepspp),
             ethfus = sum(ethfus), elaeve = sum(elaeve), oreaur = sum(oreaur)
             )

#calculate the mean catch per macrocosm (by cum, wateryr, season,and hydropattern)
mean_macro_catch <- trap_catch %>% 
  group_by(cum, wateryr, season, macro, hydropattern) %>%
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount), craymass = mean(craymass), craycount = mean(craycount), centmass = mean(centmass), centcount = mean(centcount),lepgulmass = mean(lepgulmass), lepmarmass = mean(lepmarmass),
             lepmicmass = mean(lepmicmass), leppunmass = mean(leppunmass), lepmacmass = mean(lepmacmass), micsalmass = mean(micsalmass),
             funchrmass = mean(funchrmass), erisucmass = mean(erisucmass), cicuromass = mean(cicuromass), profalmass = mean(profalmass),
             lucgoomass = mean(lucgoomass), gamholmass = mean(gamholmass), hetformass = mean(hetformass), notcrymass = mean(notcrymass),
             clabatmass = mean(clabatmass), poelatmass = mean(poelatmass), jorflomass = mean(jorflomass), labsicmass = mean(labsicmass),
             ennglomass = mean(ennglomass), lepsppmass = mean(lepsppmass), ethfusmass = mean(ethfusmass), elaevemass = mean(elaevemass), oreaurmass = mean(oreaurmass),
             lepgul = mean(lepgul), lepmar = mean(lepmar), lepmic = mean(lepmic), leppun = mean(leppun),
             lepmac = mean(lepmac), micsal = mean(micsal), funchr = mean(funchr), erisuc = mean(erisuc),
             cicuro = mean(cicuro), profal = mean(profal), lucgoo = mean(lucgoo), gamhol = mean(gamhol),
             hetfor = mean(hetfor), notcry = mean(notcry), clabat = mean(clabat), poelat = mean(poelat),
             jorflo = mean(jorflo), labsic = mean(labsic), ennglo = mean(ennglo), lepspp = mean(lepspp),
             ethfus = mean(ethfus), elaeve = mean(elaeve), oreaur = mean(oreaur)
             )
#check normality of mean_macro_catch
qqnorm(mean_macro_catch$craycount)
qqline(mean_macro_catch$craycount)
shapiro.test(mean_macro_catch$craycount)
# Shapiro-Wilk normality test
# data:  mean_macro_catch$craycount
# W = 0.90548, p-value = 0.0523
#log transform craycount to check normality
mean_macro_catch$craycount_trans <- log(mean_macro_catch$craycount)
qqnorm(mean_macro_catch$craycount_trans)
qqline(mean_macro_catch$craycount_trans)
shapiro.test(mean_macro_catch$craycount_trans)
# Shapiro-Wilk normality test
# data:  mean_macro_catch$craycount_trans
# W = 0.97543, p-value = 0.8626

qqnorm(mean_macro_catch$craymass)
qqline(mean_macro_catch$craymass)
shapiro.test(mean_macro_catch$craymass)
# Shapiro-Wilk normality test
# data:  mean_macro_catch$craymass
# W = 0.74098, p-value = 0.0001288

# craymass data in mean_macro_catch is non-normal
# need to log transform and check normality after transforming
mean_macro_catch$craymass_trans <- log(mean_macro_catch$craymass + 0.1)
qqnorm(mean_macro_catch$craymass_trans)
qqline(mean_macro_catch$craymass_trans)
shapiro.test(mean_macro_catch$craymass_trans)
# Shapiro-Wilk normality test
# data:  mean_macro_catch$craymass_trans
# W = 0.96712, p-value = 0.6932

qqnorm(mean_macro_catch$fishcount)
qqline(mean_macro_catch$fishcount)
shapiro.test(mean_macro_catch$fishcount)
# Shapiro-Wilk normality test
# data:  mean_macro_catch$fishcount
# W = 0.95871, p-value = 0.5185

qqnorm(mean_macro_catch$fishmass)
qqline(mean_macro_catch$fishmass)
shapiro.test(mean_macro_catch$fishmass)
# Shapiro-Wilk normality test
# data:  mean_macro_catch$fishmass
# W = 0.88062, p-value = 0.01816

# fishmass data in mean_macro_catch is non-normal 
# need to log transform and check normality after transforming
mean_macro_catch$fishmass_trans <- log(mean_macro_catch$fishmass + 0.1)
qqnorm(mean_macro_catch$fishmass_trans)
qqline(mean_macro_catch$fishmass_trans)
shapiro.test(mean_macro_catch$fishmass_trans)
# Shapiro-Wilk normality test
# data:  mean_macro_catch$fishmass_trans
# W = 0.94304, p-value = 0.2735

#calculate the total catch per hydropattern (by cum, wateryr and season )
sum_hydro_catch <- sum_macro_catch %>%
  group_by(cum, wateryr, season, hydropattern) %>%
  summarize (fishmass = sum(fishmass), fishcount = sum(fishcount), craymass = sum(craymass), craycount = sum(craycount), lepgulmass = sum(lepgulmass), 
             lepmarmass = sum(lepmarmass), lepmicmass = sum(lepmicmass), leppunmass = sum(leppunmass),
             lepmacmass = sum(lepmacmass), micsalmass = sum(micsalmass), funchrmass = sum(funchrmass),
             erisucmass = sum(erisucmass), cicuromass = sum(cicuromass), profalmass = sum(profalmass),
             lucgoomass = sum(lucgoomass), gamholmass = sum(gamholmass), hetformass = sum(hetformass),
             notcrymass = sum(notcrymass), clabatmass = sum(clabatmass), poelatmass = sum(poelatmass),
             jorflomass = sum(jorflomass), labsicmass = sum(labsicmass), ennglomass = sum(ennglomass),
             lepsppmass = sum(lepsppmass), ethfusmass = sum(ethfusmass), elaevemass = sum(elaevemass), oreaurmass = sum(oreaurmass),
             lepgul = sum(lepgul), lepmar = sum(lepmar), lepmic = sum(lepmic), leppun = sum(leppun),
             lepmac = sum(lepmac), micsal = sum(micsal), funchr = sum(funchr), erisuc = sum(erisuc),
             cicuro = sum(cicuro), profal = sum(profal), lucgoo = sum(lucgoo), gamhol = sum(gamhol),
             hetfor = sum(hetfor), notcry = sum(notcry), clabat = sum(clabat), poelat = sum(poelat),
             jorflo = sum(jorflo), labsic = sum(labsic), ennglo = sum(ennglo), lepspp = sum(lepspp),
             ethfus = sum(ethfus), elaeve = sum(elaeve), oreaur = sum(oreaur)
             )
#calculate the mean catch per hydropattern (by cum, wateryr, and season)
mean_hydro_catch <- mean_macro_catch %>% 
  group_by(cum, wateryr, season, hydropattern) %>%
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount), craymass = mean(craymass), craycount = mean(craycount),
             lepgulmass = mean(lepgulmass), lepmarmass = mean(lepmarmass),
             lepmicmass = mean(lepmicmass), leppunmass = mean(leppunmass), lepmacmass = mean(lepmacmass), micsalmass = mean(micsalmass),
             funchrmass = mean(funchrmass), erisucmass = mean(erisucmass), cicuromass = mean(cicuromass), profalmass = mean(profalmass),
             lucgoomass = mean(lucgoomass), gamholmass = mean(gamholmass), hetformass = mean(hetformass), notcrymass = mean(notcrymass),
             clabatmass = mean(clabatmass), poelatmass = mean(poelatmass), jorflomass = mean(jorflomass), labsicmass = mean(labsicmass),
             ennglomass = mean(ennglomass), lepsppmass = mean(lepsppmass), ethfusmass = mean(ethfusmass), elaevemass = mean(elaevemass), oreaurmass = mean(oreaurmass),
             lepgul = mean(lepgul), lepmar = mean(lepmar), lepmic = mean(lepmic), leppun = mean(leppun),
             lepmac = mean(lepmac), micsal = mean(micsal), funchr = mean(funchr), erisuc = mean(erisuc),
             cicuro = mean(cicuro), profal = mean(profal), lucgoo = mean(lucgoo), gamhol = mean(gamhol),
             hetfor = mean(hetfor), notcry = mean(notcry), clabat = mean(clabat), poelat = mean(poelat),
             jorflo = mean(jorflo), labsic = mean(labsic), ennglo = mean(ennglo), lepspp = mean(lepspp),
             ethfus = mean(ethfus), elaeve = mean(elaeve), oreaur = mean(oreaur)
             )
#calculate the standard deviations in catch rates per hydropattern (by cum, wateryr, and season)
sd_hydro_catch <- mean_macro_catch %>%
  group_by(cum, wateryr, season, hydropattern) %>%
  summarize(sdfishmass = sd(fishmass), sdfishcount = sd(fishcount), sdcraymass = sd(craymass), sdcraycount = sd(craycount), sdlepgulmass = sd(lepgulmass),
            sdlepmarmass = sd(lepmarmass), sdlepmicmass = sd(lepmicmass), sdleppunmass = sd(leppunmass), sdlepmacmass = sd(lepmacmass), sdmicsalmass = sd(micsalmass),
            sdfunchrmass = sd(funchrmass), sderisucmass = sd(erisucmass), sdcicuromass = sd(cicuromass), sdprofalmass = sd(profalmass), sdlucgoomass = sd(lucgoomass),
            sdgamholmass = sd(gamholmass), sdhetformass = sd(hetformass), sdnotcrymass = sd(notcrymass), sdclabatmass = sd(clabatmass), sdpoelatmass = sd(poelatmass),
            sdjorflomass = sd(jorflomass), sdlabsicmass = sd(labsicmass), sdennglomass = sd(ennglomass), sdlepsppmass = sd(lepsppmass), sdethfusmass = sd(ethfusmass),
            sdelaevemass = sd(elaevemass), sdoreaurmass = sd(oreaurmass), 
            lepgul = sd(lepgul), lepmar = sd(lepmar), lepmic = sd(lepmic), leppun = sd(leppun),
            lepmac = sd(lepmac), micsal = sd(micsal), funchr = sd(funchr), erisuc = sd(erisuc),
            cicuro = sd(cicuro), profal = sd(profal), lucgoo = sd(lucgoo), gamhol = sd(gamhol),
            hetfor = sd(hetfor), notcry = sd(notcry), clabat = sd(clabat), poelat = sd(poelat),
            jorflo = sd(jorflo), labsic = sd(labsic), ennglo = sd(ennglo), lepspp = sd(lepspp),
            ethfus = sd(ethfus), elaeve = sd(elaeve), oreaur = sd(oreaur)
            )

# calculate mean catch per hydropattern with standard deviations of catch by (cum, wateryr and season)
mean_sd_hydro_catch <- mean_macro_catch %>% 
  group_by(cum, wateryr, season, hydropattern) %>%
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount), craymass = mean(craymass), craycount = mean(craycount) ,lepgulmass = mean(lepgulmass), lepmarmass = mean(lepmarmass),
             lepmicmass = mean(lepmicmass), leppunmass = mean(leppunmass), lepmacmass = mean(lepmacmass), micsalmass = mean(micsalmass),
             funchrmass = mean(funchrmass), erisucmass = mean(erisucmass), cicuromass = mean(cicuromass), profalmass = mean(profalmass),
             lucgoomass = mean(lucgoomass), gamholmass = mean(gamholmass), hetformass = mean(hetformass), notcrymass = mean(notcrymass),
             clabatmass = mean(clabatmass), poelatmass = mean(poelatmass), jorflomass = mean(jorflomass), labsicmass = mean(labsicmass),
             ennglomass = mean(ennglomass), lepsppmass = mean(lepsppmass), ethfusmass = mean(ethfusmass), elaevemass = mean(elaevemass), oreaurmass = mean(oreaurmass),
             lepgul = mean(lepgul), lepmar = mean(lepmar), lepmic = mean(lepmic), leppun = mean(leppun),
             lepmac = mean(lepmac), micsal = mean(micsal), funchr = mean(funchr), erisuc = mean(erisuc),
             cicuro = mean(cicuro), profal = mean(profal), lucgoo = mean(lucgoo), gamhol = mean(gamhol),
             hetfor = mean(hetfor), notcry = mean(notcry), clabat = mean(clabat), poelat = mean(poelat),
             jorflo = mean(jorflo), labsic = mean(labsic), ennglo = mean(ennglo), lepspp = mean(lepspp),
             ethfus = mean(ethfus), elaeve = mean(elaeve), oreaur = mean(oreaur),
             sdfishmass = sd(fishmass), sdfishcount = sd(fishcount), sdcraymass = sd(craymass), sdcraycount = sd(craycount), sdlepgulmass = sd(lepgulmass),
             sdlepmarmass = sd(lepmarmass), sdlepmicmass = sd(lepmicmass), sdleppunmass = sd(leppunmass), sdlepmacmass = sd(lepmacmass), sdmicsalmass = sd(micsalmass),
             sdfunchrmass = sd(funchrmass), sderisucmass = sd(erisucmass), sdcicuromass = sd(cicuromass), sdprofalmass = sd(profalmass), sdlucgoomass = sd(lucgoomass),
             sdgamholmass = sd(gamholmass), sdhetformass = sd(hetformass), sdnotcrymass = sd(notcrymass), sdclabatmass = sd(clabatmass), sdpoelatmass = sd(poelatmass),
             sdjorflomass = sd(jorflomass), sdlabsicmass = sd(labsicmass), sdennglomass = sd(ennglomass), sdlepsppmass = sd(lepsppmass), sdethfusmass = sd(ethfusmass),
             sdelaevemass = sd(elaevemass), sdoreaurmass = sd(oreaurmass),
             sdlepgul = sd(lepgul), sdlepmar = sd(lepmar), sdlepmic = sd(lepmic), sdleppun = sd(leppun),
             sdlepmac = sd(lepmac), sdmicsal = sd(micsal), sdfunchr = sd(funchr), sderisuc = sd(erisuc),
             sdcicuro = sd(cicuro), sdprofal = sd(profal), sdlucgoo = sd(lucgoo), sdgamhol = sd(gamhol),
             sdhetfor = sd(hetfor), sdnotcry = sd(notcry), sdclabat = sd(clabat), sdpoelat = sd(poelat),
             sdjorflo = sd(jorflo), sdlabsic = sd(labsic), sdennglo = sd(ennglo), sdlepspp = sd(lepspp),
             sdethfus = sd(ethfus), sdelaeve = sd(elaeve), sdoreaur = sd(oreaur)
             )
## Section three: Data analysis of summary data
## rmANOVA of hydropattern experiment
##Assumptions of rmANOVA:Normality, Sphericity

###code out linear models for rmANOVA and to run levene's test
library(nlme)
library(car)

# create an initial model to obtain Auto Correlation Function (ACF) values
model.a1_cum <- lme(craycount ~ hydropattern + cum + hydropattern*cum,
                   random = ~1|macro,
                   data = mean_macro_catch)

model.a1.1_cum <- lme(craycount_trans ~ hydropattern + cum + hydropattern*cum,
                       random = ~1|macro,
                       data = mean_macro_catch)

model.a1_season <-lme( craycount ~ hydropattern + season + hydropattern*season,
                   random = ~1|macro,
                   data = mean_macro_catch)

model.a2_cum <-lme(craymass_trans ~ hydropattern + cum + hydropattern*cum,
                   random = ~1|macro,
                   data = mean_macro_catch)

model.a2_season <- lme(craymass_trans ~ hydropattern + season + hydropattern*season,
                       random = ~1|macro,
                       data = mean_macro_catch)

model.a3_cum <- lme(fishcount ~ hydropattern + cum + hydropattern*cum,
                    random = ~1|macro,
                    data = mean_macro_catch)

model.a3_season <- lme(fishcount ~ hydropattern + season + hydropattern*season,
                       random = ~1|macro,
                       data = mean_macro_catch)

model.a4_cum <- lme(fishmass_trans ~ hydropattern + cum + hydropattern*cum,
                    random = ~1|macro,
                    data = mean_macro_catch)

model.a4_season <- lme(fishmass_trans ~ hydropattern + season + hydropattern*season,
                       random = ~1|macro,
                       data = mean_macro_catch)

ACF(model.a1_cum)
ACF.a1.2 <- -0.27263481

ACF(model.a1.1_cum)
ACF1.1 <- -0.62210569
ACF1.2 <-  0.22746452
ACF1.3 <-  0.04595686
ACF1.4 <- -0.14533886

ACF(model.a1_season)
#   lag         ACF
# 1   0  1.00000000
# 2   1  0.08983675
# 3   2  0.03954358
# 4   3 -0.06530278
# 5   4 -1.83012370
ACF.a.1.7  <- 0.08983675
ACF.a.1.8  <- 0.03954358
ACF.a.1.9  <- -0.06530278 

ACF(model.a2_cum)
ACF.a.2.1 <- -0.6833224


ACF(model.a2_season)
#   lag         ACF
# 1   0  1.00000000
# 2   1  0.16773547
# 3   2 -0.05399526
# 4   3 -0.17807622
# 5   4 -1.79824080
ACF.a.2.7  <- 0.16773547
ACF.a.2.8  <- -0.05399526
ACF.a.2.9  <- -0.17807622

ACF(model.a3_cum)
ACF.a.3.2 <- -0.4508421


ACF(model.a3_season)
#   lag         ACF
# 1   0  1.00000000
# 2   1 -0.04987705
# 3   2 -0.34453862
# 4   3  0.10218726
# 5   4 -1.01768417
ACF.a.3.7  <- -0.04987705
ACF.a.3.8  <- -0.34453862
ACF.a.3.9  <- 0.10218726


ACF(model.a4_cum)
ACF.a.4.1 <-  -0.6795873

ACF(model.a4_season)
#   lag        ACF
# 1   0  1.0000000
# 2   1 -0.3256976
# 3   2 -0.4521345
# 4   3  0.7457244
# 5   4 -0.4272591
ACF.a.4.7  <- -0.3256976
ACF.a.4.8  <- -0.4521345
ACF.a.4.9  <- 0.7457244
ACF.a.4.10 <- -0.4272591


## conduct rmANOVA on effect of hydro-pattern treatment and cumulative sampling session (cum) on crayfish density
#convert cum in mean_macro_catch from factor to numeric for rmANOVA
mean_macro_catch$cum <- as.numeric(mean_macro_catch$cum)
is.numeric(mean_macro_catch$cum)

model.b1 <- lme(craycount ~ factor(hydropattern)*ordered(cum), 
                  random = ~1|macro, correlation = corAR1(form = ~cum|macro, value = ACF.a1.2),
                  data = mean_macro_catch, method = "REML")

summary(model.b1)
anova(model.b1)

# check for violations of assumptions
# test for normal distribution of residuals
shapiro.test(residuals(model.b1))

#plot quantiles with an expected normal distribution qqline
qqnorm(model.b1$residuals)
qqline(model.b1$residuals)
anova(model.b1)

#conduct rmANOVA on log transformed craycount data
model.b2.1 <- lme(craycount_trans ~ factor(hydropattern)*ordered(cum), 
                  random = ~1|macro, correlation = corAR1(form = ~cum|macro, value = ACF1.1),
                  data = mean_macro_catch, method = "REML")


summary(model.b2.1)
anova(model.b2.1)
# check for violations of assumptions
# test for normal distribution of residuals
shapiro.test(residuals(model.b2.1))

#plot quantiles with an expected normal distribution qqline
qqnorm(model.b2.1$residuals)
qqline(model.b2.1$residuals)
anova(model.b2.1)


## conduct rmANOVA on crayfish density by sampling season
#convert season in mean_macro_catch from factor to numeric for rmANOVA

#create models for running anova 
model.b6 <- lme(craycount ~ factor(hydropattern)*ordered(season), 
             random = ~1|macro,
             data = mean_macro_catch, method = "REML")
summary(model.b6)
anova(model.b6)

model.b7 <- lme(craycount ~ factor(hydropattern)*ordered(season), 
                random = ~1|macro, correlation = corAR1(form = ~season|macro, value = ACF.a.1.7, ACF.a.1.8),
                data = mean_macro_catch, method = "REML")

model.b8 <- lme(craycount ~ factor(hydropattern)*ordered(season), 
                random = ~1|macro, correlation = corAR1(form = ~season|macro, value = ACF.a.1.8),
                data = mean_macro_catch, method = "REML")

model.b9 <- lme(craycount ~ factor(hydropattern)*ordered(season), 
                random = ~1|macro, correlation = corAR1(form = ~season|macro, value = ACF.a.1.9),
                data = mean_macro_catch, method = "REML")
summary(model.b7)
summary(model.b8)
summary(model.b9)
####

## conduct rmANOVA on the effect of hydropattern and cumulative sampling session (cum) on crayfish biomass (log transformed)
model.c1 <- lme(craymass_trans ~ factor(hydropattern)*ordered(cum), 
                random = ~1|macro, correlation = corAR1(form = ~cum|macro, value = ACF.a.2.1),
                data = mean_macro_catch, method = "REML")

summary(model.c1)
anova(model.c1)

# check for violations of assumptions
# test for normal distribution of residuals
shapiro.test(residuals(model.c1))

#plot quantiles with an expected normal distribution qqline
qqnorm(model.c1$residuals)
qqline(model.c1$residuals)
anova(model.c1)

## conduct rmANOVA on effect of Hydropattern treatment and cumulative sampling session (cum) on fish density
model.d1 <- lme(fishcount ~ factor(hydropattern)*ordered(cum),
                random = ~1|macro, correlation = corAR1(form = ~cum|macro, value = ACF.a.3.2),
                data = mean_macro_catch, method = "REML")

summary(model.d1)
anova(model.d1)

# check for violations of assumptions
# test for normal distribution of residuals
shapiro.test(residuals(model.d1))

#plot quantiles with an expected normal distribution qqline
qqnorm(model.d1$residuals)
qqline(model.d1$residuals)
anova(model.d1)
## conduct rmANOVA on the effect of hydropattern and cumulative sampling session (cum) on crayfish biomass (log transformed)

model.e1 <- lme(fishmass_trans ~ factor(hydropattern)*ordered(cum), 
               random = ~1|macro, correlation = corAR1(form = ~cum|macro, value = ACF.a.4.1),
               data = mean_macro_catch, method = "REML")

summary(model.e1)
anova(model.e1)

# check for violations of assumptions
# test for normal distribution of residuals
shapiro.test(residuals(model.e1))

#plot quantiles with an expected normal distribution qqline
qqnorm(model.e1$residuals)
qqline(model.e1$residuals)
anova(model.e1)



### Section 4 Data Analysis: Looking at specifics beyond summary data ###
### This is data analysis in addition to 2020 annual report data analysis
### Will need to combine 2020 data analysis code into this working document

#open libraries to run rmANOVA
library(nlme)
library(car)

#### subsection A: Analyze mean crayfish density in sloughs across all 6 sampling sessions (cum = 1:6) by season and treatment ####
#make data frame of mean habitat catch including only slough habiat at macrocosm scale (subset out ridge habitat)
trap_catch_small_only.sloughs <- trap_catch_small[trap_catch_small$hab %in% c("DS", "SS"),]
mean_slough_catch <- trap_catch_small_only.sloughs %>%
  group_by (wateryr, cum,season, macro, hydropattern) %>%
  summarize (fishmass = mean(fishmass), fishcount = mean(fishcount),
             craymass = mean(craymass), craycount = mean(craycount))

# make cum in mean_slough_catch numeric
is.factor(mean_slough_catch$cum)
mean_slough_catch$cum <- as.numeric(mean_slough_catch$cum)
is.numeric(mean_slough_catch$cum)



#run rmANOVA analyzing crayfish densities for an effect of treatment, season, and treatment*season


Pf_treatmentseason.model <- lme(craycount ~ hydropattern + season + hydropattern*season,
                                random = ~1|macro,
                                data = mean_slough_catch)

# calcualte autocorrelation function values
ACF(Pf_treatmentseason.model)
ACF.Pf_ts <- -0.056661904


model.PF_ts <- lme(craycount ~ factor(hydropattern)*factor(season),
                   random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.Pf_ts),
                   data = mean_slough_catch, method = "REML")
model.PF_ts
summary(model.PF_ts)
shapiro.test(residuals(model.PF_ts))
qqnorm(model.PF_ts$residuals)
qqline(model.PF_ts$residuals)
anova(model.PF_ts)

#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, and treatment*wateryr
#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, treatment*wateryr, and season
#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, treatment*wateryr, season, and treatment*season
#create SFWMD_wateryr column
levels(mean_slough_catch$wateryr)
mean_slough_catch$SFWMD_wateryr <- factor(mean_slough_catch$wateryr)
levels(mean_slough_catch$SFWMD_wateryr)[3] <-2021
levels(mean_slough_catch$SFWMD_wateryr)[2] <-2020
levels(mean_slough_catch$SFWMD_wateryr)[1] <-2019
summary(mean_slough_catch)

MCD_treatmentwateryr.model <- lme(craycount ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr,
                                 random = ~1|macro,
                                 data = mean_slough_catch)

MCD_treatmentwateryrseason.model <- lme(craycount ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr + season,
                                       random = ~1|macro,
                                       data = mean_slough_catch)

MCD_treatmentwateryrtreatmentseason.model <- lme(craycount ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr + season + season*hydropattern,
                                                random = ~1|macro,
                                                data = mean_slough_catch)

#calculate ACF values for each of the above models
ACF(MCD_treatmentwateryr.model)
ACF.MCD_tw <- -0.45733098

ACF(MCD_treatmentwateryrseason.model)
ACF.MCD_tws <--0.4052787
  
ACF(MCD_treatmentwateryrtreatmentseason.model)
ACF.MCD_twts<- -0.36549074

model.MCD_tw <- lme(craycount ~ factor(hydropattern)*factor(SFWMD_wateryr),
                   random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.MCD_tw),
                   data = mean_slough_catch, method = "REML")

model.MCD_tws <- lme(craycount ~ factor(hydropattern)*factor(SFWMD_wateryr) + factor(season),
                    random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.MCD_tws),
                    data = mean_slough_catch, method = "REML")

model.MCD_twts <- lme(craycount ~ factor(hydropattern)*factor(SFWMD_wateryr) + factor(season)*factor(hydropattern),
                     random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.MCD_twts),
                     data = mean_slough_catch, method = "REML")

model.MCD_tw
summary(model.MCD_tw)
#assumption testing
#assumption 1: Linearity
Plot.model.MCD_tw <- plot(resid(model.MCD_tw))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$res <-residuals(model.MCD_tw) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absres <- abs(mean_slough_catch$res) #creates new column with absolute value of residuals
mean_slough_catch$res2 <- mean_slough_catch$absres^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.MCD_tw <- lm(res2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.MCD_tw)

#assumption 3: residuals are normally distributed
shapiro.test(residuals(model.MCD_tw))
qqnorm(model.MCD_tw$residuals)
qqline(model.MCD_tw$residuals)
anova(model.MCD_tw)




model.MCD_tws
summary(model.MCD_tws)
#assumption testing
#assumption 1: Linearity
Plot.model.MCD_tws <- plot(resid(model.MCD_tws))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$restws <-residuals(model.MCD_tws) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absrestws <- abs(mean_slough_catch$restws) #creates new column with absolute value of residuals
mean_slough_catch$restws2 <- mean_slough_catch$absrestws^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.MCD_tws <- lm(restws2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.MCD_tws)

#assumption 3: residuals are normally distributed
shapiro.test(residuals(model.MCD_tws))
qqnorm(model.MCD_tws$residuals)
qqline(model.MCD_tws$residuals)
anova(model.MCD_tws)


model.MCD_twts
summary(model.MCD_twts)

#assumption testing
#assumption 1: Linearity
Plot.model.MCD_twts <- plot(resid(model.MCD_twts))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$restwts <-residuals(model.MCD_twts) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absrestwts <- abs(mean_slough_catch$restwts) #creates new column with absolute value of residuals
mean_slough_catch$restwts2 <- mean_slough_catch$absrestwts^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.MCD_twts <- lm(restwts2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.MCD_twts)

#assumption 3: residuals are normally distributed
shapiro.test(residuals(model.MCD_twts))
qqnorm(model.MCD_twts$residuals)
qqline(model.MCD_twts$residuals)
anova(model.MCD_twts)


summary(model.MCD_tw)
summary(model.MCD_tws)
summary(model.MCD_twts)

#### subsection C: Analyze mean small fish density in sloughs across all 6 sampling sessions (cum = 1:6) by season and treatment ####
#run rmANOVA analyzing small fish densities for an effect of treatment, season, and treatment*season


sf_treatmentseason.model <- lme(fishcount ~ hydropattern + season + hydropattern*season,
                                random = ~1|macro,
                                data = mean_slough_catch)

# calcualte autocorrelation function values
ACF(sf_treatmentseason.model)
ACF.sf_ts <- -0.1915645


model.sf_ts <- lme(fishcount ~ factor(hydropattern)*factor(season),
                   random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.sf_ts),
                   data = mean_slough_catch, method = "REML")
model.sf_ts
summary(model.sf_ts)
shapiro.test(residuals(model.sf_ts))
qqnorm(model.sf_ts$residuals)
qqline(model.sf_ts$residuals)
anova(model.sf_ts)

#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, and treatment*wateryr
#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, treatment*wateryr, and season
#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, treatment*wateryr, season, and treatment*season
#create SFWMD_wateryr column

MSFD_treatmentwateryr.model <- lme(fishcount ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr,
                                  random = ~1|macro,
                                  data = mean_slough_catch)

MSFD_treatmentwateryrseason.model <- lme(fishcount ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr + season,
                                        random = ~1|macro,
                                        data = mean_slough_catch)

MSFD_treatmentwateryrtreatmentseason.model <- lme(fishcount ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr + season + season*hydropattern,
                                                 random = ~1|macro,
                                                 data = mean_slough_catch)

#calculate ACF values for each of the above models
ACF(MSFD_treatmentwateryr.model)
ACF.MSFD_tw <- -0.5067217

ACF(MSFD_treatmentwateryrseason.model)
ACF.MSFD_tws <- -0.4903188

ACF(MSFD_treatmentwateryrtreatmentseason.model)
ACF.MSFD_twts<- -0.1706126

model.MSFD_tw <- lme(fishcount ~ factor(hydropattern)*factor(SFWMD_wateryr),
                    random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.MSFD_tw),
                    data = mean_slough_catch, method = "REML")

model.MSFD_tws <- lme(fishcount ~ factor(hydropattern)*factor(SFWMD_wateryr) + factor(season),
                     random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.MSFD_tws),
                     data = mean_slough_catch, method = "REML")

model.MSFD_twts <- lme(fishcount ~ factor(hydropattern)*factor(SFWMD_wateryr) + factor(season)*factor(hydropattern),
                      random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.MSFD_twts),
                      data = mean_slough_catch, method = "REML")

model.MSFD_tw
summary(model.MSFD_tw)

#assumption 1: Linearity
Plot.model.MSFD_tw <- plot(resid(model.MSFD_tw))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$resa <-residuals(model.MSFD_tw) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresa <- abs(mean_slough_catch$resa) #creates new column with absolute value of residuals
mean_slough_catch$resa2 <- mean_slough_catch$absresa^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.MSFD_tw <- lm(resa2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.MSFD_tw)

#assumption 3: resoduals are normally distributed
shapiro.test(residuals(model.MSFD_tw))
qqnorm(model.MSFD_tw$residuals)
qqline(model.MSFD_tw$residuals)
anova(model.MSFD_tw)

model.MSFD_tws
summary(model.MSFD_tws)
#assumption 1: Linearity
Plot.model.MSFD_tws <- plot(resid(model.MSFD_tws))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$resb <-residuals(model.MSFD_tws) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresb <- abs(mean_slough_catch$resb) #creates new column with absolute value of residuals
mean_slough_catch$resb2 <- mean_slough_catch$absresb^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.MSFD_tws <- lm(resb2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.MSFD_tws)

#assumption 3: resoduals are normally distributed
shapiro.test(residuals(model.MSFD_tws))
qqnorm(model.MSFD_tws$residuals)
qqline(model.MSFD_tws$residuals)
anova(model.MSFD_tws)

model.MSFD_twts
summary(model.MSFD_twts)
#assumption 1: Linearity
Plot.model.MSFD_twts <- plot(resid(model.MSFD_twts))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$resc <-residuals(model.MSFD_twts) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresc <- abs(mean_slough_catch$resc) #creates new column with absolute value of residuals
mean_slough_catch$resc2 <- mean_slough_catch$absresc^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.MSFD_twts <- lm(resc2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.MSFD_twts)

#assumption 3: resoduals are normally distributed
shapiro.test(residuals(model.MSFD_twts))
qqnorm(model.MSFD_twts$residuals)
qqline(model.MSFD_twts$residuals)
anova(model.MSFD_twts)


summary(model.MSFD_tw)
summary(model.MSFD_tws)
summary(model.MSFD_twts)

#break apart interaction term of season*treatment which came back as significant p-value = 0.002

em.MSFD_twts <- emmeans(model.MSFD_twts, ~ hydropattern * season)
em.MSFD_twts.season <- contrast(em.MSFD_twts, "pairwise", by = "season", adjust = "none")
em.MSFD_twts.hydropattern <- contrast(em.MSFD_twts, "pairwise", by = "hydropattern", adjust = "none")

em.MSFD_twts.season
em.MSFD_twts.hydropattern

emmip(model.MSFD_twts, season ~ hydropattern)
emmip(model.MSFD_twts, hydropattern ~ season)

#### Subsection E:  Analyze mean macrophyte stem density in sloughs across all 6 sampling sessions (cum = 1:6) by season and treatment #### 
TTcountsandveg <- read.csv("~/Documents/FAU/Thesis/Crayfish Research/Throw Trap Data/TTcountsandveg.csv", header=TRUE)
View(TTcountsandveg)
stem_den <- TTcountsandveg[,c(1:7,10,11,13,14,26:44)]
View(stem_den)

summary(stem_den)
#change water year from a contiuous variable (numeric) to a categorical variable (factor)
stem_den$wateryr <- factor(stem_den$wateryr)
stem_den$SFWMD_wateryr <- factor(stem_den$SFWMD_wateryr)
summary(stem_den)


mean_hab_stem_den <- stem_den %>%
  group_by(Session, wateryr, SFWMD_wateryr, cum, season, macro, hydropattern, Location) %>%
  summarize(stem_den = mean(total.stems))

mean_hab_stem_den_sloughs <- mean_hab_stem_den[mean_hab_stem_den$Location %in% c("DS", "SS"),] 
View(mean_hab_stem_den_sloughs)

mean_slough_stems <- mean_hab_stem_den_sloughs %>%
  group_by(cum, Session, wateryr, SFWMD_wateryr,season, macro, hydropattern) %>%
  summarize(stem_den = mean(stem_den))
View(mean_slough_stems)
summary(mean_slough_stems)
#run rmANOVA analyzing stem densities for an effect of treatment, season, and treatment*season


sd_treatmentseason.model <- lme(stem_den ~ hydropattern + season + hydropattern*season,
                                random = ~1|macro,
                                data = mean_slough_stems)

# calcualte autocorrelation function values
ACF(sd_treatmentseason.model)
ACF.sd_ts <- -0.10716022


model.sd_ts <- lme(stem_den ~ factor(hydropattern)*factor(season),
                   random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.sd_ts),
                   data = mean_slough_stems, method = "REML")
model.sd_ts
summary(model.sd_ts)
shapiro.test(residuals(model.sd_ts))
qqnorm(model.sd_ts$residuals)
qqline(model.sd_ts$residuals)
anova(model.sd_ts)


#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, and treatment*wateryr
#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, treatment*wateryr, and season
#run rmANOVA analyzing stem densities for an effect of treatment, wateryer, treatment*wateryr, season, and treatment*season
sd_treatmentwateryr.model <- lme(stem_den ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr,
                                 random = ~1|macro,
                                 data = mean_slough_stems)

sd_treatmentwateryrseason.model <- lme(stem_den ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr + season,
                                random = ~1|macro,
                                data = mean_slough_stems)

sd_treatmentwateryrtreatmentseason.model <- lme(stem_den ~ hydropattern + SFWMD_wateryr + hydropattern*SFWMD_wateryr + season + hydropattern + season*hydropattern,
                                                random = ~1|macro,
                                                data = mean_slough_stems)

# calcualte autocorrelation function values
ACF(sd_treatmentwateryr.model)
ACF.sd_tw <- -0.55335171

ACF(sd_treatmentwateryrseason.model)
ACF.sd_tws <- -0.5427517

ACF(sd_treatmentwateryrtreatmentseason.model)
ACF.sd_twts <- -0.35561653

model.sd_tw <- lme(stem_den ~ factor(hydropattern)*factor(SFWMD_wateryr),
                   random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.sd_tw),
                   data = mean_slough_stems, method = "REML")

model.sd_tws <- lme(stem_den ~ factor(hydropattern)*factor(SFWMD_wateryr) + factor(season),
                   random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.sd_tws),
                   data = mean_slough_stems, method = "REML")

model.sd_twts <- lme(stem_den ~ factor(hydropattern)*factor(SFWMD_wateryr) + factor(season)*factor(hydropattern),
                    random = ~1|macro, correlation = corAR1(form = ~ 1|macro, value = ACF.sd_twts),
                    data = mean_slough_stems, method = "REML")

model.sd_tw
summary(model.sd_tw)

#assumption 1: Linearity
Plot.model.sd_tw <- plot(resid(model.sd_tw))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$resd <-residuals(model.sd_tw) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresd <- abs(mean_slough_catch$resd) #creates new column with absolute value of residuals
mean_slough_catch$resd2 <- mean_slough_catch$absresd^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.sd_tw <- lm(resd2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.sd_tw)

#assumption 3: resoduals are normally distributed
shapiro.test(residuals(model.sd_tw))
qqnorm(model.sd_tw$residuals)
qqline(model.sd_tw$residuals)
anova(model.sd_tw) #see results of anova below

# anova(model.sd_tw): results
# numDF denDF   F-value p-value
# (Intercept)                                    1    16 236.02738  <.0001
# factor(hydropattern)                           1     2  11.06335  0.0797
# factor(SFWMD_wateryr)                          2    16  15.66495  0.0002
# factor(hydropattern):factor(SFWMD_wateryr)     2    16   3.99292  0.0392

model.sd_tws
summary(model.sd_tws)

#assumption 1: Linearity
Plot.model.sd_tws <- plot(resid(model.sd_tws))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$rese <-residuals(model.sd_tws) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absrese <- abs(mean_slough_catch$rese) #creates new column with absolute value of residuals
mean_slough_catch$rese2 <- mean_slough_catch$absrese^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.sd_tws <- lm(rese2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.sd_tws)

#assumption 3: resoduals are normally distributed
shapiro.test(residuals(model.sd_tws))
qqnorm(model.sd_tws$residuals)
qqline(model.sd_tws$residuals)
anova(model.sd_tws)

model.sd_twts
summary(model.sd_twts)

#assumption 1: Linearity
Plot.model.sd_twts <- plot(resid(model.sd_twts))

#assumption 2: Homogeneity of variance
#build data fram to run levene's test on the model
mean_slough_catch$resf <-residuals(model.sd_twts) #extracts residual and places them in a new column in dataframe
mean_slough_catch$absresf <- abs(mean_slough_catch$resf) #creates new column with absolute value of residuals
mean_slough_catch$resf2 <- mean_slough_catch$absresf^2 #squares ths absolute value of residualsto provide more robust estimate
Levene.model.sd_twts <- lm(resf2 ~ macro, data = mean_slough_catch) #anova of the squared residuals
anova(Levene.model.sd_twts)

#assumption 3: resoduals are normally distributed
shapiro.test(residuals(model.sd_twts))
qqnorm(model.sd_twts$residuals)
qqline(model.sd_twts$residuals)
anova(model.sd_twts)

summary(model.sd_tw)
summary(model.sd_tws)
summary(model.sd_twts)

###Interaction term of hydropattern and SFWMD_wateryr came back as significant in model.sd_tw
###The interaction term needs to be broken apart using the emmeans package
library(emmeans)
library(ggplot2)

em.msd_tw <- emmeans(model.sd_tw, ~ hydropattern * SFWMD_wateryr)
em.msd_tw.SFWMD_wateryr <- contrast(em.msd, "pairwise", by = "SFWMD_wateryr", adjust = "none")
em.msd_tw.hydropattern <- contrast(em.msd, "pairwise", by = "hydropattern", adjust = "none")

em.msd_tw.SFWMD_wateryr
em.msd_tw.hydropattern

emmip(model.sd_tw, SFWMD_wateryr ~ hydropattern)
emmip(model.sd_tw, hydropattern ~ SFWMD_wateryr)

###Interaction term of hydropattern and SFWMD_wateryr came back as significant in model.sd_twts
###The interaction term needs to be broken apart using the emmeans package

em.msd_twts <- emmeans(model.sd_twts, ~ hydropattern * SFWMD_wateryr)
em.msd_twts.SFWMD_wateryr <- contrast(em.msd_twts, "pairwise", by = "SFWMD_wateryr", adjust = "none")
em.msd_twts.hydropattern <- contrast(em.msd_twts, "pairwise", by = "hydropattern", adjust = "none")

em.msd_twts.SFWMD_wateryr
em.msd_twts.hydropattern

emmip(model.sd_twts, SFWMD_wateryr ~ hydropattern)
emmip(model.sd_twts, hydropattern ~ SFWMD_wateryr)