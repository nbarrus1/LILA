#The purpose of this r script and code is to reformat the LILA data collected from FAU to the format used in the FIU Lab.
#key aspect and features of this code.. data frames and matrices will be in ALL CAPS and any "spaces" will be coded with underscores (_)
#                                    .. vectors and numbers and variables will be in lower case and "spaces" will be coded with periods (.)


rm(list = ls())
#-----------------------------------------------
####Load libraries#####
#-----------------------------------------------

#the two packages necessary for this code are tidyverse and readxl
#tidyverse is a different r syntax that enhances readability 
#readxl allows data to be entered directly from excel formats rather than just .csv
#lubridate is the tidyverse package for working with dates and time

library(tidyverse)
library(readxl)
library(lubridate)

#-----------------------------------------------
####Load in Data####
#-----------------------------------------------

#load in the fish and crayfish data specifying the year using the sheet number
FISH_CRAY_SUMMER_2018 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 3)
FISH_CRAY_SPRING_2019 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 4)
FISH_CRAY_SUMMER_2019 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 5)
FISH_CRAY_SPRING_2020 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 6)
FISH_CRAY_SUMMER_2020 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 7)
FISH_CRAY_SPRING_2021 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 8)
FISH_CRAY_SUMMER_2021 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 9)

#combine the files into one

FISH_CRAY_DATA <- FISH_CRAY_SPRING_2019 %>% 
  bind_rows(FISH_CRAY_SPRING_2020) %>% 
  bind_rows(FISH_CRAY_SPRING_2021) %>% 
  bind_rows(FISH_CRAY_SUMMER_2018) %>% 
  bind_rows(FISH_CRAY_SUMMER_2019) %>% 
  bind_rows(FISH_CRAY_SUMMER_2020) %>% 
  bind_rows(FISH_CRAY_SUMMER_2021)



#load in the invert and physical data by specifying the sheet
INVT_PHYS_SUMMER_2018 <- read_excel(path = "M:/LILA/Stage Contrast Study/Throw Trap Data/LILA_Stage_Contrast_TTdata_WS2018-WS2021.xlsx", sheet = 2)%>% 
  mutate(Macrocosm = if_else(Macrocosm == 1,
                             true = "M1",
                             false = if_else(Macrocosm == 2,
                                             true = "M2", 
                                             false = if_else(Macrocosm == 3,
                                                             true = "M3",
                                                             false = "M4"))))%>% 
  mutate(date_time = mdy_hm(date_time))

#get the throw trap dates for cray and fish from the physical
COD <- INVT_PHYS_SUMMER_2018 %>% 
  select(Session, Macrocosm,`Throw Trap`, Date) 

#give the initials for those who QA/QC the data

QA <- tibble(session = c("Summer 2018","Summer 2019", "Summer 2020", "Summer 2021", "Spring 2019", "Spring 2020",  "Spring 2021"),
             checked.by = c("JB","AL","BM","KC","JB","BM","SO")) %>% 
  mutate(sorted.by = "JS",
         entered.by = "JS")

#----------------------------------
#####Reformat Crayfish and Fish Data####
#----------------------------------

#check the data species codes
table(FISH_CRAY_DATA$`Species Code`) #there are some errors in some of the species codes
table(FISH_CRAY_DATA$Sex)            #some errors in case for male and female
table(FISH_CRAY_DATA$Form)
table(FISH_CRAY_DATA$`Throw Trap`)
table(FISH_CRAY_DATA$Macrocosm)
table(FISH_CRAY_DATA$Session)

#fix the errors and change the species codes to ALL CAPS because species codes in Lab are in all caps
FISH_CRAY_DATA <- FISH_CRAY_DATA %>% 
  mutate(`Species Code` = if_else(`Species Code` == "Lep spp",
                                  true = "LEPSPP",
                                  false = if_else(`Species Code` == "Catfry",
                                                  true = "CATFRY",
                                                  false = if_else(`Species Code` == "Cicuro",
                                                                  true = "CICURO",
                                                                  false = if_else(`Species Code` == "Circuro",
                                                                                  true = "CICURO",
                                                                                  false = if_else(`Species Code` == "Clabat",
                                                                                                  true = "CLABAT",
                                                                                                  false = if_else(`Species Code` == "Elaeve",
                                                                                                                  true = "ELAEVE",
                                                                                                                  false = if_else(`Species Code` == "Ennglo",
                                                                                                                                  true = "ENNGLO",
                                                                                                                                  false = if_else(`Species Code` == "Erisuc",
                                                                                                                                                  true = "ERISUC",
                                                                                                                                                  false = if_else(`Species Code` == "Ethfus",
                                                                                                                                                                  true = "ETHFUS",
                                                                                                                                                                  false = if_else(`Species Code` == "Fry",
                                                                                                                                                                                  true = "FRY",
                                                                                                                                                                                  false = if_else(`Species Code` == "Funchr",
                                                                                                                                                                                                  true = "Funchr",
                                                                                                                                                                                                  false = if_else(`Species Code` == "Gamhol",
                                                                                                                                                                                                                  true = "GAMHOL",
                                                                                                                                                                                                                  false = if_else(`Species Code` == "Hetfor",
                                                                                                                                                                                                                                  true = "HETFOR",
                                                                                                                                                                                                                                  false = if_else(`Species Code` == "Jorflo",
                                                                                                                                                                                                                                                  true = "JORFLO",
                                                                                                                                                                                                                                                  false = if_else(`Species Code` == "Labsic",
                                                                                                                                                                                                                                                                  true = "LABSIC",
                                                                                                                                                                                                                                                                  false = if_else(`Species Code` == "Lepgul",
                                                                                                                                                                                                                                                                                  true = "LEPGUL",
                                                                                                                                                                                                                                                                                  false = if_else(`Species Code` == "Lepmac",
                                                                                                                                                                                                                                                                                                  true = "LEPMAC",
                                                                                                                                                                                                                                                                                                  false = if_else(`Species Code` == "Lepmar",
                                                                                                                                                                                                                                                                                                                  true = "LEPMAR",
                                                                                                                                                                                                                                                                                                                  false = if_else(`Species Code` == "Leppun",
                                                                                                                                                                                                                                                                                                                                  true = "LEPPUN",
                                                                                                                                                                                                                                                                                                                                  false =if_else(`Species Code` == "Lepspp",
                                                                                                                                                                                                                                                                                                                                                 true = "LEPSPP",
                                                                                                                                                                                                                                                                                                                                                 false =if_else(`Species Code` == "Lucgoo",
                                                                                                                                                                                                                                                                                                                                                                true = "LUCGOO",
                                                                                                                                                                                                                                                                                                                                                                false =if_else(`Species Code` == "Micsal",
                                                                                                                                                                                                                                                                                                                                                                               true = "MICSAL",
                                                                                                                                                                                                                                                                                                                                                                               false =if_else(`Species Code` == "NO FISH",
                                                                                                                                                                                                                                                                                                                                                                                              true = "NOFISH",
                                                                                                                                                                                                                                                                                                                                                                                              false =if_else(`Species Code` == "Notcry",
                                                                                                                                                                                                                                                                                                                                                                                                             true = "NOTCRY",
                                                                                                                                                                                                                                                                                                                                                                                                             false = if_else(`Species Code` == "Oreaur",
                                                                                                                                                                                                                                                                                                                                                                                                                             true = "OREAUR",
                                                                                                                                                                                                                                                                                                                                                                                                                             false = if_else(`Species Code` == "Poelat",
                                                                                                                                                                                                                                                                                                                                                                                                                                             true = "POELAT",
                                                                                                                                                                                                                                                                                                                                                                                                                                             false = if_else(`Species Code` == "Profal",
                                                                                                                                                                                                                                                                                                                                                                                                                                                             true = "PROFAL",
                                                                                                                                                                                                                                                                                                                                                                                                                                                             false = if_else(`Species Code` == "Unk Fish",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             true = "UNKFISH",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             false = if_else(`Species Code` == "UNK fish",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             true = "UNKFISH",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             false = if_else(`Species Code` == "UNKfish",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             true = "UNKFISH",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             false = if_else(`Species Code` == "UNKFISH",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             true = "UNKFISH",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             false = "NOCRAY"))))))))))))))))))))))))))))))))

#check the codes

table(FISH_CRAY_DATA$`Species Code`)

#fix the sexes and change the codes to numbers because code in lab is in all caps

FISH_CRAY_DATA <- FISH_CRAY_DATA %>% 
  mutate(Sex = if_else(Sex == "M",
                       true = 1,
                       false = if_else(Sex == "m",
                                       true = 1,
                                       false = if_else(Sex == "N",
                                                       true = 1,
                                                       false = if_else(Sex == "F",
                                                                       true = 2,
                                                                       false = 2)))))
#check the sex codes

table(FISH_CRAY_DATA$Sex) 

#combine our catch dates and the info on the people who QA/QC the data
#rename the variables to the format we would like
#add the month day year as variables
#note: after running the rename code in this set of code the data frame has new names. Because I save the 
#note cont.: the changes to the same data frame, if you run the code a second time you will get an error saying
#note cont.: the old variables do not exist. To fix this, run all the code from the beginning

FISH_CRAY_DATA <- FISH_CRAY_DATA %>% 
  left_join(COD, by = c("Session", "Macrocosm","Throw Trap")) %>% 
  rename(session = Session,
         macrocosm = Macrocosm,
         throw = `Throw Trap`,
         species = `Species Code`,
         length = `Length (SL/CL)`,
         sex = Sex,
         form = Form,
         comments = Notes,
         doc = Date,
         dop = `Date (sample proccesing)`) %>% 
    left_join(QA, by = "session") %>% 
    mutate(month = month(doc),
         day = day(doc),
         year = year(doc))

#change the order of the variables

FISH_CRAY_DATA <- FISH_CRAY_DATA %>% 
  select(session,doc,year,month,day,macrocosm,throw,species,length,sex,form,comments,dop,sorted.by,entered.by,checked.by) 

#----------------------------------------------
###### fish data #####
#----------------------------------------------

#subset the data to only fish and remove the form variable becasue form does not apply to fish


FISH <- FISH_CRAY_DATA %>% 
  filter(species != "PROFAL") %>% 
  filter(species != "NOCRAY") %>% 
  select(-form)
  
#subset the data by year 
#save that years data to the LILA Data Entry folder in LILA's folder in the M drive
#note: this must be done for each year in LILA you can change this by changing the year in the filter line of code
#note cont.: when saving a different year you will also need to update the file location namely the folder for year
#note cont.: and the file name
#2nd note: Please do not run this chunk of code without a valid reason because lost fish from the TT have been 
#2nd note cont: added to the CSVs post reformatting and saved directly in excel. A work around would be to change the
#2nd note cont:file name then save if necessary but then it would not contain the lost fish from the TTs.

FISH %>% 
  filter(year == 2018) %>% 
  write_csv(file = "M:/LILA/LILA Data Entry/2018/LILA_TT_2018_FISH.csv")

#------------------------------------------------
##### crayfish data#####
#------------------------------------------------

#subset the data to only crayfish,fix the names of juveniles to PROSPP, and updated the form to reproductive
#and non-reproductive by size because in the FIU lab form is assigned based by size. But in the FAU lab, Jeff
#assigned the form based on the shape of the claws

CRAY <- FISH_CRAY_DATA %>% 
  filter(species == "PROFAL"| species == "NOCRAY") %>% 
  mutate(species = if_else(length <= 8 & sex == 2,
                           true = "PROSPP",
                           false = if_else(length <= 7 & sex == 1,
                                           true = "PROSPP",
                                           false = species)),
         form = if_else(length <= 12,
                        true = 2,
                        false = 1),
         species = if_else(is.na(length)& is.na(sex),
                           true = "NOCRAY",
                           false = if_else(is.na(length),
                                           true = "PROSPP",
                                           false = species)),
         species = if_else(is.na(species),
                           true = "PROSPP",
                           false = species))

#subset the data by year 
#save that years data to the LILA Data Entry folder in LILA's folder in the M drive
#note: this must be done for each year in LILA you can save each year by changing the year in the filter line of code
#note cont.: when saving a different year you will also need to update the file location namely the folder for year
#note cont.: and the file name
  
CRAY %>% 
  filter(year == 2021) %>% 
  write_csv(file = "M:/LILA/LILA Data Entry/2021/LILA_TT_2021_CRAY.csv")

#--------------------------------------------
#### Reformat the Physical data ####
#--------------------------------------------


#format the Physical data by selecting all the variables desired for the physical data,
#rename the variables the variables to the desired names, use the date_time variable to format the dates
#as desired, add the QA data, then remove the date_time and sorted.by variable


PHYS <- INVT_PHYS_SUMMER_2018 %>% 
  select(Session, Macrocosm, `Throw Trap`, Location, `X Coord.`, `Y Coord.`, Date, Time,date_time,Day, wateryr, `Water Depth (cm)`,
         `Total % Cover`, `peri % cover`, `Peri Vol. (mL)`) %>% 
  rename(session = Session,
         macrocosm = Macrocosm,
         throw = `Throw Trap`,
         location = Location,
         date = Date,
         time = Time,
         day = Day,
         depth = `Water Depth (cm)`,
         total.cover = `Total % Cover`,
         peri.cover = `peri % cover`,
         peri.volume = `Peri Vol. (mL)`) %>% 
  mutate(date = paste(month(date_time),day(date_time),year(date_time), sep = "/"),
         time = paste(hour(date_time),minute(date_time),sep = ":"),
         year = year(date_time)) %>% 
  left_join(QA,by = "session") %>% 
  select(-date_time,-sorted.by)

#subset the data by year then remove the year variable as it is not desired
#save that years data to the LILA Data Entry folder in LILA's folder in the M drive
#note: this must be done for each year in LILA you can save each year by changing the year in the filter line of code
#note cont.: when saving a different year you will also need to update the file location namely the folder for year
#note cont.: and the file name

PHYS %>% 
  filter(year == 2018) %>% 
  select(-year) %>% 
  write_csv(file = "M:/LILA/LILA Data Entry/2018/LILA_TT_2018_PHYS.csv")

#--------------------------------------------
#### Reformat the Vegetation data ####
#--------------------------------------------

#format the physical data by, 1) renaming the vegetation species to their species codes, 2)use the date_time variable
#to extract the year, month, and day, 3)add a comment variable, 4) select all the vegetation variables,
#5) gather all the vegetation variables into two variables: one grouping called species and one value called 
#live.density, 6) rename all the individual grass variable into one variable called GRASS, 7) format the live.density
#variable to numeric, 8) group everything by all variables except live density so that we can summarize the variables 
#and add the grass species together, 9) remove zeros so only species present in throws are included, 10) include
#the QA data, and 11) reorder the variables to the desired order

VEG <- INVT_PHYS_SUMMER_2018 %>% 
  rename(UTRPUR = `u_purpurea_%`,
         UTRFOL = `u_foliosa_%`,
         BACCAR = `bacopa_%`,
         POTILL = `pot_%`,
         CHASPP = `chara_%`,
         ELEELO = `Eleocharis elongata`,
         ELECEL = `Eleocharis cellulosa`,
         ELEINT = `Eleocharis Interstincta`,
         ELESPP = `Eleocharis spp`,
         GRASS_pan = `Panicum spp`,
         NYMORO = `Nymphaea odorata`,
         NUPADV = `Nuphar advena`,
         PONCOR = `Pontederia cordata`,
         RHYSPP = `Rhynchospora spp`,
         CRIAME = `Crinum americanum`,
         SAGSPP = `Sagittaria spp`,
         POTILL_stem = `Potamegaton spp.`,
         GRASS = Grass,
         CLAJAM = `Cladium jamaicense`,
         GRASS_pas = `Paspalidium spp.`,
         TYPSPP = `Typha spp`,
         URELOB = `Urena lobata`,
         UNKDIC = `Unknown Dicot`,
         session = Session,
         throw = `Throw Trap`,
         macrocosm = Macrocosm) %>% 
  mutate(year = year(date_time),
         month = month(date_time),
         day = day(date_time),
         comment = ".") %>% 
  select(session, year,month,day,macrocosm, throw, UTRPUR,UTRFOL,BACCAR,POTILL,CHASPP,ELEELO,ELECEL,ELESPP,
         GRASS,NYMORO,NUPADV,PONCOR,RHYSPP,POTILL_stem,CLAJAM,TYPSPP,URELOB,UNKDIC,GRASS_pan,GRASS_pas,comment) %>% 
  gather(UTRPUR,UTRFOL,BACCAR,POTILL,CHASPP,ELEELO,ELECEL,ELESPP,
         GRASS,NYMORO,NUPADV,PONCOR,RHYSPP,POTILL_stem,CLAJAM,TYPSPP,URELOB,UNKDIC,GRASS_pan,GRASS_pas,
         key ="species",value ="live.density") %>% 
  mutate(species = if_else(species == "GRASS" | species == "GRASS_pan" | species == "GRASS_pas",
                           true = "GRASS",
                           false = paste(species)),
         live.density = as.numeric(live.density)) %>% 
  group_by(session, year, month, day, macrocosm, throw, species,comment) %>% 
  summarise(live.density = sum(live.density, na.rm = T)) %>% 
  filter(live.density > 0) %>% 
  left_join(QA, by = "session") %>% 
  select(-sorted.by) %>% 
  select(session, year, month, day, macrocosm, throw, species,live.density,comment,entered.by,checked.by)

#subset the data by year then remove the year variable as it is not desired
#save that years data to the LILA Data Entry folder in LILA's folder in the M drive
#note: this must be done for each year in LILA you can save each year by changing the year in the filter line of code
#note cont.: when saving a different year you will also need to update the file location namely the folder for year
#note cont.: and the file name

VEG %>% 
  filter(year == 2018) %>% 
  ungroup() %>% 
  select(-year) %>% 
  write_csv(file = "M:/LILA/LILA Data Entry/2018/LILA_TT_2018_VEG.csv")

#--------------------------------------------------
#####Reformat the Invertebrate Data#####
#--------------------------------------------------

INVT <- INVT_PHYS_SUMMER_2018 %>% 
  rename(COLEO_a = coleo_a,
         COLEO_l = coleo_l,
         CYBIST = cybist,
         COENAG = coenag,
         AESSPP = aesspp,
         BELSPP = belspp,
         LIBSPP = libspp,
         DIPTER = dipter,
         EPHEME = epheme,
         OLIGOC = oligoc,
         PELSPP = pelfem,
         PALPAL = palpal,
         POMMAC = pommac,
         POMPAL = pompal,
         PLASPP = plaspp,
         HAISPP = physpp,
         SPHAER = sphaer,
         VILAMY = vilamy,
         MELTUB.1 = meltub,
         MELTUB.2 = Meltub,
         TRICHO_l = tricho_l,
         TIPULI = tipuli,
         TABANI = tabani,
         STRATI = strati,
         CORIXI = corixi,
         GOMPHI = gomphi,
         LITSPP = litspp,
         HETERO = hetero,
         SIRLAC = sirlac,
         SIRINT = sirint,
         RANATP = rana,
         NOTVIR = notvir,
         HIRUDI = hirudi,
         AMPMEA = ampmea,
         SPOLAC = Spolac,
         APHSPP = Aphspp,
         session = Session,
         throw = `Throw Trap`,
         macrocosm = Macrocosm)%>% 
  mutate(doc = paste(month(date_time),day(date_time),year(date_time), sep = "/"),
         year = year(date_time),
         month = month(date_time),
         day = day(date_time),
         comment = ".") %>% 
  select(session, doc, year, month, day, macrocosm, throw,comment, COLEO_a, COLEO_l, CYBIST, COENAG,AESSPP,BELSPP,LIBSPP,
         DIPTER,EPHEME,OLIGOC,PELSPP, PALPAL, POMMAC, POMPAL, PLASPP, HAISPP,SPHAER,VILAMY,MELTUB.1,MELTUB.2,TRICHO_l,
         TIPULI,TABANI,STRATI,CORIXI, GOMPHI, LITSPP,HETERO,SIRLAC,SIRINT,RANATP,NOTVIR,HIRUDI,AMPMEA,SPOLAC,APHSPP) %>% 
  gather(COLEO_a, COLEO_l, CYBIST, COENAG,AESSPP,BELSPP,LIBSPP,
         DIPTER,EPHEME,OLIGOC,PELSPP, PALPAL, POMMAC, POMPAL, PLASPP, HAISPP,SPHAER,VILAMY,MELTUB.1,MELTUB.2,TRICHO_l,
         TIPULI,TABANI,STRATI,CORIXI, GOMPHI, LITSPP,HETERO,SIRLAC,SIRINT,RANATP,NOTVIR,HIRUDI,AMPMEA,SPOLAC,APHSPP,
         key = "species", value = "count") %>% 
  mutate(species = if_else(species == "MELTUB.1" | species == "MELTUB.2" ,
                           true = "MELTUB",
                           false = paste(species)),
         live.density = as.numeric(count)) %>% 
  group_by(session,doc, year, month, day, macrocosm, throw, species,comment) %>% 
  summarise(count = sum(count, na.rm = T)) %>% 
  filter(count > 0) %>% 
  left_join(QA, by = "session") %>% 
  select(-sorted.by) %>% 
  select(session,doc, year, month, day, macrocosm, throw, species,count,comment,entered.by,checked.by)

#subset the data by year then remove the year variable as it is not desired
#save that years data to the LILA Data Entry folder in LILA's folder in the M drive
#note: this must be done for each year in LILA you can save each year by changing the year in the filter line of code
#note cont.: when saving a different year you will also need to update the file location namely the folder for year
#note cont.: and the file name

INVT %>% 
  filter(year == 2018) %>% 
  write_csv(file = "M:/LILA/LILA Data Entry/2018/LILA_TT_2018_INVT.csv")
