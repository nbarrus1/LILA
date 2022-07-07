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
                                                             false = "M4"))))

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

#fix the errors and change the
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

#fix the sexes and change the codes to numbers

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
         dop = `Date (sample proccesing)`)

FISH_CRAY_DATA <- FISH_CRAY_DATA %>% 
  mutate(month = month(doc),
         day = day(doc),
         year = year(doc)) %>% 
  left_join(QA, by = "session") 

FISH_CRAY_DATA <- FISH_CRAY_DATA %>% 
  select(session,doc,year,month,day,macrocosm,throw,species,length,sex,form,comments,dop,sorted.by,entered.by,checked.by) 
#----------------------------------------------
###### fish data #####
#----------------------------------------------


FISH <- FISH_CRAY_DATA %>% 
  filter(species != "PROFAL"| species != "NOCRAY") %>% 
  select(-form)
  

FISH %>% 
  filter(year == 2021) %>% 
  write_csv(file = "M:/LILA/LILA Data Entry/2021/LILA_TT_2021_FISH.csv")

#------------------------------------------------
##### crayfish data#####
#------------------------------------------------

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
  
CRAY %>% 
  filter(year == 2021) %>% 
  write_csv(file = "M:/LILA/LILA Data Entry/2021/LILA_TT_2021_CRAY.csv")
