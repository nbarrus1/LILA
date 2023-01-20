#this script is to load in data for all the analyses performed in the isocline manuscript
#this code should be run prior to all the other code

rm(list = ls())

#-------------------------------------------
####Libraries#####
#-------------------------------------------

library(tidyverse)
library(readxl)
library(here)
library(lubridate)

#--------------------------------------------
#####Tethering Data#####
#--------------------------------------------

#####load in data####
tetherdata.raw <- read_excel(here("Pomacea/Isocline_manuscript/data","tetheringdata.xlsx"), sheet = 4)

lengthcodes <- read_excel(here("Pomacea/Isocline_manuscript/data","tetheringdata.xlsx"), sheet = 2)      

tetherdata <- tetherdata.raw %>% 
  left_join(lengthcodes, by = c("season", "box", "bin", "box.id","box.day")) %>% 
  dplyr::select(-entered.by.x, -entered.by.y, -checked.by.x, -checked.by.y) %>% 
  mutate(survival = if_else(fate=="a", true = 1, false = 0))             #this adds a variable called survival with 1 = survival, and 0 = dead/eaten

rm(list = c("lengthcodes","tetherdata.raw"))

#--------------------------------------------
#####predator data####
#--------------------------------------------

predatordata <- read_excel(here("Pomacea/Isocline_manuscript/data","PredatorDiets_v1.5.xls"), sheet = 3,
                           na = "UNK") %>% 
  mutate(juvcray = if_else(SpeciesCode == "Profal" & StandLen_mm < 14, true = 0,
                           false = 1))

#---------------------------------------------
####predator free survival####
#---------------------------------------------

predatorfree <- read_excel("predatorfree_survival.xlsx", sheet = 2)

#--------------------------------------------
####Water and Air Temps####
#--------------------------------------------

#read in water temperatures add cell names as grouping variables

WATERTEMP_M2 <- read_csv(here("Pomacea/Isocline_manuscript/data", "HOBO_20423783_M2_12-17-2020--8-16-2021.csv"), skip = 1) %>%  #read in the M2 data
  mutate(type = "water_M2")                                                                  #name as M2 in new variable called "type"

WATERTEMP_M3 <- read_csv(here("Pomacea/Isocline_manuscript/data", "HOBO_20423785_M3_12-17-2020--8-16-2021.csv"), skip = 1) %>%  #read in the M3 data
  mutate(type = "water_M3")                                                                  #name as M3 in new variable called "type"

AIRTEMP <- read_csv(here("Pomacea/Isocline_manuscript/data","DBHYDRO_airtemp_12-18-2020--8-11-2021.csv"), skip = 3) %>%  #read in the air temp data
  rename(date.time =`Daily Date`,                                 #change daily date to date.time                 
         temp.c = `Data Value`) %>%                               #change data value to air.temp.c
  dplyr::select(date.time,temp.c) %>%                                #only use these variables
  mutate(date.time = as_datetime(dmy(date.time)),
         type = "air_WestPalm")                                   #format the date and time to r's structure

#combine the data, fix variable names, set date structure, remove setting day and ending day

TEMP <- WATERTEMP_M2 %>%                                          #save WATERTEMP_M2 as a different tibble 
  bind_rows(WATERTEMP_M3) %>%                                     #combine WATERTEMP_M3 using the column names
  rename(obs = "#",                                               #change "#" into obs
         date.time = `Date Time, GMT-05:00`,                      #change the large variable name to date.time
         temp.c = `Avg: Temperature`) %>%                         #change Avg: Temp to air.temp.c
  mutate(date.time = as_datetime(mdy_hm(date.time))) %>%          #format date.time to R's date time structure
  filter(date.time %within% interval(start = ymd("2020-12-18"),   #filter the data within the time intervals specified
                                     end = ymd("2021-8-11"))) %>% # 
  bind_rows(AIRTEMP) %>% 
  dplyr::select(date.time, type, temp.c) %>% 
  mutate(season = if_else(date.time %within% interval(start = ymd("2020-12-18"),
                                                      end = ("2021-5-31")),
                          true = "dry", false = "wet"))

#check the data

table(is.na(TEMP$date.time))          # NAs

TEMP <- TEMP %>%                      #remove the one NA
  drop_na(date.time)

table(is.na(TEMP$date.time))          #no NAs
table(is.na(TEMP$temp.c))             #no NAs
table(is.na(TEMP$season))             #no NAs
table(TEMP$type)                      #looks good each type as the same number of observations
table(TEMP$season)                    #as expected there are more observatin in dry than wet

rm(list = c("AIRTEMP","WATERTEMP_M2","WATERTEMP_M3"))

