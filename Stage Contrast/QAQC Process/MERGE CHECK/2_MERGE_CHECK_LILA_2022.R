#### LILA STAGE CONTRAST EXPERIMENT MERGE CHECK CODE 
#### Experiment started WET SEASON of 2018 while NJD was at FAU  
#### Experiment was managed at FAU From WET SEASON 2018 - WET SEASON 2021 
#### NJD moved to FIU FROM FAU FALL 2021. all aspects of data analysis and storage 
#### changed to TrexLAB formats to be compatible with all other projects in the LAB 
#### DATA QA/QC for all other projects in the lab are conducted using SAS, LILA QA/QC 
#### IS conducted in R. USING THE following code Processes.

### Code Created 8/11/2022 by JS and NB

#### LILA DATA QA/QC process
#### STEP 1: IMPORT CHECK  
#### STEP 2: MERGE CHECK (YOU ARE HERE) 
#### STEP 3: DATA MERGE 

#This program merges all the throwtrapping data together to check for consistency amoung spatial
#and temporal components. It also will find discrepancies between the wet weight and the animal
#data densities


# TO USE THIS PROGRAM:
# REPLACE ________ COMMENTS WITH RELAVENT DATES AND FILE NAMES FOR THE CURRENT DATA. 
# RUN THE PROGRAM ONE STEP AT A TIME, NOT ALL AT ONCE
# READ COMMENTS AS YOU GO AND FOLLOW DIRECTIONS PROVIDED 
# WHEN QAQC IS COMPLETED, SAVE THE PROGRAM AS A NEW FILE SHOWING THE CURRENT YEAR

#########################################################################################

#### BEGIN MERGE CHECK CODE HERE 
#remove anything in the global environment

rm(list = ls())
   
#### Read in the libraries being used in the code 

#tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: 
#GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats
#readxl allows excel files to be read into R

library(tidyverse)
library(lubridate)

# Import all the QAQC files from the first round of the QAQC process
#his code also renames the comments, sex, and species so they will specify which data
#they come from. The code will also create a new indicator variable which assigns a 
#"1" to each individual observation as a presence variable, this will be used later in the 
#mismatch code 

QC_cray <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_cray_length_2022.csv',
                   na = "NA") %>% 
  rename(cray_comments = Comments,
         cray_length = Length,
         cray_sex = Sex,
         cray_species = Species) %>% 
  mutate(cray1 = 1)


QC_fish <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_fish_length_2022.csv',
                   na = "NA") %>%
  rename(fish_comments = Comments,
         fish_length = Length,
         fish_sex = Sex,
         fish_species = Species) %>% 
  mutate(fish1 = 1)

QC_invt <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_invt_count_2022.csv',
                   na = "NA") %>%
  rename(invt_comments = Comments,
         invt_species = Species) %>% 
  mutate(invt1 = 1)

QC_phys <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_phys_2022.csv',
                   na = "NA") %>% 
  mutate(Year = year(date_time),
         Month = month(date_time),
         Day = day(date_time))%>%
  rename(phys_comments = Comments) %>% 
  mutate(phys1 = 1)

QC_veg <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_veg_2022.csv',
                    na = "NA")%>%
  rename(veg_comments = Comments) %>% 
  mutate(veg1 = 1) 

QC_wwt <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_wwt_2022.csv',
                        na = "NA") %>%
  rename(wwt_comments = Comments) %>% 
  mutate(wwt1 = 1) 

#### 1st QAQC: Merge all the data together to create a file that documents where a "mismatch" has occurred.
#the mismatch will indicate where different spatial or temporal values

#the code joints all the data together using the session, year, month, day, wetland, and throw variables.
#because we have the indicators (i.e.,"1"s from above) this should give us 6 additional columns (one from each
#data sheet), if there is a mismatch we will have duplicate observations for that observation with some missing "1s",
#we assign the missing values a zero. We then sum the six additional variables to find the mismatches in different
#column called "allsum". If "allsum" is less than 6 then there are mismatches between the data and the data need to be 
#fixed to match.

mismatch <- QC_cray %>% 
  full_join(QC_fish, by = c("Session","Year","Month","Day","Wetland","Throw")) %>% 
  full_join(QC_invt, by = c("Session","Year","Month","Day","Wetland","Throw")) %>% 
  full_join(QC_veg, by = c("Session","Year","Month","Day","Wetland","Throw")) %>% 
  full_join(QC_phys, by = c("Session","Year","Month","Day","Wetland","Throw")) %>% 
  full_join(QC_wwt, by = c("Session","Year","Month","Day","Wetland","Throw")) %>% 
  select(Session,Year,Month,Day,Wetland,Throw,cray_comments,fish_comments,invt_comments,
         veg_comments,phys_comments,wwt_comments,cray1,fish1,invt1,veg1,phys1,wwt1) %>% 
  replace_na(replace = list(cray1 = 0,
                            fish1 = 0,
                            invt1 = 0,
                            veg1 = 0,
                            phys1 = 0,
                            wwt1 = 0)) %>% 
  mutate(allsum = cray1+fish1+invt1+veg1+phys1+wwt1) %>%
  filter(allsum < 6) %>% 
  select(Session,Year,Month,Day,Wetland,Throw,cray_comments,fish_comments,invt_comments,veg_comments,
         phys_comments,wwt_comments,cray1,fish1,invt1,veg1,phys1,wwt1,allsum)%>% 
  distinct()

#after running the above code which finds the errors the following code will save the errors
#as a .csv file in the LILA QAQC_data/2022/2_MERGE_CHECK. Make sure to update the file path for year,
#and the file name, which should be saved each consecutive run with and "_1" at the end of the file name
#see below. When there are no mismatches the saved, .csv file will have the variable names with no observations

mismatch %>% 
  write_csv(path =  "M:/LILA/LILA QAQC_data/2022/2_MERGE_CHECK/MISMATCH_LILA_2.csv")

#Annotate ERRORS and changes from the data here:

# 1) The Day on Wetland M2 and Throw 10 did not match the other Day in WWT
#           ->changed the Day from 15 to 16

#### 2nd QAQC: Calculate fish, crayfish, shrimp, other invertebrates, other vertebrates, and mollusk densities,
#and compare those to the wet weight data to find mismatches/errors


##calculate the crayfish densites

cray_dens <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_cray_length_2022.csv',
                    na = "NA") %>% 
  mutate(Category = if_else(Species == "NOCRAY",
                           true = "C",
                           false = "C")) %>% 
  group_by(Session,Year,Month,Day,Wetland,Throw,Category,Species) %>% 
  summarise(Tot = n()) %>% 
  mutate(Tot = if_else (Species == "NOCRAY",
                        true = NA_integer_,
                        false = Tot)) %>% 
  group_by(Session,Year,Month,Day,Wetland,Throw,Category) %>% 
  summarise(Tot = sum(Tot,na.rm = T))

#calculate the fish densities

fish_dens <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_fish_length_2022.csv',
                    na = "NA") %>%
  mutate(Category = if_else(Species == "NOFISH",
                           true = "F",
                           false = "F")) %>% 
  group_by(Session,Year,Month,Day,Wetland,Throw,Category,Species) %>% 
  summarise(Tot = n())%>% 
  mutate(Tot = if_else (Species == "NOFISH",
                        true = NA_integer_,
                        false = Tot))%>% 
  group_by(Session,Year,Month,Day,Wetland,Throw,Category) %>% 
  summarise(Tot = sum(Tot,na.rm = T))

#calculate the 4 other categories from the invertebrate data (i.e., shrimp, mollusks, other invert, other vert)
#note) if there are additional species not found in 2022 then these species will need to be added to the code.
#to do this add a "|" to the end of the ending species and then write Species == "NEWCODE" where NEWCODE is the code
#of the new species not found in 2022
#an example is given in the notes below

#old code with only 2022 species                 #new code with addition 2022 species  

#         Species == "POMPAL"|                   Species == "POMPAL"|
#         Species == "POMMAC"|                   Species == "POMMAC"|
#         Species == "PLASPP"|                   Species == "PLASPP"|
#         Species == "HAISPP"|                   Species == "HAISPP"|
#         Species == "MELTUB"|                   Species == "MELTUB"|
#         Species == "SPHAER",                   Species == "SPHAER"|
#                                                Species == "NEWCODE"


invt_dens <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_invt_count_2022.csv',
                    na = "NA") %>%
  mutate(Category = if_else(Species == "NOINVT",
                     true = NA_character_,
                     false = if_else(Species == "PALPAL",
                                     true = "S",
                                     false = if_else(Species == "POMPAL"|
                                                     Species == "POMMAC"|
                                                     Species == "PLASPP"|
                                                     Species == "HAISPP"|
                                                     Species == "MELTUB"|
                                                     Species == "SPHAER",
                                                     true = "M",
                                                     false = if_else(Species == "RANATP"|
                                                                     Species == "HYLATP"|
                                                                     Species == "SIRLAC"|
                                                                     Species == "NOTVIR",
                                                                     true="OV",
                                                                     false = "OI"))))) %>% 
  group_by(Session,Year,Month,Day,Wetland,Throw,Category,Species) %>%
  summarise(Tot = sum(Count)) %>% 
  group_by(Session,Year,Month,Day,Wetland,Throw,Category) %>%
  summarise(Tot = sum(Tot,na.rm = T)) %>% 
  ungroup() %>% 
  complete(nesting(Session,Year,Month,Day,Wetland,Throw),Category,fill = list(Tot = 0)) %>% 
  drop_na(Category)

#combine the crayfish densities with the fish and other categories' densities

animaldensity <- cray_dens %>% 
  bind_rows(fish_dens) %>% 
  bind_rows(invt_dens)

##merge the calculated densities with the wet weight data. Find the mismatches and indicate them
#in and error column

all_merge <-  animaldensity %>% 
  full_join(QC_wwt, by = c("Session","Year","Month","Day","Wetland","Throw","Category")) %>% 
  select(-wwt_comments,-wwt1) %>% 
  mutate(error = if_else(Tot > 0 & `Wet Weight (g)` == 0,
                         true = "Missing Weight",
                         false = if_else(Tot == 0 & `Wet Weight (g)` > 0,
                                         true = "Missing Density",
                                         false = NA_character_)))

#after running the above code which finds the errors the following code will save the errors
#as a .csv file in the LILA QAQC_data/2022/2_MERGE_CHECK. Make sure to update the file path for year,
#and the file name, which should be saved each consecutive run with and "_1" at the end of the file name
#see below. When there are no mismatches the saved, .csv file will have the variable names with no observations

all_merge %>% 
  filter(!is.na(error)) %>% 
  write_csv(path =  "M:/LILA/LILA QAQC_data/2022/2_MERGE_CHECK/DENSITY_WEIGWT_CHECK_LILA_2.csv")

#Annotate ERRORS and changes to the data here:

# 1) The Shrimp weight and density on Wetland M3 and Throw 13 did not match t
#           ->miss wrote PALPAL instead of PELSPP: Data was changed to PELSPP