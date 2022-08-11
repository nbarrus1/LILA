#### LILA STAGE CONTRAST EXPERIMENT INVERTEBRATE IMPORT CHECK CODE 
#### Experiment started WET SEASON of 2018 while NJD was at FAU  
#### Experiment was managed at FAU From WET SEASON 2018 - WET SEASON 2021 
#### NJD moved to FIU FROM FAU FALL 2021. all aspects of data analysis and storage 
#### changed to TrexLAB formats to be compatible with all other projects in the LAB 
#### DATA QA/QC for all other projects in the lab are conducted using SAS, LILA QA/QC 
#### IS conducted in R. USING THE following code Processes.

### Code Created 8/11/2022 by JS and NB

#### LILA DATA QA/QC process
#### STEP 1: IMPORT CHECK  (YOU ARE HERE) 
#### STEP 2: MERGE CHECK 
#### STEP 3: DATA MERGE 

#---------------------------------------------------------------------------------------
#########################*THIS PROGRAM RUNS BEFORE THE FISH FILE#########################
#--------------------------------------------------------------------------------------

# TO USE THIS PROGRAM:
  # REPLACE ________ COMMENTS WITH RELAVENT DATES AND FILE NAMES FOR THE CURRENT DATA. 
# RUN THE PROGRAM ONE STEP AT A TIME, NOT ALL AT ONCE
# READ COMMENTS AS YOU GO AND FOLLOW DIRECTIONS PROVIDED 
# WHEN QAQC IS COMPLETED, SAVE THE PROGRAM AS A NEW FILE SHOWING THE CURRENT YEAR

#########################################################################################
#remove anything in the global environment

rm(list = ls())

#### BEGIN IMPORT CHECK CODE HERE 
#### Read in the libraries being used in the code 

#tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: 
#GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats
#readxl allows excel files to be read into R
#naniar will easily replace (.) with NAs

library(tidyverse)
library(naniar)
library(readxl)


#### Import Cray file as an xlsx file, update file path every year/season 
invt_count <- read_xlsx('M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_INVT_SPRING.xlsx',
                        na = ".") 

#### 1st QAQC: Checking Session, Wetland, Year, Month, Day, Throw for Errors.
#note) when checking session, the session name will need to be replaced with
#the session that you're working with. Also) spaces and capitalization 
#matter in this name
#note 2) the QC process will change the data type from numeric to character 
#which makes sense because throw is categorical type variable

QC_INVT <- invt_count %>% 
  mutate(Session = if_else(Session == "Spring 2022",        #session will need to be changed to the current session
                                   true = paste(Session),
                                   false = "Session Error"),
         Wetland = if_else(Wetland == "M1" |
                           Wetland == "M2" |
                           Wetland == "M3" |
                           Wetland == "M4",
                                   true = paste(Wetland),
                                   false = "Wetland Error"),
         Year = if_else(Year == 2022,                       #year will need to be changed to the current year
                                   true = paste(Year),
                                   false = "Year Error"),
         Month = if_else(Month > 0 &
                         Month < 13, 
                                   true = paste(Month),
                                   false = "Month Error"),
         Day = if_else(Day >0 &
                       Day <32, 
                                   true = paste(Day),
                                   false = "Day Error"),
         Throw = if_else(Throw >0 & 
                         Throw<15,
                                  true = paste(Throw),
                                  false = "Throw Error"))

#check to see if we have any "Session Errors". the following code should print out any errors in 
#the R console (bottom left screen). However if there are more than (~10 errors) then it will
#only print out the first 10. The first line of the output will give the actual number of session errors. 
#This output will read: "#A tibble: Some Number x 15" where "Some Number" is the number of errors found
#Fix the first 10 printed errors, then re-upload and rerun the previous line of code
#to find then additional errors not printed

#note) some "session errors" may also have errors from other variables.
#it will be more efficient to fix both before moving on

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Session == "Session Error") 

#same thing as above but for "Wetland Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Wetland == "Wetland Error")

#same thing as above but for "Year Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Year == "Year Error")

#same thing as above but for "Month Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"


QC_INVT %>% 
  filter(Month == "Month Error")

#same thing as above but for "Day Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Day == "Day Error")

#same thing as above but for "Throw Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Throw == "Throw Error")


#FIX THESE ERRORS BEFORE MOVING FORWARD!!!!

# Annotate ERRORS and changes from the data here:
# none
table(invt_count$Species)

#### 2nd QAQC: Checking Species, Length, SEX, FORM and COMMENT
#note) the QC process will change the data type from numeric to character 
#which makes sense because throw is a categorical type variable

QC_INVT <- QC_INVT %>%
  separate(Comments, into = c("comm_count", "Comments"), sep = " ", fill = "left") %>% 
  mutate(Species = if_else(Species == "NOINVT" |
                           Species == "BELINC" |
                           Species == "BELNYM" |
                           Species == "BELSPP" |
                           Species == "BRAGRA" |
                           Species == "CELSPP" |
                           Species == "COENAG" |
                           Species == "CORING" |
                           Species == "DINSPP" |
                           Species == "DYTSPP" |
                           Species == "EPHEME" |
                           Species == "GYRELE" |
                           Species == "ERYSIM" |
                           Species == "HAISPP" |
                           Species == "HIRUDI" |
                           Species == "HYDSPP" |
                           Species == "HYLATP" |
                           Species == "LIBINC" |
                           Species == "MELTUB" |
                           Species == "NEOHES" |
                           Species == "NEOUNI" |
                           Species == "NOTVIR" |
                           Species == "PACLON" |
                           Species == "PALPAL" |
                           Species == "PELSPP" |
                           Species == "PLASPP" |
                           Species == "POMMAC" |
                           Species == "RANATP" |
                           Species == "SIRLAC" |
                           Species == "SPHAER" |
                           Species == "STRATI" ,
                                  true = paste(Species),
                                  false = "Species Error"),
         Comments = if_else(Comments == "NOINVT" |
                            Comments == "NODATA" |
                            Comments == "ROTCUP"|
                            Comments == "PRTMIS"|
                            Comments == "EMPCUP"|
                            Comments == "GRAVID",
                                  true = paste(Comments),
                                  false = "Comments Error"),
         Count = as.numeric(Count),
         Count = if_else(Count >= 0 & Count < 500,
                          true = paste(Count),
                          false = "Count Error"))

#check to see if we have any "Species Errors". the following code should print out any errors in 
#the R console (bottom left screen). However if there are more than (~10 errors) then it will
#only print out the first 10. The first line of the output will give the actual number of session errors. 
#This output will read: "#A tibble: Some Number x 15" where "Some Number is the number of errors found
#Fix the first 10 printed errors, then re-upload and rerun the previous line of code
#to find then additional errors not printed

#note) some "session errors" may also have errors from other variables.
#it will be more efficient to fix both before moving on

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Species == "Species Error")

#same thing as above but for "Comments Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Comments == "Comments Error")

#same thing as above but for "Length Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_INVT %>% 
  filter(Count == "Count Error")


# Annotate ERRORS and changes from the data here:
# none

#---------------------------------
#prepare for data merging####
#---------------------------------

#remove any comments that refer to a single speciment and not an entire cup (e.g., gravid, partmis, etc. )

QC_INVT <- QC_INVT %>% 
  mutate(Comments = if_else(Comments == "ROTCUP"|Comments == "EMPCUP",
                            true = paste(Comments),
                            false = NA_character_)) %>% 
  select(-`Sorted By`,-`Checked By`,-`Entered By`,-comm_count)

table(is.na(QC_INVT$Comments)) #no rotcups or empcups in the set means all should be NA (i.e., code spits out all "trues")

QC_INVT %>% 
  write_csv(file = "M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_invt_count_2022.csv")
