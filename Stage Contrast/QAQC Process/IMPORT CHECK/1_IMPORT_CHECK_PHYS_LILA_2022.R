#### LILA STAGE CONTRAST EXPERIMENT PHYSICAL DATA IMPORT CHECK CODE 
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

library(tidyverse)
library(naniar)
library(readxl)
library(lubridate)

#tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: 
#GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats
#readxl allows excel files to be read into R

#### Import Physical data file as an xlsx file, update file path every year/season 
#note) the col_types were specified because to make sure the Time file gets read in as a poisxct struture (date time)
#note 2) the dates and times were combined together into date_time column
#note 3) if there are missing times this code will replace the hour with a 0 and a minute with 00. So,
#the missing times will read 00:00  
PHYS_Data <- read_xlsx('M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_PHYS_SPRING.xlsx',
                       na = ".",
                       col_types = c("guess",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "date",
                                     "guess",
                                     "date",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "guess",
                                     "guess")) %>% 
  mutate(year = year(Date),
         month = month(Date),
         day = day (Date),
         hour = hour(Time),
         minute = minute(Time)) %>% 
  replace_na(list(hour = 0,
                  minute = 0)) %>% 
  mutate(date_time = ymd_hm(paste(year,month,day,hour,minute)),
         Time = as.character(Time)) %>%
  separate(Time, into = c("X","Time"), sep = " ") %>% 
  select(-year,-month,-day,-hour,-minute, -X)

#### 1st QAQC: Checking Dates and Session, Wetland, Year, Month, Day, Throw for Errors.
#note) when checking session, the session name will need to be replaced with
#the session that you're working with. Also) spaces and capitalization 
#matter in this name
#note 2) the QC process will change the data type from numeric to character 
#which makes sense because throw is categorical type variable

QC_PHYS <- PHYS_Data %>% 
  mutate(Session = if_else(Session == "Spring 2022",        #session will need to be changed to the current session
                                   true = paste(Session),
                                   false = "Session Error"),
         Wetland = if_else(Wetland == "M1" |
                           Wetland == "M2" |
                           Wetland == "M3" |
                           Wetland == "M4",
                                   true = paste(Wetland),
                                   false = "Wetland Error"),
         Throw = if_else(Throw >0 & 
                         Throw<15,
                                  true = paste(Throw),
                                  false = "Throw Error"),
         Location = if_else(Location == "DS"|
                            Location == "SS"|
                            Location == "CR",
                                  true = paste(Location),
                                  false = "Location Error"),
         #note the next 8 lines of code deal with the sampling dates these will need to be updated in the interval
         #the dates updated should be within the week of sampling. "start" should be sunday before and "end"
         #should be the saturday after sampling -make sure the dates are entered in the same format as already 
         #written surrounded by a quotations i.e., "MM-DD-YYYY", If there is only one sampling week (i.e., wet season
         # sampling) then you can omit an interval make sure to also omit the "|" symbol
         Date = if_else(date_time %within% interval(start = mdy("03-13-2022"), end = mdy("03-19-2022"))|
                        date_time %within% interval(start = mdy("04-03-2022"), end = mdy("04-09-2022")), 
                                  true = paste(Date),
                                  false = "Date Error"),
         Date = if_else(!is.na(Date),
                        paste(Date),
                        false = "Date Error"),
         Time = if_else(date_time %within% interval(start = mdy("03-13-2022"), end = mdy("03-19-2022"))|
                        date_time %within% interval(start = mdy("04-03-2022"), end = mdy("04-09-2022")),
                                  true =  if_else(is.na(Time),
                                                  true = NA_character_,
                                                  false = paste(Time)),
                                  false = "Time Error"),
         Day = if_else(Day == "Monday"  |
                       Day == "Tuesday" |
                       Day == "Wednesday"|
                       Day == "Thursday"|
                       Day == "Friday",
                                  true = paste(Day),
                                  false = "Day Error"),
         Wateryr = if_else(Wateryr == 2021,
                                  true = paste(Wateryr),
                                  false = "Wateryr Error"))

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

QC_PHYS %>% 
  filter(Session == "Session Error") 

#same thing as above but for "Wetland Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Wetland == "Wetland Error")

#same thing as above but for "Year Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Throw == "Throw Error")

#same thing as above but for "Month Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"


QC_PHYS %>% 
  filter(Location == "Location Error")

#same thing as above but for "Day Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Day == "Day Error")

#same thing as above but for "Throw Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Date == "Date Error")

#same thing as above but for "Throw Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Wateryr == "Wateryr Error")

#same thing as above but for "Throw Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Time == "Time Error")

#FIX THESE ERRORS BEFORE MOVING FORWARD!!!!

#check to see if we have any Times that need to be double schecked (e.g., missing times)
#. the following code should print out any values to be checked in 
#the R console (bottom left screen). However if there are more than (~10 values) then it will
#only print out the first 10. The first line of the output will give the actual number . 
#This output will read: "#A tibble: Some Number x 15" where "Some Number" is the number of values found
#Check the values ix if possible then re-upload and rerun the previous line of code
#to find then additional errors not printed

# Annotate ERRORS and changes from the data here:
# none


#### 2nd QAQC: Checking Species, Length, SEX, FORM and COMMENT
#note) the QC process will change the data type from numeric to character 
#which makes sense because throw is a categorical type variable

QC_PHYS <- QC_PHYS %>%
  separate_rows(Comments,sep = ",") %>% 
  mutate(Comments = if_else(Comments == "NOTIME" |
                            Comments == "NODEPT" |
                            Comments == "LOSINV"|
                            Comments == "LOSFIS"|
                            Comments == "RELESD"|
                            Comments == "NOPEVL",
                                  true = paste(Comments),
                                  false = "Comments Error"),
         Depth = as.numeric(Depth),
         Depth = if_else(Depth >= 0 & Depth <= 100,
                          true = if_else(is.na(Depth),
                                         true = NA_character_,
                                         false = paste(Depth)),
                          false = "Depth Error"),
         `Total Cover` = as.numeric(`Total Cover`),
         `Total Cover` = if_else(`Total Cover` >= 0 & `Total Cover` <= 100,
                         true = if_else(is.na(`Total Cover`),
                                        true = NA_character_,
                                        false = paste(`Total Cover`)),
                         false = "Total Cover Error"),
         `Peri Cover` = as.numeric(`Peri Cover`),
         `Peri Cover` = if_else(`Peri Cover` >= 0 & `Peri Cover` <= 100,
                         true = if_else(is.na(`Peri Cover`),
                                        true = NA_character_,
                                        false = paste(`Peri Cover`)),
                         false = "Peri Cover Error"),
         `Peri Volume` = as.numeric(`Peri Volume`),
         `Peri Volume` = if_else(`Peri Volume` >= 0 & `Peri Volume` < 50000,
                         true = if_else(is.na(`Peri Volume`),
                                        true = NA_character_,
                                        false = paste(`Peri Volume`)),
                         false = "Peri Volume Error"))

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

QC_PHYS %>% 
  filter(Depth == "Depth Error")

#same thing as above but for "Comments Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Comments == "Comments Error")

#same thing as above but for "Length Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(Depth == "Depth Error")

#same thing as above but for "Length Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(`Total Cover` == "Total Cover Error")

#same thing as above but for "Length Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(`Peri Cover` == "Peri Cover Error")
#same thing as above but for "Length Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_PHYS %>% 
  filter(`Peri Volume` == "Peri Volume Error")

# Annotate ERRORS and changes from the data here:
# LOSFIS misspelled as LOSFFIS

#---------------------------------
#prepare for data merging####
#---------------------------------

#remove any comments that refer to a single speciment and not an entire cup (e.g., gravid, partmis, etc. )

QC_PHYS <- QC_PHYS %>% 
  mutate(Comments = if_else(Comments == "ROTCUP"|Comments == "EMPCUP",
                            true = paste(Comments),
                            false = NA_character_)) %>% 
  select(-`Checked By`,-`Entered By`)

table(is.na(QC_PHYS$Comments)) #no rotcups or empcups in the set means all should be NA (i.e., code spits out all "trues")

QC_PHYS %>% 
  write_csv(file = "M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_phys_2022.csv")
