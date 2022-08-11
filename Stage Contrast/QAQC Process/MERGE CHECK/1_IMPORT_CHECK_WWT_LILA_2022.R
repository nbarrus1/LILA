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
#data sets


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

#### Import all the QAQC files from the first round of the process

QC_cray <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_cray_length_2022.csv',
                   na = "NA") 
QC_fish <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_fish_length_2022.csv',
                   na = "NA") 
QC_invt <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_invt_count_2022.csv',
                   na = "NA") 
QC_phys <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_phys_2022.csv',
                   na = "NA") 
QC_veg <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_veg_2022.csv',
                    na = "NA") 
QC_wwt <- read_csv('M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_wwt_2022.csv',
                        na = "NA") 

#### 1st QAQC: Merge all the data together to create a file that documents where a "mismatch" has occurred.
#the mismatch will indicate where different spatial or temporal values

#check the QC_fish file by the QC_cray file
anti_join(QC_fish, QC_cray, by = c("Session","Year","Month","Day","Wetland","Throw"))

#check the QC_fish file by the QC_invt file

anti_join(QC_fish, QC_invt, by = c("Session","Year","Month","Day","Wetland","Throw"))

#check the QC_fish file by the QC_invt file

anti_join(QC_fish, QC_veg, by = c("Session","Year","Month","Day","Wetland","Throw"))

#check the QC_fish file by the QC_invt file
  
anti_join(QC_fish, QC_wwt, by = c("Session","Year","Month","Day","Wetland","Throw"))

# Annotate ERRORS and changes from the data here:
# QCwwt had a the wrong day (15 was changed to 16)

#### 2nd QAQC: Checking Species, Length, SEX, FORM and COMMENT
#note) the QC process will change the data type from numeric to character 
#which makes sense because throw is a categorical type variable

QC_WWT <- WWT %>%
  mutate(Category = if_else(Category == "F" |
                            Category == "S" |
                            Category == "C" |
                            Category == "M" |
                            Category == "OV" |
                            Category == "OI" ,
                                  true = Category,
                                  false = "Category Error"),
         Comments = as.character(Comments),
         Comments = if_else(Comments == "NOINVT" |
                            Comments == "NODATA" |
                            Comments == "ROTCUP"|
                            Comments == "PRTMIS"|
                            Comments == "EMPCUP"|
                            Comments == "GRAVID",
                                  true = Comments,
                                  false = "Comments Error"),
         Comments = if_else(Comments == "EMPCUP"& !is.na(`Wet Weight (g)`),
                            true = "Comments or Wet Weight Error",
                            false = Comments),
         `Wet Weight (g)` = as.numeric(`Wet Weight (g)`),
         `Wet Weight (g)` = if_else(`Wet Weight (g)` >= 0 & `Wet Weight (g)` < 30,
                          true = paste(`Wet Weight (g)`),
                          false = "Wet Weight Error"))

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

QC_WWT %>% 
  filter(Category == "Category Error")

#same thing as above but for "Comments Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_WWT %>% 
  filter(Comments == "Comments Error"| Comments == "Comments or Wet Weight Error")

#same thing as above but for "Length Errors"

###AFTER FIXING ANY ERRORS THE DATA NEEDS TO BE REENTERED AND THE CODE UP TO THIS POINT NEEDS 
###TO BE RERUN!!!!!

#when there are no errors the output in the r console will begin with "#A tibble: 0 x 15"

QC_WWT %>% 
  filter(`Wet Weight (g)` == "Wet Weight Error" )


# Annotate ERRORS and changes from the data here:
# none

#---------------------------------
#prepare for data merging####
#---------------------------------

#remove any comments that refer to a single speciment and not an entire cup (e.g., gravid, partmis, etc. )

QC_WWT <- WWT %>% 
  mutate(Comments = if_else(Comments == "ROTCUP"|Comments == "EMPCUP",
                            true = paste(Comments),
                            false = NA_character_)) %>% 
  select(-`Sorted By`,-`Checked By`,-`Entered By`) %>% 
  mutate(`Wet Weight (g)` = as.numeric(`Wet Weight (g)`),
         `Wet Weight (g)` = replace_na(`Wet Weight (g)`,0)) 

table(is.na(QC_WWT$Comments)) #no rotcups or empcups in the set means all should be NA (i.e., code spits out all "trues")

QC_WWT %>% 
  write_csv(file = "M:/LILA/LILA QAQC_data/2022/1_IMPORT_CHECK_R/LILA_wwt_2022.csv")
