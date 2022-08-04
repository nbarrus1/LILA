#### LILA STAGE CONTRAST EXPERIMENT CRAYFISH IMPORT CHECK CODE ####
#### Experiment started WET SEASON of 2018 while NJD was at FAU  ####
#### Experiment was managed at FAU From WET SEASON 2018 - WET SEASon 2021 ####
#### NJD moved to FIU FROM FAU FALL 2021. all aspects of data analysis and storage ####
#### changed to TrexLAB formats to be compatible with all other projects in the LAB ####
#### DATA QA/QC for all other projects in the lab are conducted using SAS, LILA QA/QC #### 
#### IS conducted in R. USING THE following code Processees.####

### Code Created 8/3/2022 by JS and NB

#### LILA DATA QA/QC process ####
#### STEP 1: IMPORT CHECK  (YOU ARE HERE) ####
#### STEP 2: MERGE CHECK ####
#### STEP 3: DATA MERGE ####

#########################*THIS PROGRAM RUNS BEFORE THE CRAY FILE#########################
  
# TO USE THIS PROGRAM:
  # REPLACE ________ COMMENTS WITH RELAVENT DATES AND FILE NAMES FOR THE CURRENT DATA. 
# RUN THE PROGRAM ONE STEP AT A TIME, NOT ALL AT ONCE
# READ COMMENTS AS YOU GO AND FOLLOW DIRECTIONS PROVIDED 
# WHEN QAQC IS COMPLETED, SAVE THE PROGRAM AS A NEW FILE SHOWING THE CURRENT YEAR

#########################################################################################

#### BEGIN IMPORT CHECK CODE HERE ####
#### Read in the libraries being used in the code ####   
library(tidyverse)
# tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: 
#GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats

library(dplyr)
#dplyr is a library which provides tools for eficiently manipulating datasets 

library(readxl)
#readxl allows excel files to be read into R

#### Import Cray file as an xlsx file, update file path every year/season ####
cray_length_spring_2022 <- read_xlsx('M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_CRAY_SPRING.xlsx') 
attach(cray_length_spring_2022)

#### 1st QAQC: Checking Session, Wetland, Year, Month, Day, Throw for Errors.

if (Wetland != c('M1', 'M2', 'M3', 'M4')) {print('Wetland Error')} 
if (Year != 2022) {print('Year Error')}
if (Month != 1:12) {print('Month Error')}
if (Day != 1:31) {print('Day Error')}
if (Throw != 1:14) {print('Throw Error')}

#If anything is wrong it will show up in the outputs below.
#FIX THESE ERRORS BEFORE MOVING FORWARD

# Annotate ERRORS from the data here:
# 


#### 2nd QAQC: Checking Species, Length, SEX, FORM and COMMENT
if (Species != C('.', 'NOCRAY', 'PROFAL', 'PROSPP', 'PROALL'))
if (Sex != C(".", 1, 2, 3, 4)) {print('Sex Error')}