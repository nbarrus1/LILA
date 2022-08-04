#### LILA STAGE CONTRAST EXPERIMENT CRAYFISH IMPORT CHECK CODE 
#### Experiment started WET SEASON of 2018 while NJD was at FAU  
#### Experiment was managed at FAU From WET SEASON 2018 - WET SEASon 2021 
#### NJD moved to FIU FROM FAU FALL 2021. all aspects of data analysis and storage 
#### changed to TrexLAB formats to be compatible with all other projects in the LAB 
#### DATA QA/QC for all other projects in the lab are conducted using SAS, LILA QA/QC 
#### IS conducted in R. USING THE following code Processees.

### Code Created 8/3/2022-8/4/2022 by JS and NB

#### LILA DATA QA/QC process
#### STEP 1: IMPORT CHECK  (YOU ARE HERE) 
#### STEP 2: MERGE CHECK 
#### STEP 3: DATA MERGE 

#---------------------------------------------------------------------------------------
#########################*THIS PROGRAM RUNS BEFORE THE CRAY FILE#########################
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

# tidyverse includes the packages that you're likely to use in everyday data analyses
#packages within tidyverse 1.3.0 include: 
#GGPlot, dplyr, tidyr, readr, purrr, tibble, stringr and forcats

library(readxl)
#readxl allows excel files to be read into R

#### Import Cray file as an xlsx file, update file path every year/season 
cray_length_spring_2022 <- read_xlsx('M:/LILA/LILA Data Entry/2022/Throw Trapping/Spring/LILA_TT_2022_CRAY_SPRING.xlsx') 

#### 1st QAQC: Checking Session, Wetland, Year, Month, Day, Throw for Errors.
#note) when checking session, the session name will need to be replaced with
#the session that you're working with. Also) spaces and capitalization 
#matter in this name
#note 2) the QC process will change the data type from numeric to character 
#which makes sense because throw is categorical type variable

QC_cray <- cray_length_spring_2022 %>% 
  replace_with_na(replace = list(Session = ".",
                                 Wetland = ".",
                                 Year = ".",
                                 Month =".",
                                 Day=".",
                                 Throw=".",
                                 Species = ".",
                                 Length= ".",
                                 Sex= ".",
                                 Form= ".",
                                 Comments= ".")) %>% 
  mutate(Session = if_else(Session == "Spring 2022",
                                   true = paste(Session),
                                   false = "Session Error"),
         Wetland = if_else(Wetland == "M1" |
                           Wetland == "M2" |
                           Wetland == "M3" |
                           Wetland == "M4",
                                   true = paste(Wetland),
                                   false = "Wetland Error"),
         Year = if_else(Year == 2022,
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

#check to see if we have any errors. the following six lines of code should
#not give and "TRUES" in the R console (bottom left screen). If there are "TRUES", 
#Click on the QC_cray file in the global environment (top right screen) and the data
#will open. To find the Errors, click on the variable names and it will automatically sort
#the data for that variable

#FIX THESE ERRORS BEFORE MOVING FORWARD

table(QC_cray$Session == "Session Error") 
table(QC_cray$Wetland == "Wetland Error") 
table(QC_cray$Year == "Year Error")
table(QC_cray$Month == "Month Error")
table(QC_cray$Day== "Day Error")
table(QC_cray$Throw == "Throw Error")

# Annotate ERRORS and changes from the data here:
# none

#### 2nd QAQC: Checking Species, Length, SEX, FORM and COMMENT
#note) the QC process will change the data type from numeric to character 
#which makes sense because throw is categorical type variable

QC_cray <- cray_length_spring_2022 %>%
  replace_with_na(replace = list(Session = ".",
                                 Wetland = ".",
                                 Year = ".",
                                 Month =".",
                                 Day=".",
                                 Throw=".",
                                 Species = ".",
                                 Length= ".",
                                 Sex= ".",
                                 Form= ".",
                                 Comments= ".")) %>% 
  mutate(Species = if_else(Species == "NOCRAY" |
                           Species == "PROFAL" |
                           Species == "PROSPP" |
                           Species == "PROALL",
                                  true = paste(Species),
                                  false = "Species Error"),
         Sex = if_else(Sex == "1" |
                       Sex == "2" |
                       Sex == "3" |
                       Sex == "4",
                                  true = paste(Sex),
                                  false = "Sex Error"),
         Form = if_else(Form == "1" |
                        Form == "2" ,
                                  true = paste(Form),
                                  false = "Form Error"),
         Comments = if_else(Comments == "NOCRAY" |
                            Comments == "HELCOP" |
                            Comments == "NODATA" |
                            Comments == "ROTCUP"|
                            Comments == "SITDEE"|
                            Comments == "TRLDRY"|
                            Comments == "VEGTHK"|
                            Comments == "SITDRY"|
                            Comments == "GRAVID",
                                  true = paste(Comments),
                                  false = "Comments Error"),
         Length = as.numeric(Length),
         Length = if_else(Length >= 0 & Length < 40,
                          true = paste(Length),
                          false = "Length Error"))



#check to see if we have any errors. the following six lines of code should
#not give and "TRUES" in the R console (bottom left screen). If there are "TRUES", 
#Click on the QC_cray file in the global environment (top right screen) and the data
#will open. To find the Errors, click on the variable names and it will automatically sort
#the data for that variable

table(QC_cray$Species == "Species Error") 
table(QC_cray$Sex == "Sex Error") 
table(QC_cray$Form == "Form Error")
table(QC_cray$Comments == "Comments Error")
table(QC_cray$Length== "Length Error")

# Annotate ERRORS and changes from the data here:
# none
