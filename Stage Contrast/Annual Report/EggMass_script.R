rm(list = ls())

####load libraries (must be installed before downloading)####

library(tidyverse)
library(readxl)
library(lubridate)
library(lemon)

#-------------------------------------
####load data####
#-------------------------------------

#load in the data collected from the Dorn Lab at FAU, and create the year, month and day variables for 
#use when merging the new data collected from the FIU Lab.

eggdata.master <- read_excel("M:/LILA/LILA Data Entry/eggmass_2018-2021/EggMassSurvey/EggMassData_v1.12_nb.xlsx",
                      sheet = 2) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date))

#load in the newer data collected from the FIU lab and then reformat for merging.
#note) this will need to be done by copying and pasting the code from lines 25-34
#for each additional year of surveys done in FIU.
#note 2) the North transect was renamed to be called T1 transect and South transect
#was renamed to be called the T2 transect (see metadata in the FAU data file "EggMassData_v1.12_nb.xlsx")
#note 3) the P. paludosa data for M4 is removed because Paludosa have been extirpated
#from Cell M4 (see code lines 39-41)


eggdata.2022 <- read_excel("M:/LILA/LILA Data Entry/2022/LILA_EMS_2022_EMC.xlsx",
                           sheet = 1) %>% 
  rename(Cell = Macrocosm,
         Notes = Comment) %>% 
  gather(`Pompal Ridge`,`Pompal Slough`,`Pommac Ridge`,`Pommac Slough`, key = "group", value = "Count") %>% 
  separate(group, sep = " ", c("Pomacea_Species","Habitat")) %>% 
  mutate(Pomacea_Species = if_else(Pomacea_Species == "Pompal", true = "Paludosa",
                                   false = "Maculata"),
         Transect = if_else(Transect == "NORTH", true = "T1",
                            false = "T2"),
         Cell = if_else(Cell == "M4" & Pomacea_Species == "Paludosa",true = "extirpated",
                        false = paste(Cell))) %>% 
  filter(Cell != "extirpated")

#merge the newer data collected from the FIU lab to the eggdata.master (the older FAU data)

eggdata.master <- eggdata.master %>% 
  bind_rows(eggdata.2022)

#check the data by year to see if the merge was successful 

table(eggdata.master$Year,eggdata.master$Transect)
table(eggdata.master$Year,eggdata.master$Cell)

#check the counts
hist(eggdata.master$Count)

#check for NAs
table(is.na(eggdata.master$Cell))
table(is.na(eggdata.master$Transect))
table(is.na(eggdata.master$Habitat))
table(is.na(eggdata.master$Pomacea_Species))
table(is.na(eggdata.master$Count))

#the merge looks good when there are equal numbers of transects T1 and T2 by year
#note) there were additional transects walked 2019 and 2021 to check for egg clutches
#in other habitats
#note 2) there should be no NA in other wards lines 57-61 should give all FALSE when run

#####add the Area data from the transects ##### 

#Manually create this dataframe since this info is in the Meta Data and the size of 
#the data is not large 
#note) See Meta data to corroborate the actual areas of each transect

transectarea <- tibble(Cell = c("M4","M4","M3","M3","M2","M2","M1","M1"),
                       Transect = c("T1","T2","T1","T2","T1","T2","T1","T2"),
                       Area_m= c(1408,1268,1368,1264,1340,1276,1320,1292)) %>% 
  group_by(Cell) %>% 
  summarise(tot.area.m = sum(Area_m),
            tot.area.ha = tot.area.m/10000)
#-------------------------------------------------------
####Egg Clutch Densities Time series####
#-------------------------------------------------------

#merge the transect area data to the count data, then calculate densities of egg masses
#per hectare on the transects. While renaming the data.frame to eggdensity

eggdensity <- eggdata.master %>% 
  select(-Notes) %>% 
  group_by(Year,Month,Day,Cell,Pomacea_Species) %>% 
  summarise(tot.eggs = sum(Count)) %>% 
  left_join(transectarea, by = "Cell") %>% 
  mutate(egg.ha = tot.eggs/tot.area.ha)%>% 
  ungroup() %>% 
  group_by(Year, Cell, Pomacea_Species) %>% 
  summarise(n = n(),
            ave.egg.ha = mean(egg.ha, na.rm = T),
            sd.egg.ha = sd(egg.ha, na.rm = T)) %>%
  mutate(se = sd.egg.ha/sqrt(n))

#create and assign the written out scientific names for Maculata and Paludosa for the plots

pom_labels <- c("Pomacea maculata", "Pomacea paludosa")
names(pom_labels) <- c("Maculata", "Paludosa")

#create the time series plot for the apple snail egg clutch densities 
#note) to specify the superscript in the y axis plot you need to use the function
#expression. When using the expression function the text does not need to be in quotes,
#spaces need to be coded as "~".  Google "customizing axis titles in R" to get a blog post
#on how to work with expressions if you have further questions.
#note 2) the labeller function will use our specified scientific names for the panel titles
#instead of the "Maculata" and "Paludosa" from lines 104 and 105

eggdensity %>% 
  ggplot(aes(x = Year, y = ave.egg.ha, shape = Cell, fill = Cell))+
  geom_line(size = 1)+
  geom_point(size = 3.5, color = "black")+
  facet_rep_grid(~Pomacea_Species, labeller = labeller(Pomacea_Species = pom_labels))+
  theme_classic()+
  scale_x_continuous(breaks = c(2019,2020,2021,2022))+
  labs(x = NULL, y = expression(Average~Clutch~Density~(ha^-1)))+
  scale_shape_manual(values = c(22,22,21,21))+
  scale_fill_manual(values = c("black","white","black","white"))+
  theme(strip.text = element_text(face = "italic",size = 14),
        legend.title = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text( angle = 60, vjust = 0.5),
        legend.text = element_text(size = 14))

#save the plot into our Figures & Tables folder for the 2022 Anual Report

ggsave("M:/LILA/Stage Contrast Study/Annual reports/2022 Annual Report/Figures & Tables/eggclutch_dens_timeseries.png",
       last_plot(), device = png, units = "in", width = 8, height = 4)
#--------------------------------------------------------------------
####Egg Clutch proportional composition bar graph time series####
#--------------------------------------------------------------------

#Manually read in the 2014 egg clutch counts given from N Dorn.  

egg_2014 <- tibble(Cell = rep(c("M1","M2","M3","M4"), times = 2),
                   Pomacea_Species = c(rep("Maculata", times = 4),
                                       rep("Paludosa", times = 4)),
                   Year = rep(2014, times = 8),
                   Count = c(7,8,1,26,38,33,32,12))

#merge the 2014 egg clutch counts and calculate the proportion of eggs each by species
#and rename the dataframe to eggprop

eggprop <- eggdata.master %>% 
  select(-Notes) %>% 
  group_by(Year,Cell,Pomacea_Species) %>% 
  summarise(Count = sum(Count)) %>% 
  bind_rows(egg_2014) %>% 
  mutate(tot.eggs = sum(Count),
         prop = Count/tot.eggs)

#create the bar graph time series of the species contribution (proportionally) to the total amount
#egg clutch in the surveys

eggprop %>% 
  ggplot(aes(x = as.character(Year), y = prop, fill = Pomacea_Species))+
  geom_bar(stat = "identity")+
  facet_rep_wrap(~Cell, repeat.tick.labels = TRUE)+
  theme_classic()+
  scale_fill_manual(values = c("black","#999999"), labels = c("P. maculata", "P. paludosa")) +
  labs(x = "Survey Year", y = "Clutch Proportion")+
  theme(strip.text = element_text(size = 14),
        legend.title = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text( angle = 60, vjust = 0.5),
        legend.text = element_text(size = 14,face = "italic"))

#save the plot

ggsave("M:/LILA/Stage Contrast Study/Annual reports/2022 Annual Report/Figures & Tables/eggclutch_prop_timeseries.png",
       last_plot(), device = png, units = "in", width = 8, height = 6)
