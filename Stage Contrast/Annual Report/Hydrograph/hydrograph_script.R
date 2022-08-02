rm(list = ls())

####Load libraries (must be installed before loading)####

library(tidyverse)    #contains a whole host of packages including ggplot and dplyr
library(lubridate)    #package with in tidyverse used for working with dates

###Load in Data####

##Line 16 -> loads in data from csv skipping the first 6 rows
            #note number of rows to skip depends on the number of stations
##Line 17 -> renames the Station as Cell
##Line 18 -> renames 'Daily Date' as Date
##Line 19 -> renames 'Data Value' as Depth.ft

depth.raw <- read_csv("M:/LILA/Pomacea/FAU/Barrus_Data/Hydrograph/LilaDepths_06-01-2018--07-28-2022.csv", skip = 6) %>% 
  rename(Cell = Station,
         Date = 'Daily Date',
         Depth.ft = 'Data Value') 

####Data Wrangling of raw data####

##Line 32 -> Identify our Deep Slough Depth in feet which is 13.5 Feet
##Line 34 -> start with depth.raw data and save all changes into depth.raw data
##Line 35 -> use only the three columns we need which are Cell, Date, Depth.ft
##Line 36-42 -> Change the names from Station IDs to wetland IDs
##Line 43 -> Create a new Depth variable converting Depths to cm above the deep slough
##Line 44-46 -> Create a new variable that identifies which wetland is in which treatment
##Line 47 -> covert the Date variable to a format that R will recognize as a date
##Line 48-50 -> takes our dates and groups them into weeks starting on mondays

DSDepth <- 13.5

depth.raw <- depth.raw %>%  
  select(Cell, Date, Depth.ft) %>% 
  mutate(Cell = if_else(Cell == "LILA1O",
                        true = "M1",
                        false = if_else(Cell == "LILA2O",
                                        true = "M2",
                                        false = if_else(Cell == "LILA3O",
                                                        true = "M3",
                                                        false = "M4"))),
         depth.cm = (Depth.ft - DSDepth)*12*2.54,
         treatment = if_else(Cell == "M1" | Cell == "M3",
                             true = "unconstrained",
                             false = "constrained"),
         Date = dmy(Date),
         week.date = ceiling_date(Date,
                                  unit = "week",
                                  week_start  = getOption("lubridate.week.start", 1)))

#### Plot the Depths through time to create our Hydrograph #####

##Line 70 -> use our depth.raw data
##Line 71 -> remove missing values
##Line 72-73 -> change date and week.date to a as.POSIXct format for plotting
##Line 74 -> identify the weeks and treatments as our grouping variables
##Line 75 -> finds average depths for each week and treatment
##Line 76 -> start our plot of weekly depths, using treatments as a grouping color
##Line 77 -> set the theme of the plot to make it white backgroud
##Line 78 -> plot the water depths as lines with a size of 0.75
##Line 79 -> give our Water Depth label but remove our date label
##Line 80 -> format x axis text to angle them by 60 degrees
##Line 81 -> set the title text size
##Line 82 -> set the axis titles sizes
##Line 83 -> set the legend title sizes
##Line 84 -> format the axis so each break occurs at 1 month formatted by month-year
##Line 85 -> select the specific colors to represent each treatment

depth.sum <- depth.raw %>% 
  drop_na() %>% 
  mutate(Date = as.POSIXct(Date),
         week.date = as.POSIXct(week.date)) %>% 
  group_by(week.date, treatment) %>% 
  summarise(depth.cm = mean(depth.cm, na.rm = T)) 

depth.raw %>% 
  drop_na() %>% 
  ggplot(aes(x = as.POSIXct(Date), y = depth.cm, color = treatment))+
  theme_classic()+
  geom_line(size = 0.75)+
  labs(y = "Water Depth in Deep Slough (cm)", x = NULL)+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        title = element_text(size = 36),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))+
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b-%y")+
  scale_color_grey(start = 0.8, end = 0.2)

      ###note you can change the number of breaks on the x axis using Line 84 by changing
      ###"1 month" to "x months" where the labels are given every x number of months
      ###this can be changed to "x year" if you wanted to separate them by years

ggsave("M:/LILA/Stage Contrast Study/Annual reports/2022 Annual Report/Figures & Tables/hydrograph.png",
       last_plot(), device = png, units = "in", width = 10, height = 5)
