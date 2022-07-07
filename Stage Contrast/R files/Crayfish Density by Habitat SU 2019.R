### SFWMD Annual report 2019 Graphs ###

### Crayfsih Density by Habitat Wet Season 2019 ###

## read in data set ##  

Crayfish_count_Data <- read.csv("/Volumes/JSOMMER FAU/Jeff's Files/
                                Crayfish Research/SFWMD annual Report 2019/
                                Crayfish_count_data_su_2019.csv") 

## Separate Crayfish Data into separate objects per macrocosm ##
Crayfish_count_Data_1 <- Crayfish_count_Data[which(Crayfish_count_Data$Macrocosm=="1"), ]
Crayfish_count_Data_2 <- Crayfish_count_Data[which(Crayfish_count_Data$Macrocosm=="2"), ]
Crayfish_count_Data_3 <- Crayfish_count_Data[which(Crayfish_count_Data$Macrocosm=="3"), ]
Crayfish_count_Data_4 <- Crayfish_count_Data[which(Crayfish_count_Data$Macrocosm=="4"), ]

## Generate Stripcharts for each macrocosm ##
# open ggplot2 library #
library(ggplot2)
#Generate Stripchart for M1#
CD_SU19_M1 <- ggplot(Crayfish_count_Data_1, aes(x = Location, y = profal)) + geom_jitter(width = 0.33, height = 0.33)
#Generate Stripchart for M2#
CD_SU19_M2 <- ggplot(Crayfish_count_Data_2, aes(x = Location, y = profal)) + geom_jitter(width = 0.33, height = 0.33)
#Generate Stripchart for M3#
CD_SU19_M3 <- ggplot(Crayfish_count_Data_3, aes(x = Location, y = profal)) + geom_jitter(width = 0.33, height = 0.33)
#Generate Stripchart for M4#
CD_SU19_M4 <- ggplot(Crayfish_count_Data_4, aes(x = Location, y = profal)) + geom_jitter(width = 0.33, height = 0.33)
##stripchat with mean and standard deviation ##

P1 <- CD_SU19_M1 + stat_summary(fun.data = "mean_sdl", geom = "pointrange") + theme_classic() + ggtitle("M1") + xlab("Habitat") + ylab("# of Crayfish")
P2 <- CD_SU19_M2 + stat_summary(fun.data = "mean_sdl", geom = "pointrange") + theme_classic() + ggtitle("M2") + xlab("Habitat") + ylab("# of Crayfish")
P3 <- CD_SU19_M3 + stat_summary(fun.data = "mean_sdl", geom = "pointrange") + theme_classic() + ggtitle("M3") + xlab("Habitat") + ylab("# of Crayfish")
P4 <- CD_SU19_M4 + stat_summary(fun.data = "mean_sdl", geom = "pointrange") + theme_classic() + ggtitle("M4") + xlab("Habitat") + ylab("# of Crayfish")


##stripchat with mean and standard error ##
P5 <- CD_SU19_M1 + stat_summary(fun.data = mean_se, geom = "pointrange") + theme_classic() + ggtitle("M1") + xlab("Habitat") + ylab("# of Crayfish")
P6 <- CD_SU19_M2 + stat_summary(fun.data = mean_se, geom = "pointrange") + theme_classic() + ggtitle("M2") + xlab("Habitat") + ylab("# of Crayfish")
P7 <- CD_SU19_M3 + stat_summary(fun.data = mean_se, geom = "pointrange") + theme_classic() + ggtitle("M3") + xlab("Habitat") + ylab("# of Crayfish")
P8 <- CD_SU19_M4 + stat_summary(fun.data = mean_se, geom = "pointrange") + theme_classic() + ggtitle("M4") + xlab("Habitat") + ylab("# of Crayfish")

## Adjust shape of mean by adding "shape = #" to stat_summary code line ##
# plots with mean and StDev
P1 <- CD_SU19_M1 + stat_summary(fun.data = "mean_sdl", geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M1") + xlab("Habitat") + ylab("")
P2 <- CD_SU19_M2 + stat_summary(fun.data = "mean_sdl", geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M2") + xlab("Habitat") + ylab("")
P3 <- CD_SU19_M3 + stat_summary(fun.data = "mean_sdl", geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M3") + xlab("Habitat") + ylab("")
P4 <- CD_SU19_M4 + stat_summary(fun.data = "mean_sdl", geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M4") + xlab("Habitat") + ylab("")
# plots with mean and SE
P5 <- CD_SU19_M1 + stat_summary(fun.data = mean_se, geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M1") + xlab("Habitat") + ylab("")
P6 <- CD_SU19_M2 + stat_summary(fun.data = mean_se, geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M2") + xlab("Habitat") + ylab("")
P7 <- CD_SU19_M3 + stat_summary(fun.data = mean_se, geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M3") + xlab("Habitat") + ylab("")
P8 <- CD_SU19_M4 + stat_summary(fun.data = mean_se, geom = "pointrange", shape = 3) + theme_classic() + ggtitle("M4") + xlab("Habitat") + ylab("")
## Combine individual graphs into one figure ##
library(gridExtra)
# Make a 4 plot figure: with mean and stdev ##
grid.arrange(P1, P2, P3, P4, nrow = 2, ncol = 2)

# make a 4 plot figure: with mean and SE #
grid.arrange(P5, P6, P7, P8, nrow = 2, ncol = 2)

## Anova of Crayfish Data by habitat (aka location) ##
## and wetland (aka macrocosm) ##
## as well as wetland * habitat ##

# Create full model of cryafish data with interaction term #
crayfish_Full_Model <- lm(profal ~ Macrocosm * Location, data = Crayfish_count_Data)

# run anova of full model #
anova(crayfish_Full_Model)

#Analysis of Variance Table#

Response: profal
Df  Sum Sq Mean Sq F value Pr(>F)
Macrocosm           1   2.627  2.6273  0.6956 0.4067
Location            2   3.685  1.8423  0.4878 0.6158
Macrocosm:Location  2   5.061  2.5307  0.6700 0.5145
Residuals          82 309.718  3.7770   

# Residual plot from full model #
plot(residuals(crayfish_Full_Model) ~ fitted(crayfish_Full_Model))
abline(0,0)

# normal quantile plot of residuals #
qqnorm(residuals(crayfish_Full_Model), pch = 16, col = "chartreuse",
       las = 1, ylab = "residuals", xlab = "Normal Quantile", main = "")

## Sqrt transform profal ##
Crayfish_count_Data$SQRTprofal <- sqrt(Crayfish_count_Data$profal + 1/2)

# create new linear model with square root transformed data #
crayfish_sqrtTransformed_Full_Model <- lm(SQRTprofal ~ Macrocosm * Location, data = Crayfish_count_Data)

# run anova of rayfish_sqrtTransformed_Full_Model #
anova(crayfish_sqrtTransformed_Full_Model)

# Residual plot from transformed full model #
plot(residuals(crayfish_sqrtTransformed_Full_Model) ~ fitted(crayfish_sqrtTransformed_Full_Model))
abline(0,0)

# normal quantile plot of residuals #
qqnorm(residuals(crayfish_sqrtTransformed_Full_Model), pch = 16, col = "chartreuse",
       las = 1, ylab = "residuals", xlab = "Normal Quantile", main = "")
