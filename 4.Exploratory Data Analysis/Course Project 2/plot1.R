


## Clear Work Environment
rm(list=ls())
getwd()

## Load Packages
library(dplyr)
library(ggplot2)
library(scales)

## Load Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



##-----------
##Question 1
##-----------

## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission 
## from all sources for each of the years 1999, 2002, 2005, and 2008.


# Calculate the number of emissions for each of the years 1999, 2002, 2005, and 2008.
Emission <- NEI %>%
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarise(Sum_Emission = sum(Emissions)) %>%
    data.frame()


# Plot the result by using "base plotting system".

png("plot1.png",width = 620, height = 620)

par(mar = c(4, 4, 3, 2))
plot(Emission$year, 
     Emission$Sum_Emission/1000000,
     type="n",
     xlab = "Year",
     ylab = "Emission",
     main = "Total Emissions From PM2.5 Decreased from 1999 to 2008",
     las = 1,
     yaxt="n",
     ylim = c(2800000/1000000,8000000/1000000),
     boxwex=0.01,
     cex.axis = 1)
pts <- pretty(Emission$Sum_Emission / 1000000)
axis(2, at = pts, labels = paste(pts, "M", sep = ""), las = 1)
points(Emission$year, Emission$Sum_Emission/1000000, pch = 19)
lines(Emission$year, Emission$Sum_Emission/1000000)
text(Emission$year, (Emission$Sum_Emission/1000000) + 600000/1000000, 
     labels = c("7,332,967", "5,635,780", "5,454,703", "3,464,206"), col = 4)

dev.off()


# Conclusion :
# Total emissions from PM2.5 decreased in the United States from 1999 to 2008.





