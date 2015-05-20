


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
##Question 3
##-----------

## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
## which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
## Which have seen increases in emissions from 1999-2008? 
## Use the ggplot2 plotting system to make a plot answer this question.


# Calculate total emissions from PM2.5 in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008 with four types
Emission <- NEI %>%
    mutate(year = as.factor(year)) %>%
    filter(fips == "24510") %>%
    group_by(year, type) %>%
    summarise(Sum_Emission = sum(Emissions)) %>%
    data.frame()


# Plot the result
png("plot3.png",width = 620, height = 620)

ggplot(Emission, aes(x = year, y = Sum_Emission, fill = type)) + 
    geom_bar(position="dodge", stat = "identity", width = 0.9) +
    theme_bw() + 
    guides(fill = FALSE) +
    facet_grid(.~type) +
    theme(strip.text.x = element_text(size = 12, face = "bold")) +
    scale_y_continuous(breaks=pretty_breaks(n = 15)) +
    ylab("Emission") +
    xlab("Year") +
    ggtitle("Total Emissions From PM2.5 For Four types of sources from 1999 to 2008 in Baltimore City") + 
    theme(plot.title = element_text(lineheight = .8, face = "bold", size = 13)) +
    theme(axis.text.x = element_text(size = 12)) +
    guides(color = FALSE) +
    theme(axis.title.y = element_text(face = "bold", size=14)) +
    theme(axis.title.x = element_text(face = "bold", size=14))

dev.off()


# Conclusion :
# Total emissions from PM2.5 for "NONPOINT" and "ON_ROAD" decreased with years.
# Total emissions from PM2.5 for "POINT" and "NON_ROAD" Fluctuated with years.











