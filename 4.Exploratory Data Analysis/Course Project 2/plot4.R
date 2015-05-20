

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
##Question 4
##-----------

## Across the United States, 
## how have emissions from coal combustion-related sources changed from 1999-2008?


# Filter data in SCC table which data includes "Coal"
Coal <- SCC %>%
    mutate(Match = ifelse(grepl("Coal",EI.Sector,ignore.case = T),"Yes","NO")) %>%
    filter(Match == "Yes")

# Subset data in NEI table which includes SCC in SCC table
Coal_related <- NEI[NEI$SCC %in% Coal$SCC,]


# Calculate total emissions from PM2.5 between 1999 and 2008.
Emission <- Coal_related %>%
    mutate(year = as.factor(year)) %>%
    group_by(year) %>%
    summarise(Sum_Emission = round(sum(Emissions))) %>%
    data.frame()

# Plot the result
png("plot4.png",width = 620, height = 620)

ggplot(Emission, aes(x = year, y = Sum_Emission, fill = year, ymax = max(650000))) +
    geom_bar(stat = "identity", width = 0.7) +
    scale_fill_brewer() +
    theme_bw() +
    geom_text(aes(label = Sum_Emission), vjust = -1, position = position_dodge(width = 0.9)) +
    ylab("Total Emissions from PM2.5") +
    xlab("Year") +
    ggtitle("Emissions from Coal Combustion-Related Sources Across the United States") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 14)) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
    theme(axis.title.y = element_text(face = "bold", size=12)) +
    theme(axis.title.x = element_text(face = "bold", size=12)) +
    theme(axis.text.x = element_text(size = 14))

dev.off()






# Calculate total emissions from PM2.5 between 1999 and 2008 with types.
Emission.1 <- Coal_related %>%
    mutate(year = as.factor(year)) %>%
    group_by(year, type) %>%
    summarise(Sum_Emission = round(sum(Emissions))) %>%
    data.frame()


# Plot the result with types
png("plot4.1.png",width = 620, height = 620)

ggplot(Emission.1, aes(x = year, y = Sum_Emission, fill = type, ymax = max(650000))) +
    geom_bar(position="dodge", stat = "identity", width = 0.9) +
    scale_fill_brewer() +
    theme_bw() +
    geom_text(aes(label = Sum_Emission), vjust = -1, position = position_dodge(width = 0.9)) +
    ylab("Total Emissions from PM2.5") +
    xlab("Year") +
    ggtitle("Emissions from Coal Combustion-Related Sources Across the United States") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 14)) +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
    theme(axis.title.y = element_text(face = "bold", size=12)) +
    theme(axis.title.x = element_text(face = "bold", size=12)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(legend.position = "top")

dev.off()


