
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
##Question 6
##-----------

## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?


# Subset data including "Mobile" and identify four type vehicles.
Mobile <- SCC %>%
    mutate(Match = ifelse(grepl("Mobile", EI.Sector, ignore.case = T),"Yes","NO")) %>%
    filter(Match == "Yes") %>%
    data.frame() %>%
    mutate(On_Road_type = ifelse(grepl("Diesel Heavy Duty Vehicles", EI.Sector, ignore.case = T),"1.Diesel_Heavy Duty",
                                 ifelse(grepl("Diesel Light Duty Vehicles", EI.Sector, ignore.case = T),"2.Diesel_Light Duty",
                                        ifelse(grepl("Gasoline Heavy Duty Vehicles", EI.Sector, ignore.case = T),"3.Gasoline_Heavy Duty",
                                               ifelse(grepl("Gasoline Light Duty Vehicles", EI.Sector, ignore.case = T),"4.Gasoline_Light Duty",NA)))),
           SCC = as.character(SCC)) %>%
    filter(!is.na(On_Road_type)) %>%
    select(SCC, On_Road_type)



# Subset data in NEI table which includes SCC in SCC table
Motor_Related <- NEI[NEI$SCC %in% Mobile$SCC,]


# Subset data in NEI table which includes SCC in SCC table. Also, left join SCC table to include four type vehicles.
Motor_Related <- Motor_Related %>%
    left_join(Mobile, by = "SCC")


# Calculate total emissions from PM2.5 between 1999 and 2008 in Baltimore and in Los Angeles.
Emission <- Motor_Related %>%
    filter(fips == "24510" | fips == "06037") %>%
    mutate(year = as.factor(year),
           City = ifelse(fips == "24510", "Baltimore", "Los Angeles")) %>%
    group_by(year, City) %>%
    summarise(Sum_Emission = round(sum(Emissions))) %>%
    data.frame()


## Plot the result
png("plot6.png",width = 620, height = 620)

ggplot(Emission, aes(x = year, y = Sum_Emission, fill = City, ymax = max(5000))) +
    geom_bar(position="dodge", stat = "identity", width = 0.9) +
    scale_fill_brewer() +
    theme_bw() +
    geom_text(aes(label = Sum_Emission), vjust = -1, position = position_dodge(width = 0.9)) +
    ylab("Total Emissions from PM2.5") +
    xlab("Year") +
    ggtitle("Emissions from Motor Vehicle Sources in Baltimore City and Los Angeles County") + 
    theme(plot.title = element_text(lineheight=.8, face="bold", size = 14)) +
    theme(axis.title.y = element_text(face = "bold", size=12)) +
    theme(axis.title.x = element_text(face = "bold", size=12)) +
    theme(axis.text.x = element_text(size = 14)) +
    theme(legend.position = "top")

dev.off()


## Conclusion
## Emission decreased between 1999 and 2008 in in Baltimore City.
## Emission increased from 1999 to 2005 and decreased from 2005 to 2008 in  Los Angeles County.



