
###------------------------
### 4.Exploratory Data Analysis
### Project 1
###------------------------


## Download data from Internet
## Source : http://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data

temp <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00235/household_power_consumption.zip",temp)
data <- read.csv(unz(temp, "household_power_consumption.txt"), sep=";")
unlink(temp)



## Load Packages
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)



## Check Data
str(data)
dim(data)


## Calculate Memory

2075259*9*8

149418648 / (2^20)
# = 142.4967 MB


## Data Wrangling

# Format Date and subset data
Ver <- data

NewDate <- Ver %>% 
  mutate( Date = as.character(Date),
          OK = gsub("/", "-" , Date),
          sample_date = as.Date(OK, format("%d-%m-%Y")),
          Date_Time = paste(sample_date,Time, sep=" ")) %>%
  filter(sample_date == '2007-02-02' | sample_date == '2007-02-01')

NewDate$Date_Time_New <- strptime(NewDate$Date_Time,format = "%Y-%m-%d %H:%M:%S")


#Check how many observations
table(NewDate$sample_date)

#2007-02-01 2007-02-02 
#1440       1440

# Check class for time and date
class(NewDate$Date_Time_New)

# Write data out and read again to transfer data type
write.csv(NewDate,"C:/Users/Cindy/Documents/CourseData.csv")

mydata <- read.csv("CourseData.csv") # read data from work directory 

mydata$Date_Time_New <- strptime(mydata$Date_Time,format = "%Y-%m-%d %H:%M:%S")

# Check missing value
sum(is.na(mydata)) #0


## Plot 2
png(filename="plot2.png", width=480, height=480)
print(plot(mydata$Date_Time_New, 
           mydata$Global_active_power, 
           type = "l",
           ylab = "Global Active Power (kilowatts)",
           xlab = " "))
dev.off()



