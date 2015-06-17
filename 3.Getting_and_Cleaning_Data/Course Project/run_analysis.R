




rm(list = ls())
setwd("C:/Users/Cindy/Desktop/R/UCI HAR Dataset")
library(dplyr)


##------------------
## Load data into R
##------------------

### Train data
X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
sub_train <- read.table("./train/subject_train.txt")

### Test data
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
sub_test <- read.table("./test/subject_test.txt")



##-----------------------------------------------------------------
## 1.Merges the training and the test sets to create one data set.
##-----------------------------------------------------------------

x_data <- rbind(X_train,X_test)
y_label <- rbind(y_train,y_test)
subject <- rbind(sub_train,sub_test)



##-------------------------------------------------------------------------------------------
## 2.Extracts only the measurements on the mean and standard deviation for each measurement.
##-------------------------------------------------------------------------------------------

### Load "features" dataset
features <- read.table("./features.txt")

### Extract data only including "mean" and "std".
features_data <- grep("mean\\(\\)|std\\(\\)", features[,2])

### Check how many variables
length(features_data) # 66 variables

### Subset data columns only including "mean" and "std".
data_mean_std <- x_data[,features_data]

### Subset variable name only including "mean" and "std".
var <- features[features_data,]

names(var) <- c("ID", "Variable")



##-----------------------------------------------------------------------
## 4. Appropriately labels the data set with descriptive variable names.
##-----------------------------------------------------------------------
var <- var %>%
  mutate(Variable = gsub("-mean\\(\\)","_mean", Variable),
         Variable = gsub("-std\\(\\)","_std", Variable),
         Variable = gsub("-","_", Variable),
         Variable = gsub("^t","Time_", Variable),
         Variable = gsub("^f","Frequency_", Variable),
         Variable = gsub("BodyBody","Body", Variable))

### Name columns
names(data_mean_std) <- var[,2]


##---------------------------------------------------------------------------
## 3.Uses descriptive activity names to name the activities in the data set.
##---------------------------------------------------------------------------

### Load "activity" dataset
activity_labels <- read.table("./activity_labels.txt")

### Rename appropriate column name.
names(activity_labels) <- c("ID", "Activity")
names(y_label)[1] <- "ID"
names(subject)[1] <- "Subject_Id"

### Replace with descriptive activity names.
Activity <- y_label %>%
  left_join(activity_labels, by = "ID") %>%
  select(Activity)

### Create a complete dataset including subject, activity and records.  
All_data <- cbind(subject,Activity,data_mean_std)


##--------------------------------------------------------------------------------
## 5.From the data set in step 4, creates a second, independent tidy data set with 
## the average of each variable for each activity and each subject.
##--------------------------------------------------------------------------------

tidy_data <- All_data %>%
  group_by(Subject_Id, Activity) %>%
  summarise_each(funs(mean))

write.table(tidy_data, "tidy_data.txt", row.name = FALSE, sep="\t")



