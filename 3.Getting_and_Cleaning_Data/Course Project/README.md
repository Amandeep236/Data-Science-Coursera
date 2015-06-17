
# Introduction

The data involves in a experiment about human activity recognition. 30 subjects participate in the experiment. During the process, they wear a smartphone on the waist and take six activities, namely walking, walking upstairs, walking downstairs, sitting, standing and laying. The smartphone records data by using its embedded accelerometer and gyroscope when subjects precess activities. The purpose of the project is to clean the raw data and prepare a tidy dataset for later analysis. The processes are following.      


### Raw Dataset    
Raw data for smartphone record can be downloaded from the link.    
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip     


### Relevant tables    

 - Train file :    
1.X_train.txt': Training data set contains **70% of subjects**.    
2.y_train.txt': The label number in training data set means six activities.    
3.subject_train.txt : Subject id for training data set.    

 - Test file :     
1.X_test.txt': Test data set contains **30% of subjects**.    
2.y_test.txt': The label number in test data set means six activities.    
3.subject_test.txt : Subject id for test data set. 

 - features.txt :     
It contains 561 variables.    

 - activity_labels.txt:    
Links the class labels with their activity name.    

### Fianl Tidy Dataset      

 - The R code for cleaning raw data is shown in **"run_analysis.R"** script.    
 - The steps for processing data are following :    
1.Load relevant data mentioned above into R.    
2.Merge train data and test data to individual dataset.    
3.Subset variables only containing mean and std(standard deviation).        
4.Use descriptive activity names to name six activities in activity table.         
5.Combind subject table, activity table and mean_std table into one dataset.        
6.Create a second, independent tidy dataset with the average of each variable for each activity and each subject.        


### Code Book    
Code book records corresponding meanings of each variable for the tidy data.            

