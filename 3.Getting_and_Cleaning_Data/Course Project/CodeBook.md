

# Code Book   

In the tidy data set, the definitions of variables are following. They are from "feature_info.txt" in the original "UCI HAR Dataset" file.         


1. **Subject id :**    
Number 1 to number 30 mean 30 subjects who participate in the experiment.    

2. **Activity :**    
Each subject performes six activities when wearing a smartphone.    

 - Walking    
 - Walking upstairs    
 - Walking downstairs    
 - Sitting    
 - Standing    
 - Laying
 
 
3. Variables contain **X, Y or X.**    
It is used to denote 3-axial signals in the X, Y and Z directions.    
    
4. Variables contain **Gyro or ACC.**    
The smartphone embeds two sensor signals for recording the results of activities, **accelerometer** and **gyroscope** respectively.     

5. Variables contain **Time_Acc_XYZ and Time_Gyro_XYZ.**         
These time domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise.    

6. Variables contain Time_**Body**Acc_XYZ and Time_**Gravity**Acc_XYZ.    
The acceleration signal was separated into **body** and **gravity** acceleration signals using another low pass Butterworth filter with a corner frequency of 0.3 Hz.     

7. Time_BodyAcc**Jerk**_XYZ and Time_BodyGyro**Jerk**_XYZ.     
The body linear acceleration and angular velocity were derived in time to obtain **Jerk signals**.    

       
8. Time_BodyAcc**Mag**, Time_GravityAcc**Mag**, Time_BodyAccJerk**Mag**, Time_BodyGyro**Mag** and Time_BodyGyroJerk**Mag**.    
The magnitude of these three-dimensional signals were calculated using the Euclidean norm.    

9. **Frequency**_BodyAcc-XYZ, **Frequency**_BodyAccJerk-XYZ, **Frequency**_BodyGyro-XYZ, **Frequency**_BodyAccJerkMag, **Frequency**_BodyGyroMag and **Frequency**_BodyGyroJerkMag.    
A Fast Fourier Transform (FFT) was applied to some of these signals producing these variables.    

10. Variables contain **mean**: Mean value.    

11. Variables contain **std**: Standard deviation.    
