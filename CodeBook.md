COURSE PROJECT CodeBook 

Using data from Human Activity Recognition Using Smartphones Dataset Version 1.0, I followed course instructions to pull together data gathered by smartphones. (Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012).

My Project repository will contain the following files:
- 'README.md'
- 'CodeBook.md'
- 'run_analysis.R'
- 'tidydata.txt'

Feature Selection (as described in course project)

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. (Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012).

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). (Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012).

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). (Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012).

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

The set of variables that were estimated from these signals are: mean(): Mean value and std(): Standard deviation. I chose the following features from the full set provided because each had to do with calculations of mean or std. The features that included the word "mean" that I excluded were the 7 Angle calculations that inlcuded previous mean calculations but did not compute means themselves:
- subjectids
-	activitycodes
-	tBodyAcc.mean...X
-	tBodyAcc.mean...Y
-	tBodyAcc.mean...Z
-	tGravityAcc.mean...X
-	tGravityAcc.mean...Y
-	tGravityAcc.mean...Z
-	tBodyAccJerk.mean...X
-	tBodyAccJerk.mean...Y
-	tBodyAccJerk.mean...Z
-	tBodyGyro.mean...X
-	tBodyGyro.mean...Y
-	tBodyGyro.mean...Z
-	tBodyGyroJerk.mean...X
-	tBodyGyroJerk.mean...Y
-	tBodyGyroJerk.mean...Z
- tBodyAccMag.mean..
-	tGravityAccMag.mean..
-	tBodyAccJerkMag.mean..
-	tBodyGyroMag.mean..
-	tBodyGyroJerkMag.mean..
-	fBodyAcc.mean...X
-	fBodyAcc.mean...Y
-	fBodyAcc.mean...Z
-	fBodyAcc.meanFreq...X
-	fBodyAcc.meanFreq...Y
-	fBodyAcc.meanFreq...Z
-	fBodyAccJerk.mean...X
-	fBodyAccJerk.mean...Y
-	fBodyAccJerk.mean...Z
- fBodyAccJerk.meanFreq...X
- fBodyAccJerk.meanFreq...Y
-	fBodyAccJerk.meanFreq...Z
-	fBodyGyro.mean...X
-	fBodyGyro.mean...Y
-	fBodyGyro.mean...Z
-	fBodyGyro.meanFreq...X
-	fBodyGyro.meanFreq...Y
-	fBodyGyro.meanFreq...Z
-	fBodyAccMag.mean..
-	fBodyAccMag.meanFreq..
-	fBodyBodyAccJerkMag.mean..
-	fBodyBodyAccJerkMag.meanFreq..
-	fBodyBodyGyroMag.mean..
-	fBodyBodyGyroMag.meanFreq..
-	fBodyBodyGyroJerkMag.mean..
-	fBodyBodyGyroJerkMag.meanFreq..
-	tBodyAcc.std...X
-	tBodyAcc.std...Y
-	tBodyAcc.std...Z
-	tGravityAcc.std...X
-	tGravityAcc.std...Y
-	tGravityAcc.std...Z
-	tBodyAccJerk.std...X
-	tBodyAccJerk.std...Y
-	tBodyAccJerk.std...Z
-	tBodyGyro.std...X
-	tBodyGyro.std...Y
-	tBodyGyro.std...Z
-	tBodyGyroJerk.std...X
-	tBodyGyroJerk.std...Y
-	tBodyGyroJerk.std...Z
-	tBodyAccMag.std..
-	tGravityAccMag.std..
-	tBodyAccJerkMag.std..
-	tBodyGyroMag.std..
-	tBodyGyroJerkMag.std..
-	fBodyAcc.std...X
-	fBodyAcc.std...Y
-	fBodyAcc.std...Z
-	fBodyAccJerk.std...X
-	fBodyAccJerk.std...Y
-	fBodyAccJerk.std...Z
-	fBodyGyro.std...X
-	fBodyGyro.std...Y
-	fBodyGyro.std...Z
- fBodyAccMag.std..
- fBodyBodyAccJerkMag.std..
- fBodyBodyGyroMag.std..
- fBodyBodyGyroJerkMag.std..

Description of project and how data was gathered from the README of:
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

