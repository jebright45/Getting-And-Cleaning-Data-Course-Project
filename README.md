COURSE PROJECT README 

Using data from Human Activity Recognition Using Smartphones Dataset Version 1.0, followed course instructions to pull together data gathered by smartphones. (Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012).

First, in the opening section entitled "Putting the train data together in one data frame," I combined the training data together. The training data was made up of X_train, y_train, and subject_train. These files had data from the movement recordings, activities, and subject ids respectively. 

Next, I performed the same process in the section entitled "Putting the test data together in one data frame," but this time for the test data.

Then, I combined the test and train data together in the section called "Putting the test and train data sets together to form one new data set." I knew I needed to have a data set with rows for 30 subjects times 6 activities for each subject, so that came to a total of 180 rows. The number of columns should remain at 81 because we should have a column for the subject, the activity, and all the features. So, what what I had to first split activitydataset4 by subject to get 30 different subsets for each respondent. Then, split within those splits by activity. At first, I tried using sapply and the by() function to try calculating the mean for each activity across all the columns. But since that didn't seem to be working very well, I then went back to Course 2 R Programming Week 3 to reference videos on how to use split and found some good stuff on splitting when you have multiple factors via the interaction() function.

So, after splitting by the interaction of activity labels and subject ids, I used sapply() and an anonymous function to calculate column means for each feature. This command put features as row names, so I then used t() to transpose the matrix to get the features back as the column names. After that, I added two columns with levels for subjectids and actvities to get a finished data set I called activitydataset8. Lastly, I went back to the activitydataset4 and calculated means on various columns for particular subjectid, activities, and features to compare back to my activitydataset8. At first I wasn't getting the means to match up between activitydataset4 and activitydataset8 for the corresponding subject, activity, and column but after having the wind totally taken out of my sails haha I was scrolling back through activitydataset4 and realized that the values I originally used to calculate the mean from activitydataset4 were incomplete. Ie. I only used rows 1:27 of activitydataset4 because I thought that that was all the cases of subject 1 standing. That gave me a value of 0.2792155, which didn't match 0.2789176 from activitydataset8 for subject 1, activity standing, column tBodyAcc-mean()-X. But after going back and correctly subsetting from activitydataset4 (which can now be seen above) for all the values corresponding to subject 1, activity standing, column tBodyAcc-mean()-X, I got the means to match. 

The very last step was to write code using write.table() that created a tidy data text file with the final data set.
