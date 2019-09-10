##  Course 3 Project  #################################################################################################################################################

#don't need to run this unzip line again
unzip(zipfile = "./getdata_projectfiles_UCI HAR Dataset.zip", exdir = "G:/My Drive/Coursera Data Science Specialization/Course 3_Getting and Cleaning Data/Final Project") 

setwd("G:/My Drive/Coursera Data Science Specialization/Course 3_Getting and Cleaning Data/Final Project/UCI HAR Dataset")


##  Putting the train data together in one data frame  ################################################################################################################

X_train <- read.table(file = "./train/X_train.txt")
View(X_train)
dim(X_train)

y_train <- read.table(file = "./train/y_train.txt")
dim(y_train)
View(y_train)

subject_train <- read.table(file = "./train/subject_train.txt")
dim(subject_train)
View(subject_train)

train <- subject_train                     # Renaming the subject_train matrix to form a new matrix to whiich we can bind the other columns of train data
train <- cbind(train, y_train)             # Binding the column of activity types (ie. y_train)
train <- cbind(train, X_train)             # Binding the columns of measurement data for each activity and respondent/subject
dim(train)                                 # Checking that the dimensions are what I thought they should be (7352 by 563)--they are.
View(train)

colnames(train)[1] <- "subjectid"          # Renaming the first column
colnames(train)[2] <- "activitycode"       # Renaming the second column



features <- read.table(file = "./features.txt")
dim(features)
View(features)

features <- features[,2]
View(features)
dim(features)
features

dim(train)
colnames(train)[3:563] <- features
View(train)                                    # Well, that didn't work. Instead of pasting the names from features it put the row numbers.

class(features)                                # My thought was that that was b/c the elements of features were factors as shown by this step. 
features <- as.character(features)             # So, I changed their class to characters and tried renaming train columns again.
class(features)
colnames(train)[3:563] <- features
View(train)                                    # And that worked, noice. 



##  Putting the test data together in one data frame  #################################################################################################################

X_test <- read.table(file = "./test/X_test.txt")
dim(X_test)

y_test <- read.table(file = "./test/y_test.txt")
dim(y_test)

subject_test <- read.table(file = "./test/subject_test.txt")
dim(subject_test)

test <- subject_test                      # Renaming the subject_train matrix to form a new matrix to whiich we can bind the other columns of train data
test <- cbind(test, y_test)               # Binding the column of activity types (ie. y_train)
test <- cbind(test, X_test)               # Binding the columns of measurement data for each activity and respondent/subject
dim(test)                                 # Checking that the dimensions are what I thought they should be (7352 by 563)--they are.
View(test)

colnames(test)[1] <- "subjectid"          # Renaming the first column
colnames(test)[2] <- "activitycode"       # Renaming the second column



features <- read.table(file = "./features.txt")
dim(features)
View(features)

features <- features[,2]
View(features)
dim(features)

dim(test)
features <- as.character(features)             # Was able to skip a few of the trial and error steps I needed when figuring out how to make this work for... 
class(features)                                # the train data. Namely, making features characters from the start. 
colnames(test)[3:563] <- features
View(test)                                     # And it worked, noice.



###   Putting the test and train data sets together to form one new data set  #########################################################################################

activitydataset <- rbind(train, test)          # Creating one data set by merging the training and test data sets.
dim(activitydataset)



subsetactivitydataset <- activitydataset[ , grepl("[Mm]ean", names(activitydataset))] # This isn't quite it yet. Some of the times "mean" is mentioned its imputing...
dim(subsetactivitydataset)                                                            # previously calculated mean values into another calculation (eg. angle), so...
View(subsetactivitydataset)                                                           # those types of mentions shouldn't be included.

activitydataset2 <- activitydataset[,1:2]
View(activitydataset2)
activitydataset2 <- cbind(activitydataset2, subsetactivitydataset)
dim(activitydataset2)
View(activitydataset2)
names(activitydataset2)

activitydataset3 <- activitydataset2[,1:48]        # Excluding columns 49:55 of activitydataset2 because they were not calculations of mean, but rather...
dim(activitydataset3)                              # but rather calculations of angle that used previous mean calculations as inputs.
View(activitydataset3)

stdactivitydataset <- activitydataset[ , grepl("[Ss]td", names(activitydataset))]
dim(stdactivitydataset)                            # Unlike with the means, all the std that grepl found are actually std calculations so we'll keep 'em all.

activitydataset4 <- cbind(activitydataset3, stdactivitydataset)
dim(activitydataset4)                          # There she is. The final activity data set with train and test data that involve mean and std calculations.
View(activitydataset4)

class(activitydataset4$activitycode)           # Now getting ready to rename the activity column levels to be more descriptive. First step, check their class.
activitydataset4$activitycode <- as.factor(activitydataset4$activitycode)
class(activitydataset4$activitycode)           # Looks good to go. It's a factor now, instead of an integer variable.

levels(activitydataset4$activitycode) <- c("walking", "walkingupstairs", "walkingdownstairs", "sitting", "standing", "laying")
levels(activitydataset4$activitycode)
View(activitydataset4)                  # Looks good. The levels seem to have overlayed correctly.



###  Now Taking the last iteration of the data set and caluclating averages for each subject in each column  #########################################################

# Thinking through what I need to do here. So, I need to have a data set with rows for 30 subjects times 6 activities for each subject. That comes to a total of...
# 180 rows. The number of columns should be 81 still because we have a column for the subject, the activity, and all the features. So, what I have to do is...
# first split activitydataset4 by subject to get 30 different subsets for each respondent. Then, split within by activity or maybe just use sapply with the...
# by() function perhaps to calculate the mean for each activity across all the columns.

# First tried this, but couldn't figure out how to get it to work, so... 

activitydataset5 <- split(x = activitydataset4, f = activitydataset4$subjectid)
View(activitydataset5)          

# then I went back to Course 2 R Programming Week 3 to reference videos on how to use split and found some good stuff on splitting when you have multiple factors.

class(activitydataset4$subjectid)
activitydataset4$subjectid <- as.factor(activitydataset4$subjectid)
class(activitydataset4$subjectid)

activitydataset4$subjectid
activitydataset4$activitycode

nlevels(activitydataset4$subjectid)
nlevels(activitydataset4$activitycode)

interaction(activitydataset4$subjectid, activitydataset4$activitycode)

splitactivitydataset4 <- split(activitydataset4, interaction(activitydataset4$activitycode, activitydataset4$subjectid), drop = TRUE) # Makes it so that we have...
activitydataset5 <- sapply(splitactivitydataset4, function(x) colMeans(x[ ,3:81]))                                                    # the 180 combos of levels...
View(activitydataset5)                                                                                                                # that I mentioned above.
dim(activitydataset5)                                                                                                                 # Then split by those levels.
activitydataset6 <- t(activitydataset5)  # Transposing the matrix from activitydataset5 so that it follows the form of features as column names, not row names.
View(activitydataset6)                   # Small note, wasn't working until I made it a data.frame.

subjectids <- data.frame(gl(n = 30, k = 6))     # Just creating the factor variable so that I can add a column to this data frame for subjectids.
View(subjectids)

activitycodes <- data.frame(rep(gl(n = 6, k = 1, labels = c("walking", "walkingupstairs", "walkingdownstairs", "sitting", "standing", "laying")), 30))
View(activitycodes)                             # Same thing as with the subjectids. Just making factor variable for activity codes. Small note, wasn't working until...
# I made it a data.frame.

activitydataset7 <- cbind(activitycodes, activitydataset6) # Adding the activity codes column out in front. 
activitydataset8 <- cbind(subjectids, activitydataset7)    # Adding the subjectids column out in front of that.
View(activitydataset8)

colnames(activitydataset8)[1] <- "subjectids"          # Renaming the first column
colnames(activitydataset8)[2] <- "activitycodes"       # Renaming the second column

dim(activitydataset8)
View(activitydataset8)        # Sweet baby heyzues. I think that's everything, I think that's it. 

### Checking to make sure I got it  #####################################################################################################################################

check <- activitydataset4[which(activitydataset4$subjectid == 1 & activitydataset4$activitycode == "standing"), ]
View(check)
mean(check$`tBodyAcc-mean()-X`) 
# Which returned 0.2789176. That same value is what we should find in activitydataset8 for subject = 1, activity = standing, column = tBodyAcc-mean()-X.
# Moment of truth. Let's go take a look. Please, please, please be right. And it is indeed: 0.2789176. Right on.

check <- activitydataset4[which(activitydataset4$subjectid == 1 & activitydataset4$activitycode == "sitting"), ]
View(check)
mean(check$`tBodyAcc-mean()-X`) 

## OOOOOOOOOOOOOOOOOOOOOOOOOOOH BABY! That's a fat DUBYA. At first I wasn't getting the means to match up between activitydataset4 and activitydataset8 for the...
## corresponding subject, activity, and column but after having the wind totally taken out of my sails haha I was scrolling back through activitydataset4...
## and realized that the values I originally used to calculate the mean from activitydataset4 were incomplete. Ie. I only used rows 1:27 of activitydataset4...
## b/c I thought that that was all the cases of subject 1 standing. That gave me a value of 0.2792155, which didn't match 0.2789176 from activitydataset8 for...
## subject 1, activity standing, column tBodyAcc-mean()-X. But after going back and correctly subsetting from activitydataset4 (which can now be seen above)...
## for all the values corresponding to subject 1, activity standing, column tBodyAcc-mean()-X, I got the means to match. 

###  Writing table to view tidy data set in both txt and csv forms  #####################################################################################################
write.table(x = activitydataset8, file = "G:/My Drive/Coursera Data Science Specialization/Course 3_Getting and Cleaning Data/Final Project/tidydata.txt", sep = " ", append = FALSE, dec = ".",row.names = FALSE, col.names = TRUE) 
data <- read.table(file = "G:/My Drive/Coursera Data Science Specialization/Course 3_Getting and Cleaning Data/Final Project/tidydata.txt", header = TRUE)
View(data)

write.table(x = activitydataset8, file = "G:/My Drive/Coursera Data Science Specialization/Course 3_Getting and Cleaning Data/Final Project/tidydata.csv", row.names = FALSE, sep = ",")
data1 <- read.csv("G:/My Drive/Coursera Data Science Specialization/Course 3_Getting and Cleaning Data/Final Project/tidydata.csv")
View(data1)

############################################################  Mission Accomplished.  #####################################################################################