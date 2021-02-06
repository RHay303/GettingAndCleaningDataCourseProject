library(reshape2)

#set working directory

setwd("C:/Users/rp303/OneDrive/Documents/coursera data science/GettingAndCleaningDataCourseProject2")

#create file

filename <- "getdata_dataset.zip"

## Check if the file exists already in the directory and if not, download and unzip the files in a folder
##named UCI HAR Dataset

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

##In the dataset is a file with labels for each type of activity as follows:
## 1 WALKING
## 2 WALKING_UPSTAIRS
## 3 WALKING_DOWNSTAIRS
## 4 SITTING
## 5 STANDING
## 6 LAYING

# Load activity labels file to variable with same name
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] <- as.character(activityLabels[,2])

##Load the features.txt data into features variable
#561-feature vector with time and frequency domain variables

features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

#We are on interested in the mean and std. deviation
# Look in the second column of features and search for 'mean' or 'std' and
# store that to featuresWanted


featuresWanted <- grep(".*mean.*|.*std.*", features[,2])

#found 79 matches
#bring back the corresponding names
featuresWanted.names <- features[featuresWanted,2]

#To make names more readalbe, make substitions in so that Mean and Std are capitalized, removes a dash, and
#removes parenthesis and dash;
featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)

featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)


#The data set has both test and train datasets and labels
#- 'train/X_train.txt': Training set.
#- 'train/y_train.txt': Training labels.
#- 'test/X_test.txt': Test set.
#- 'test/y_test.txt': Test labels.

# Load the train dataset limiting the selection to the featuresWanted we defined above

train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresWanted]
trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

#combine all the columns into one dataset train
train <- cbind(trainSubjects, trainActivities, train)

#Load the test dataset again limiting the selection the featuresWanted
test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresWanted]
testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

#combine all the results into on dataset test
test <- cbind(testSubjects, testActivities, test)

# merge train and test datasets together into allData
allData <- rbind(train, test)
#aChange the column names in allData to those used in featuresWanted
colnames(allData) <- c("subject", "activity", featuresWanted.names)

# turn activities & subjects into factors
#replace activity number in allData$actity with the name of the related activity in activityLabels
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)


#use melt function to create a unique id-variable combination
allData.melted <- melt(allData, id = c("subject", "activity"))

#casted the melted data and calculate the mean
allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)

#output data to tidy.text file

write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
