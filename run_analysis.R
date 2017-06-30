rm(list = ls())
library(dplyr)
library(data.table) 
library(plyr)
library(tidyr)

setwd("C:/Users/calvar44/Dropbox/2. COURSERA/1. Data Science/3. Getting and Cleaning Data/Project")
fileZip<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileZip, destfile = "Dataset.zip")
unzip(zipfile="Dataset.zip",exdir="./Data")
setwd("C:/Users/calvar44/Dropbox/2. COURSERA/1. Data Science/3. Getting and Cleaning Data/Project/Data/UCI HAR Dataset")

activity_labels <- read.table("activity_labels.txt")[,2]
features <- read.table("features.txt")[,2]
subject_test <- read.table("test/subject_test.txt")
test_set <- read.table("test/x_test.txt")
test_labels <- read.table("test/y_test.txt")
subject_train<- read.table("train/subject_train.txt")
training_set<- read.table("train/X_train.txt")
training_labels<- read.table("train/y_train.txt")

names(test_set) = features
names(subject_test) = "Subject"
test_labels[,2] = activity_labels[test_labels[,1]]
names(test_labels) = c("Activity_ID", "Activity_Label")
test_data <- cbind(as.data.table(subject_test), test_labels, test_set)

names(training_set) = features
names(subject_train) = "Subject"
training_labels[,2] = activity_labels[training_labels[,1]]
names(training_labels) = c("Activity_ID", "Activity_Label")
training_data <- cbind(as.data.table(subject_train), training_labels, training_set)

## 1. Merges the training and the test sets to create one data set.
finalData <- rbind(training_data,test_data)
colNames <- colnames(finalData)
dim(finalData)


## 2. Extracts only the measurements on the mean and standard deviation for 
##each measurement.
names(finalData) <- make.unique(names(finalData))
datameanstd <- select(finalData,matches("Subject|Activity_ID|mean|std"))
dim(datameanstd)


## 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("activity_labels.txt")
names(activity_labels) = c("Activity_ID", "Activity_Label")
datameanstd2 <- merge(datameanstd, activity_labels, by="Activity_ID")
str(datameanstd2)


## 4. Appropriately labels the data set with descriptive variable names.
names(datameanstd2)
## All lower case when possible
names(datameanstd2) <- tolower(names(datameanstd2))
## Descriptive
names(datameanstd2) <- gsub("-","",names(datameanstd2),)
names(datameanstd2) <- gsub("tbody","timebody",names(datameanstd2),)
names(datameanstd2) <- gsub("std()","SD",names(datameanstd2),)
names(datameanstd2) <- gsub("mean()","MEAN",names(datameanstd2),)
names(datameanstd2) <- gsub("freq()","frequency",names(datameanstd2),)
names(datameanstd2) <- gsub("fbodybody","frequencybody",names(datameanstd2),)
names(datameanstd2) <- gsub("fbody","frequencybody",names(datameanstd2),)
names(datameanstd2) <- gsub("tgravity","timegravity",names(datameanstd2),)
names(datameanstd2) <- gsub("acc","accelerometer",names(datameanstd2),)
names(datameanstd2) <- gsub("gyro","gyroscope",names(datameanstd2),)
names(datameanstd2) <- gsub("mag","magnitude",names(datameanstd2),)


## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
tidy <- ddply(datameanstd2, c("subject","activity_id"), numcolwise(mean), row.name=FALSE)
write.table(tidy,file="tidy.txt")

