

#This R script downloads data from a Human Activity Recognition database and 
#1. Extracts only the measurements on the mean and standard deviation for each measurement.
#2. Uses descriptive activity names to name the activities in the data set
#3. Appropriately labels the data set with descriptive variable names.
#4. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#The resulting tidy data is written to a text file

#Requires the dplyr package for use of the summarise_all function

#

# Download and unzip source file
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="UCI_HAR_20_Dataset.zip", method="curl")
unzip(zipfile="UCI_HAR_20_Dataset.zip")

#1. Merging the training and the test sets to create one data set.

#Get the features for labeling the columns
features<-read.table("UCI HAR Dataset/features.txt")
#convert to a vector for use as column names later
features<-as.vector(features)
#keep only the one column with the actual feature names
features<-features[,2]

#Get the subject numbers
subjectstest<-read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")
subjectstrain<-read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")

#Get the activity labels
activity_columns=c("Number", "Name")
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = activity_columns )

#Get the data tables and label them
xtestdata<-read.table("UCI HAR Dataset/test/x_test.txt", col.names = features)
ytestdata<-read.table("UCI HAR Dataset/test/y_test.txt", col.names = "Activity")
xtraindata<-read.table("UCI HAR Dataset/train/x_train.txt", col.names = features)
ytraindata<-read.table("UCI HAR Dataset/train/y_train.txt", col.names = "Activity")

#merge subject and data tables of test and train data
testdata<-cbind(subjectstest,xtestdata,ytestdata)
traindata<-cbind(subjectstrain,xtraindata,ytraindata)

#merge test and train data
data<-rbind(testdata,traindata)

#pick only the columns activity, subject and any column containing mean or standard deviation (containing string "mean." or "std.")
extracteddata <- data[, grepl("Activity|Subject|mean\\.|std\\.", names(data))]

#2. Applying descriptive activity names to name the activities in the data set

#replace the activity factors with the corresponding names from the activities table
extracteddata$Activity <- factor(extracteddata$Activity, levels = activities$Number, labels = activities$Name)

#3. Appropriate labeling the data set with descriptive variable names.

colnames(extracteddata)<-sub("^t", "Time_", colnames(extracteddata))
colnames(extracteddata)<-sub("^f", "Frequency_", colnames(extracteddata))
colnames(extracteddata)<-sub("BodyAcc", "Body_Accelaration_", colnames(extracteddata))
colnames(extracteddata)<-sub("GravityAcc", "Gravity_Acceleration_", colnames(extracteddata))
colnames(extracteddata)<-sub("BodyGyro", "Body_Angular_Velocity_", colnames(extracteddata))
colnames(extracteddata)<-sub(".mean.", "Mean_", colnames(extracteddata))
colnames(extracteddata)<-sub(".std.", "StandardDev_", colnames(extracteddata))
colnames(extracteddata)<-sub("Mag", "Magnitude_", colnames(extracteddata))
colnames(extracteddata)<-sub("..X", "X", colnames(extracteddata))
colnames(extracteddata)<-sub("..Y", "Y", colnames(extracteddata))
colnames(extracteddata)<-sub("..Z", "Z", colnames(extracteddata))


#4.Creating a tidy data set with the average of each variable for each activity and each subject.


library(dplyr)

tidyData<- extracteddata %>%  group_by(Activity,Subject) %>%   summarise_all(funs(mean))

write.table(tidyData, "tidyData.txt", row.name=FALSE, quote= FALSE)


#remove the intermediary data
rm(xtestdata,ytestdata,xtraindata,ytraindata,testdata,traindata,data,subjectstest,subjectstrain,activities)
