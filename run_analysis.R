# Instructions on Site - Create a script that : 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# My Submission :  


#Set Working directory & Data read part - both test & train 

setwd("D:\\COursera\\Getting & cleaning data\\Course Project\\Working dir")

X_Train = read.table("X_train.txt")
Y_Train = read.table("y_train.txt")
Subject_Train = read.table("subject_train.txt")
colnames(Subject_Train)<-c("id")

X_Test = read.table("X_test.txt")
Y_Test = read.table("y_test.txt")
Subject_Test = read.table("subject_test.txt")
colnames(Subject_Test)<-c("id")

#Assigning the column names from features files to X train/test

Heading_file <- read.table("features.txt")
formatted_headings<-Heading_file$V2

colnames(X_Train)<-formatted_headings
colnames(X_Test) <-formatted_headings

# Mapping Y train/test labels to Activities provided in separate file using merge function 

activity_labels  <- read.table("activity_labels.txt")
Y_Train<-merge(Y_Train,activity_labels,by="V1",all.x=T)
Y_Train<-Y_Train[,2]
Y_Test<-merge(Y_Test,activity_labels,by="V1",all.x=T)
Y_Test<-Y_Test[,2]

# Creating the training & test set separately using cbind. 
# Column names uniformity maintained to do rbind later

training_set = cbind(X_Train, Subject_Train, Y_Train)
test_set = cbind(X_Test, Subject_Test, Y_Test)
training_set$Activity<-training_set$Y_Train
test_set$Activity<-test_set$Y_Test
colnames(training_set)

training_set<-training_set[,c(1:562,564)]
test_set<-test_set[,c(1:562,564)]
summary(test_set)
summary(training_set)

# Merging training & test data using rbind function 

Combined_dataset<-rbind(training_set,test_set)

# Cleaning special characters from the column names & Subsetting for only mean/std dev measurements

names(Combined_dataset)
names(Combined_dataset) <- gsub("\\(|\\)", "", names(Combined_dataset))
names(Combined_dataset) <- gsub("\\(|\\)|-|,", "", names(Combined_dataset))
required_colnames <- names(Combined_dataset)
required_colnames <- grep("mean|std",required_colnames, value=TRUE)
Combined_dataset<-Combined_dataset[,c("id","Activity",required_colnames)]


#For the final Summary/Grouped Averages file - used SQL package of R and ran a simple query 

library(sqldf)

summary <- sqldf("select Activity,id,avg(tBodyAccmeanX) as Avg_tBodyAccmeanX,avg(tBodyAccmeanY) as Avg_tBodyAccmeanY,
avg(tBodyAccmeanZ) as Avg_tBodyAccmeanZ,avg(tBodyAccstdX) as Avg_tBodyAccstdX,
avg(tBodyAccstdY) as Avg_tBodyAccstdY,avg(tBodyAccstdZ) as Avg_tBodyAccstdZ,
avg(tGravityAccmeanX) as Avg_tGravityAccmeanX,avg(tGravityAccmeanY) as Avg_tGravityAccmeanY,
avg(tGravityAccmeanZ) as Avg_tGravityAccmeanZ,avg(tGravityAccstdX) as Avg_tGravityAccstdX,
avg(tGravityAccstdY) as Avg_tGravityAccstdY,avg(tGravityAccstdZ) as Avg_tGravityAccstdZ,
avg(tBodyAccJerkmeanX) as Avg_tBodyAccJerkmeanX,avg(tBodyAccJerkmeanY) as Avg_tBodyAccJerkmeanY,
avg(tBodyAccJerkmeanZ) as Avg_tBodyAccJerkmeanZ,avg(tBodyAccJerkstdX) as Avg_tBodyAccJerkstdX,
avg(tBodyAccJerkstdY) as Avg_tBodyAccJerkstdY,avg(tBodyAccJerkstdZ) as Avg_tBodyAccJerkstdZ,
avg(tBodyGyromeanX) as Avg_tBodyGyromeanX,avg(tBodyGyromeanY) as Avg_tBodyGyromeanY,
avg(tBodyGyromeanZ) as Avg_tBodyGyromeanZ,avg(tBodyGyrostdX) as Avg_tBodyGyrostdX,
avg(tBodyGyrostdY) as Avg_tBodyGyrostdY,avg(tBodyGyrostdZ) as Avg_tBodyGyrostdZ,
avg(tBodyGyroJerkmeanX) as Avg_tBodyGyroJerkmeanX,avg(tBodyGyroJerkmeanY) as Avg_tBodyGyroJerkmeanY,
avg(tBodyGyroJerkmeanZ) as Avg_tBodyGyroJerkmeanZ,avg(tBodyGyroJerkstdX) as Avg_tBodyGyroJerkstdX,
avg(tBodyGyroJerkstdY) as Avg_tBodyGyroJerkstdY,avg(tBodyGyroJerkstdZ) as Avg_tBodyGyroJerkstdZ,
avg(tBodyAccMagmean) as Avg_tBodyAccMagmean,avg(tBodyAccMagstd) as Avg_tBodyAccMagstd,
avg(tGravityAccMagmean) as Avg_tGravityAccMagmean,avg(tGravityAccMagstd) as Avg_tGravityAccMagstd,
avg(tBodyAccJerkMagmean) as Avg_tBodyAccJerkMagmean,avg(tBodyAccJerkMagstd) as Avg_tBodyAccJerkMagstd,
avg(tBodyGyroMagmean) as Avg_tBodyGyroMagmean,avg(tBodyGyroMagstd) as Avg_tBodyGyroMagstd,
avg(tBodyGyroJerkMagmean) as Avg_tBodyGyroJerkMagmean,avg(tBodyGyroJerkMagstd) as Avg_tBodyGyroJerkMagstd,
avg(fBodyAccmeanX) as Avg_fBodyAccmeanX,avg(fBodyAccmeanY) as Avg_fBodyAccmeanY,
avg(fBodyAccmeanZ) as Avg_fBodyAccmeanZ,avg(fBodyAccstdX) as Avg_fBodyAccstdX,
avg(fBodyAccstdY) as Avg_fBodyAccstdY,avg(fBodyAccstdZ) as Avg_fBodyAccstdZ,
avg(fBodyAccmeanFreqX) as Avg_fBodyAccmeanFreqX,avg(fBodyAccmeanFreqY) as Avg_fBodyAccmeanFreqY,
avg(fBodyAccmeanFreqZ) as Avg_fBodyAccmeanFreqZ,avg(fBodyAccJerkmeanX) as Avg_fBodyAccJerkmeanX,
avg(fBodyAccJerkmeanY) as Avg_fBodyAccJerkmeanY,avg(fBodyAccJerkmeanZ) as Avg_fBodyAccJerkmeanZ,
avg(fBodyAccJerkstdX) as Avg_fBodyAccJerkstdX,avg(fBodyAccJerkstdY) as Avg_fBodyAccJerkstdY,
avg(fBodyAccJerkstdZ) as Avg_fBodyAccJerkstdZ,avg(fBodyAccJerkmeanFreqX) as Avg_fBodyAccJerkmeanFreqX,
avg(fBodyAccJerkmeanFreqY) as Avg_fBodyAccJerkmeanFreqY,
avg(fBodyAccJerkmeanFreqZ) as Avg_fBodyAccJerkmeanFreqZ,
avg(fBodyGyromeanX) as Avg_fBodyGyromeanX,avg(fBodyGyromeanY) as Avg_fBodyGyromeanY,
avg(fBodyGyromeanZ) as Avg_fBodyGyromeanZ,avg(fBodyGyrostdX) as Avg_fBodyGyrostdX,
avg(fBodyGyrostdY) as Avg_fBodyGyrostdY,avg(fBodyGyrostdZ) as Avg_fBodyGyrostdZ,
avg(fBodyGyromeanFreqX) as Avg_fBodyGyromeanFreqX,
avg(fBodyGyromeanFreqY) as Avg_fBodyGyromeanFreqY,
avg(fBodyGyromeanFreqZ) as Avg_fBodyGyromeanFreqZ,
avg(fBodyAccMagmean) as Avg_fBodyAccMagmean,
avg(fBodyAccMagstd) as Avg_fBodyAccMagstd,
avg(fBodyAccMagmeanFreq) as Avg_fBodyAccMagmeanFreq,avg(fBodyBodyAccJerkMagmean) as Avg_fBodyBodyAccJerkMagmean,avg(fBodyBodyAccJerkMagstd) as Avg_fBodyBodyAccJerkMagstd,
avg(fBodyBodyAccJerkMagmeanFreq) as Avg_fBodyBodyAccJerkMagmeanFreq,avg(fBodyBodyGyroMagmean) as Avg_fBodyBodyGyroMagmean,avg(fBodyBodyGyroMagstd) as Avg_fBodyBodyGyroMagstd,avg(fBodyBodyGyroMagmeanFreq) as Avg_fBodyBodyGyroMagmeanFreq,avg(fBodyBodyGyroJerkMagmean) as Avg_fBodyBodyGyroJerkMagmean,avg(fBodyBodyGyroJerkMagstd) as Avg_fBodyBodyGyroJerkMagstd,
avg(fBodyBodyGyroJerkMagmeanFreq) as Avg_fBodyBodyGyroJerkMagmeanFreq from Combined_dataset group by Activity,id")

# Writing out the final tidy data to working directory

write.table(summary,file="tidy_data.txt",row.name=FALSE)
