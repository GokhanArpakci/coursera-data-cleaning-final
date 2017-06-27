#Peer-graded Assignment: Getting and Cleaning Data Course Project 

#REQUIREMENT 1: Download the data and merge them 

#downloading the data with all features 
DATA_FILE <- "sets.zip"
if (!file.exists(DATA_FILE)){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", DATA_FILE, method="libcurl")
}  

#unpacking sets 
DATA_FOLDER <- "DATA_FOLDER"
if (!dir.exists(DATA_FOLDER)) { 
  unzip(DATA_FILE)
}

#merging the data function.
merge_sets <- function(folder, subject, data, label) {
  data_frame_subject <- read.table(paste(DATA_FOLDER, folder, subject, sep = .Platform$file.sep))
  data_frame_data <- read.table(paste(DATA_FOLDER, folder, data, sep = .Platform$file.sep))
  data_frame_label <- read.table(paste(DATA_FOLDER, folder, label, sep = .Platform$file.sep))
  
  return(cbind(data_frame_subject, data_frame_data, data_frame_label))
}

#Apply the function

data_frame_test <- merge_sets("test", "subject_test.txt", "X_test.txt", "y_test.txt")
data_frame_train <- merge_sets("train", "subject_train.txt", "X_train.txt", "y_train.txt")

#Bring the all Data Together
complete_data_frame <- rbind(data_frame_test, data_frame_train)

#Append a Column
data_column_names <- read.table(paste(DATA_FOLDER, "features.txt", sep = .Platform$file.sep), stringsAsFactors = FALSE)
colnames(complete_data_frame) <- c("subject", data_column_names[,2], "activity_id")
#(END OF REQUIREMENT 1)

#REQUIREMENT 2: Data Cleaning

#Deleting the columns that are not mean and standard, and subject and activity_id
columns_to_keep = grep("subject|std\\(\\)|mean\\(\\)|activity_id", colnames(complete_data_frame), value=TRUE)
#Extract the needed columns
required_data_frame <- complete_data_frame[columns_to_keep]
#(END OF REQUIREMENT 2)

#REQUIREMENT 3: Merge and Convert

activity_labels_data_frame <- read.table(paste(DATA_FOLDER, "activity_labels.txt", sep = .Platform$file.sep), stringsAsFactors = FALSE)
colnames(activity_labels_data_frame) <- c("activity_id", "activity_label")
#merging, and over-writing activity_id
required_data_frame <- merge(required_data_frame, activity_labels_data_frame,by="activity_id")
#converting the activity to factors and removing the activity_id column
required_data_frame$activity_label = as.factor(required_data_frame$activity_label)
required_data_frame$activity_id <- NULL
#(END OF REQUIREMENT 3)


#Requirement 4: Mean and STD dev Columns Preparetion

#cleaning up variable names.
#1) changing - to _
colnames(required_data_frame) <- gsub("-", "_", names(required_data_frame))
#2) changing mean() to mean-value
colnames(required_data_frame) <- gsub("mean\\(\\)", "mean_value", names(required_data_frame))
#3) changing std() to standard_deviation
colnames(required_data_frame) <- gsub("std\\(\\)", "standard_deviation", names(required_data_frame))
#4) chaning "Acc" to "_acceleration_"
colnames(required_data_frame) <- gsub("Acc", "_acceleration", names(required_data_frame))
#5) expending the prefix (t or f) to (time_ or frequency_)
colnames(required_data_frame) <- gsub("^t", "time_", names(required_data_frame))
colnames(required_data_frame) <- gsub("^f", "frequency_", names(required_data_frame))
#(END OF REQUIREMENT 4)

#REQUIREMENT 5: reshape
#data will be reshaped in order to melt and cast functions to create the tidy averages for each subject
install.packages("reshape")
library(reshape)

melted_data_frame <- melt(required_data_frame, id=c("subject", "activity_label"))
average_activity_values_data_frame <- cast(melted_data_frame, subject+activity_label~variable, mean)

write.table(average_activity_values_data_frame, file = "tidy.txt", row.names = FALSE)
#(END OF REQUIREMENT 5)
