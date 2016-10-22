#Load neccessary database
library(dplyr)
library(tidyr)

#Read data from files
#Read overview data files
setwd("~/Documents/Coursera/DataScientist/Course3/UCI HAR Dataset")
activity_labels <- read.csv("activity_labels.txt", sep = " ", header = FALSE)
features <- read.csv("features.txt", sep = " ", header = FALSE)

#Read testing data files
setwd("~/Documents/Coursera/DataScientist/Course3/UCI HAR Dataset/test")
X_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

#Read training data files
setwd("~/Documents/Coursera/DataScientist/Course3/UCI HAR Dataset/train")
X_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

#Step 1
#Combine test and train population, train on top of test
X_combined <- rbind(X_train, X_test)
y_combined <- rbind(y_train, y_test)
subject_combined <- rbind(subject_train, subject_test)

#Step 2
#Find location of mean and standard deviation -> Step 2
mean_pos <- grep("mean", features[,2])
mean_std <- grep("std", features[,2])
meanstd_pos <- grep("\\bmean\\b|\\bstd\\b", features[,2])
#Get data that only comprised of mean and standard deviation
X_combined_meanstd <- X_combined[, meanstd_pos]          

#Step 3
#Add activities and subject as first and second columns into X_combined 
#Activities name was extracted from activity_labels
X_combined_meanstd2 <- cbind(subject_combined, activity_labels[y_combined[,1], 2], X_combined_meanstd)

#Step 4
#Label column with appropriate names, including: "subject", "activity" and appropriate activities name
#Activities name was extracted from features.txt
#Get rid of '()'
colname1 <- gsub(".mean\\(\\).|.mean\\(\\)", "Mean", as.character(features[meanstd_pos,2])) 
colname2 <- gsub(".std\\(\\).|.std\\(\\)", "Std", colname1)
#Add columns name
colnames(X_combined_meanstd2)<- c("subject", "activity", colname2)

#Step 5
#Tidying data into long narrow table with four columns: subjects, activities, variable, variable value
X_combined_meanstd3 <- gather(X_combined_meanstd2,variable, 
                              variable_value,tBodyAccMeanX:fBodyBodyGyroJerkMagStd)

#Group the variable into 
X_combined_meanstd3 %>% tbl_df %>% group_by(subject, activity,variable) -> X_combined_meanstd3group
result <- summarise(X_combined_meanstd3group, mean(variable_value))

#Write data out
write.table(X_combined_meanstd2, "Step4Data.csv", row.names = FALSE)
write.table(X_combined_meanstd3, "Step5Data.csv", row.names = FALSE)
write.table(c("activity", "subject", features[,2]), "new_feature.csv", row.names = TRUE)