library(tidyr)
library(dplyr)
#1.Merges the training and the test sets to create one data set.
train_X <- read.table("./Data/train/X_train.txt")
train_Y <- read.table("./Data/train/y_train.txt")
train_Subject <- read.table("./Data/train/subject_train.txt")
test_X <- read.table("./Data/test/X_test.txt")
test_Y <- read.table("./Data/test/y_test.txt") 
test_Subject <- read.table("./Data/test/subject_test.txt")
join_X <- rbind(train_X, test_X)
join_Y <- rbind(train_Y, test_Y)
join_Subject <- rbind(train_Subject, test_Subject)

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./data/features.txt")

meanData <- grep("mean()", features[,2], fixed = TRUE)
stdData <- grep("std", features[,2], fixed = TRUE)

meanStd <- c(meanData, stdData)
meanStd <- data.frame(meanStd)
meanStd <- arrange(meanStd, meanStd)
meanStd <- unlist(meanStd)
meanStd <- as.integer(meanStd)

join_X <- join_X[, meanStd]
names(join_X) <- gsub("V", "", features[meanStd, 2])
names(join_X) <- gsub("mean", "Mean", names(join_X))
names(join_X) <- gsub("std", "Std", names(join_X))
names(join_X) <- gsub("-", "", names(join_X))
names(join_X) <- gsub("()", "", names(join_X), fixed=TRUE)

#3.Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("./data/activity_labels.txt")

activity_labels[, 2] <- gsub("_", "", activity_labels[, 2])
activity_labels[, 2] <- tolower(activity_labels[, 2])
activity_labels[, 2] <- gsub("down", "Down", activity_labels[, 2], fixed = TRUE)
activity_labels[, 2] <- gsub("up", "Up", activity_labels[, 2], fixed = TRUE)

activity_labels <- activity_labels[join_Y[, 1], 2]
join_Y[, 1] <- activity_labels
names(join_Y) <- "activity"

#4.Appropriately labels the data set with descriptive variable names. 
names(join_Subject) <- "subject"
cleanMerge <- cbind(join_Subject,join_Y, join_X)
write.table(cleanMerge, "merged_data.txt")

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
meanData <- aggregate(. ~subject + activity, cleanMerge, mean)
meanData <- arrange(meanData, subject, activity)
write.table(meanData, file = "tidy_merged_data.txt", row.name=FALSE)