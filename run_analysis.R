library(tidyverse)
#Part 1
##Load Data
features <- rio::import("UCI HAR Dataset/features.txt")
activities <- rio::import("UCI HAR Dataset/activity_labels.txt")
subject_test <- rio::import("UCI HAR Dataset/test/subject_test.txt")
X_test <- rio::import("UCI HAR Dataset/test/X_test.txt")
Y_test <- rio::import("UCI HAR Dataset/test/Y_test.txt")
subject_train <- rio::import("UCI HAR Dataset/train/subject_train.txt")
X_train <- rio::import("UCI HAR Dataset/train/X_train.txt")
Y_train <- rio::import("UCI HAR Dataset/train/Y_train.txt")
##Name columns
colnames(features)<- c("x","reading")
colnames(activities) <- c("number","activity")
colnames(X_test) <- features$reading
colnames(Y_test) <- c("Labels")
colnames(subject_test) <- c("subjects")
colnames(X_train) <- features$reading
colnames(Y_train) <- c("Labels")
colnames(subject_train) <- c("subjects")
##Bind data
###Add subjects
X_test <- cbind(subject_test,X_test)
Y_test <- cbind(Y_test)
X_train <- cbind(subject_train, X_train)
Y_train <- cbind(Y_train)
###combine all X data 
x_full <- rbind(X_test,X_train)
y_full <- rbind(Y_test,Y_train)
full_data <- cbind(y_full,x_full)

#Part 2
mean_sdfull <- full_data%>%
  dplyr::select_if(grepl("mean",colnames(.)) | grepl("subject",colnames(.)) | grepl("std",colnames(.)) | grepl("Labels",colnames(.)))

#Part 3
mean_sdact <- mean_sdfull %>%
  dplyr::mutate(Labels = case_when(Labels == 1 ~ "WALKING",
                                   Labels == 2 ~ "WALKING_UPSTAIRS",
                                   Labels == 3 ~ "WAlKING_DOWNSTAIRS",
                                   Labels == 4 ~ "SITTING",
                                   Labels == 5 ~ "STANDING",
                                   Labels == 6 ~ "LAYING"))
#Part 4
##VARIABLE NAMES ARE PLACED AS COLUMNNAMES

#Part 5
summarized_data <- mean_sdact %>%
  dplyr::group_by(Labels,subjects)%>%
  dplyr::summarize_all(list(mean))

