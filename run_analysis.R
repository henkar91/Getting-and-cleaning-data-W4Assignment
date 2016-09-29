#You should create one R script called run_analysis.R that does the following.

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###############################################################################
setwd("~/Google Drive/R/Coursera/Getting and cleaning data")
setwd("C:/Users/Henrik.Karlsson/Desktop/Projekt/R/Coursera/Getting and cleaning data")

#'README.txt'
#- 'features_info.txt': Shows information about the variables used on the feature vector.
#- 'features.txt': List of all features.
#- 'activity_labels.txt': Links the class labels with their activity name.                      
#- 'train/X_train.txt': Training set.
#- 'train/y_train.txt': Training labels.
#- 'test/X_test.txt': Test set.
#- 'test/y_test.txt': Test labels.
###############################################################################

# 1. Merges the training and the test sets to create one data set.

# read all test data
features <- read.table("data/UCI HAR Dataset/features.txt")
x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
activity_label <- read.table("Data/UCI HAR Dataset/activity_labels.txt")

# Update column names in data frames
colnames(subject_test) <- "id"
colnames(y_test) <- "activity_id"
colnames(x_test) <- features$V2
colnames(activity_label) <- c("activity_id", "activity_type" )

# merge activity name with activity id in y_test
y_test <- merge(x = y_test, y = activity_label, by = "activity_id", all.x = TRUE)

# merge columns from subject_test, y_test and x_test
test <- cbind(subject_test, y_test, x_test)
str(test)

###############################################################################
# read train data
# features <- read.table("data/UCI HAR Dataset/features.txt")
x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")

# Update column names in data frames
colnames(subject_train) <- "id"
colnames(y_train) <- "activity_id"
colnames(x_train) <- features$V2

# merge activity name with activity id in y_train
y_train <- merge(x = y_train, y = activity_label, by = "activity_id", all.x = TRUE)

# merge columns from subject_train, y_train and x_train
train <- cbind(subject_train, y_train, x_train)

# merges test and train to one data set
dt <- rbind(test, train)

###############################################################################

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# logical vector which tells what columns to include
wanted_col <- (grepl("id", names(dt)) | grepl("activity_", names(dt)) | grepl("-mean", names(dt)) & !grepl("mean..-",names(dt)) & !grepl("-meanFreq", names(dt)) | grepl("-std", names(dt)) & !grepl("-std()..-",names(dt)))

# check number of columns to include
table(wanted_col)

# removes unwanted columns from the merged data set
dt <- dt[,wanted_col]
names(dt)

###############################################################################

# 3. Uses descriptive activity names to name the activities in the data set

#already done

###############################################################################

# 4. Appropriately labels the data set with descriptive variable names.

names(dt)

grep("()", names(dt))

colnames(dt) <- gsub("-mean()", " Mean", names(dt))
colnames(dt) <- gsub("-std()", " St Dev", names(dt))
colnames(dt) <- gsub("_", " ", names(dt))
colnames(dt) <- gsub("^(t)", "Time ", names(dt))
colnames(dt) <- gsub("^(f)", "Frequency ", names(dt))
names(dt)

###############################################################################

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dt2 <- dt[,colnames(dt) != "activity type"]

tidy <- aggregate(dt2[, names(dt2) != c("id", "activity id")], by = list(dt2$id, dt2$`activity id`), mean)

names(tidy)[names(tidy) == "Group.1"] <- "id"
names(tidy)[names(tidy) == "Group.2"] <- "activity_id"

tidy <- merge(tidy, activity_label, by = "activity_id", all.x = TRUE)

View(tidy)

write.table(tidy, file = "W4Assignment result.txt", row.names = TRUE, sep = ",")

