#############################################################################
########### Getting and Cleaning Data - Course Project ######################
#############################################################################

## First, let's download and unzip the data

filename <- "Course_Project_Data.zip"

if (!file.exists(filename)) {
      fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(fileURL, filename, method = "curl")
}

if(!file.exists("UCI HAR Dataset")) {
      unzip(filename)
}

## Let's load the activity labels and features
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt",
                             stringsAsFactors = FALSE)
class(activityLabels[,2])
# if we had not added stringsAsFactors, we could need to convert activity labels
# to character
### activityLabels[ ,2] <- as.character(activityLabels[,2])
featureLabels <- read.table("UCI HAR Dataset/features.txt",
                             stringsAsFactors = FALSE)
class(featureLabels[,2])

# Now we'll load the datasets that contain the data
# Let's start with the train data
train <- read.table("UCI HAR Dataset/train/X_train.txt")
# Load the activity code data
trainAct <- read.table("UCI HAR Dataset/train/Y_train.txt")
# Load the subject ID data
trainID <- read.table("UCI HAR Dataset/train/subject_train.txt")
# Now we'll bind these together into one train dataset
# and only pull the rows with the features we need
train_all <- cbind(trainID, trainAct, train)

# Let's load the test data
test <- read.table("UCI HAR Dataset/test/X_test.txt")
# Load the activity code data
testAct <- read.table("UCI HAR Dataset/test/Y_test.txt")
# Load the subject ID data
testID <- read.table("UCI HAR Dataset/test/subject_test.txt")
# Now we'll bind these together into one test dataset
# and only pull the rows with the features we need
test_all <- cbind(testID, testAct, test)

### Now we can combine the datasets together
myData <- rbind(train_all, test_all)
# Rename all of the column headings
colnames(myData) <- c("ID", "Activity", featureLabels[ ,2])

## Convert the activity codes to the actual activity name
# Use the factor() function to take the activity data column to assign the data
# as factor levels using the first column of activityLabels and then label those
# levels using the 2nd column of activityLabels
myData$Activity <- factor(myData$Activity, levels = activityLabels[ ,1],
                          labels = activityLabels[ ,2])

## Now let's start to remove variables that do not include means or std's
# Create a vector for all the column names
colNames <- colnames(myData)
# Create a logic vector to identify which columns we want to keep
# i.e., those with ID, Activity and the phrase mean or std in them
colNamesNeeded <- (grepl("ID", colNames) |
                  grepl("Activity", colNames) |
                  grepl(".*mean.", colNames) |
                  grepl(".*std.", colNames))
# Then we subset the columns where the logic vector is TRUE
myDataNeeded <- myData[ , colNamesNeeded == TRUE]

# Rename the columns to clean up the -'s and ()'s
CleanColNames <- colnames(myDataNeeded)
CleanColNames <- gsub('-mean', "Mean", CleanColNames)
CleanColNames <- gsub("-std", "Mean", CleanColNames)
CleanColNames <- gsub("[-()]", "", CleanColNames)
colnames(myDataNeeded) <- c(CleanColNames)

### Now we need to reshape the data to provide the new tidy table with the
# desired outcomes
## Use the reshape2() library and the melt() function
library(reshape2)
# Using melt, we can reshape the data to be organized by ID and activity
melt_data <- melt(myDataNeeded, id = c("ID", "Activity"))
# Using dcast, we can cast a melted dataframe into an operation (acast for other types)
# Note that a melted data frame does not use character labels for the columns
Melted_ID_act_mean <- dcast(melt_data, ID + Activity ~ variable, mean)

# Output a file called "tidy.txt" that provides the new dataset
write.table(Melted_ID_act_mean, "tidy.txt", row.names = FALSE, quote = FALSE)
