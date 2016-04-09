
# Clean up workspace
rm(list=ls())

fileName <- "getdata_dataset.zip"

## Download and unzip the dataset

if (!file.exists(fileName)){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,fileName,method = "curl")
  
  unzip(fileName, exdir = "./data")  
}



## Step 1
## Merge the training and test sets
features     <- read.table("./data/UCI HAR Dataset/features.txt")
activityType <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
subjectTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
xTrain       <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
yTrain       <- read.table("./data/UCI HAR Dataset/train/y_train.txt")

## Assigin column names to the data imported above
colnames(activityType)  <- c("activityId","activityType")
colnames(subjectTrain)  <- "subjectId"
colnames(xTrain)        <- features[,2] 
colnames(yTrain)        <- "activityId"

## Combine train data
trainingData <- cbind(yTrain,subjectTrain,xTrain)

subjectTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
xTest       <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
yTest       <- read.table("./data/UCI HAR Dataset/test/y_test.txt")

## Assigin column names to the data imported above
colnames(subjectTest) <- "subjectId"
colnames(xTest)       <- features[,2] 
colnames(yTest)       <- "activityId"


## Combine the test data
testData <- cbind(yTest,subjectTest,xTest)


## Combine training and test data 
finalData <- rbind(trainingData,testData)

## Create a vector for the column names from the finalData, which will be used
## to select the desired mean() & stddev() columns
colNames  <- colnames(finalData)


## Step 2
## Extract only the measurements on the mena an std
logicalVector <- (grepl("activity..",colNames) | 
                    grepl("subject..",colNames) | 
                    grepl("-mean..",colNames) & 
                    !grepl("-meanFreq..",colNames) & 
                    !grepl("mean..-",colNames) | 
                    grepl("-std..",colNames) & 
                    !grepl("-std()..-",colNames))

# Subset finalData table based on the logicalVector to keep only desired columns
finalData <- finalData[logicalVector==TRUE]

## step 3 
## Use descriptive activity names to name the activities in the data set
finalData <- merge(finalData,activityType,by="activityId",all.x=TRUE)

colNames  <- colnames(finalData)

## step 4
## Label the data set with descriptive variables
for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

## Reassigning the new descriptive column names to the finalData set
colnames(finalData) <- colNames

## step 5
## crate independent tidy data set with the avg of each variable
## for each activity adn each subject 
finalDataNoActivityType  <- finalData[,names(finalData) != "activityType"]

tidyData    <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c("activityId","subjectId")],
                         by=list(activityId=finalDataNoActivityType$activityId,
                          subjectId = finalDataNoActivityType$subjectId),mean)

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    <- merge(tidyData,activityType,by="activityId",all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, "./data/UCI HAR Dataset/tidyData.txt",row.names=TRUE,sep="\t")