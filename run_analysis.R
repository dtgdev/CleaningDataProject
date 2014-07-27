run_analysis <- function(){

  #Merges the training and the test sets to create one data set.
  
  #Get X training data 
  tempTrainingData <- read.table("train/X_train.txt")
  #Get test data 
  tempTestData <- read.table("test/X_test.txt")
  #Merge both data
  mergedXTrainingData <- rbind(tempTrainingData, tempTestData)
  
  #Get Y training data 
  tempTrainingData <- read.table("train/Y_train.txt")
  #Get test data 
  tempTestData <- read.table("test/Y_test.txt")
  #Merge both data
  mergedYTrainingData <- rbind(tempTrainingData, tempTestData)
  
  #get subject data
  subjectTrain <- read.table("train/subject_train.txt")
  subjectTest <- read.table("test/subject_test.txt")
  mergedSubjectData <- rbind(subjectTrain, subjectTest)
  

  #Extracts only the measurements on the mean and standard deviation for each measurement.
  
  features <- read.table("features.txt")
  #get the row's indices of the mean and std from column 2
  indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  #get the filtered data based on mean and std from the training data based 
  mergedXTrainingData <- mergedXTrainingData[, indices]

  names(mergedXTrainingData) <- gsub("\\(|\\)", "", features[indices, 2])
  names(mergedXTrainingData) <- tolower(names(mergedXTrainingData))  
  head(mergedXTrainingData)
  
  # Uses descriptive activity names to name the activities in the data set
  activitieLables <- read.table("activity_labels.txt")
  activitieLables[, 2] = as.character(activities[, 2])
  mergedYTrainingData[,1] = activitieLables[mergedYTrainingData[,1], 2]
  names(mergedYTrainingData) <- "activity"
  
  #Appropriately labels the data set with descriptive activity names.
  names(mergedSubjectData) <- "subject"
  apLabled <- cbind(mergedSubjectData, mergedYTrainingData, mergedXTrainingData)
  write.table(apLabled, "mergedLabledData.txt")
  
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

row = 1;
numOfSubjects = length(unique(mergedSubjectData)[,1])
numOfActivities = length(activitieLables[,1])
uniqueSubjects = unique(mergedSubjectData)[,1]
result = apLabled[1:(numOfSubjects*numOfActivities), ]
numCols = dim(apLabled)[2]

#loop by the number of subjects
for (s in 1:numOfSubjects) {
  ##inner loop based on the number of activities
  for (a in 1:numOfActivities) {
    result[row, 2] = activitieLables[a, 2]
    result[row, 1] = uniqueSubjects[s]  
    lbledData <- apLabled[apLabled$subject==s & apLabled$activity==activitieLables[a, 2], ]
    result[row, 3:numCols] <- colMeans(lbledData[, 3:numCols])
    #increment by one
    row = row+1
  }
}
write.table(result, "tidyDataWithaverages.txt")
}