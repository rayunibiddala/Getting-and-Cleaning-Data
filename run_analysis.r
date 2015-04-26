#USAGE
#> test_data <- simplify("test")
#> train_data <- simplify("train")
#> combined <- rbind(d,t)

# Helper function to gather mean, std for data 
simplify <- function (type)
{    
    data <- NULL
    
    #features header 
    featuresHeader <- read.csv("./UCI HAR Dataset/features.txt", sep=" ", header=F)
    featuresHeader<- featuresHeader[, c(1,2)]
    
    colnames(featuresHeader) <- as.character(c("id", "name"))    
    
    # read data  
    XData<-  read.table(paste("./UCI HAR Dataset/",type,"/X_" , type , ".txt", sep=""))
    XData <- XData[, c(1:561)]
    colnames(XData) <- featuresHeader$name
    
    #cExtracts only the measurements on the mean and standard deviation for each measurement. 
    #selectedCols <- grep("mean\\(\\)|std\\(\\)", colnames(XData))
    selectedCols <- grep("(mean|std)", colnames(XData))
    data <- XData[ , selectedCols]    
    
    # crate new variable for subjects
    subjectData <- read.table(file=paste("./UCI HAR Dataset/", type , "/subject_" , type , ".txt", sep=""))
    data$subject = subjectData$V1
    
    #create new variable for activity 
    YData<-  read.table(file=paste("./UCI HAR Dataset/" , type ,"/Y_" , type , ".txt", sep=""))   
    data$activity = YData$V1
    
    # return data for given type 
    data    
}

run_analysis <- function ()
{
    #
    # 1. Merges the training and the test sets to create one data set.
    # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    #
    
    # Read test data   
    testData <- simplify("test")
    # Read train data
    trainData <- simplify("train")
    #combine test and training data
    combinedData <- rbind(testData,trainData)
    
    #
    #3. Uses descriptive activity names to name the activities in the data set
    #
    
    activityLabels <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep=" ", header=F)
    activityLabels<- activityLabels[, c(1,2)]
    colnames(activityLabels) <- as.character(c("id", "description"))   
    
    #
    # 4. Appropriately labels the data set with descriptive variable names. 
    # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    # 
    
    # Group by subject and activity 
    summaryTidyData = aggregate(combinedData, by=list(activity = combinedData$activity, subject=combinedData$subject), mean)    
    summaryTidyData[with(summaryTidyData, order(activity, subject)), ]
    
    # Skip subject and activity (82, 83 columns) have no use after aggregation 
    summaryTidyData <- summaryTidyData[1:81]
    
    # Add activity Lables for data set from actiity_labels.txt
    library(sqldf)
    summaryTidyData <- sqldf("select summaryTidyData.*, activityLabels.description from summaryTidyData inner join activityLabels on summaryTidyData.activity = activityLabels.id")
    
    # replace activity id with label and remove description
    summaryTidyData$activity <- summaryTidyData$description
    summaryTidyData <- summaryTidyData[1:81]
       
    # Write data to file
    write.table(combinedData, file = "./tidy_data.txt",row.names = FALSE)      
    write.table(summaryTidyData, file = "./tidy_data_mean.txt",row.names = FALSE) 
   
}
