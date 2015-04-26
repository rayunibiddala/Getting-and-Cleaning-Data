Code Book
=========

Data source
-----------

This raw dataset is derived from the "Human Activity Recognition Using Smartphones Data Set". The dataset contains sensor information collected from 30 volunteers for six activities performed wearing Samsung Galaxy SII. Using smartphone embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. The data set is avialable at url http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones. 

Feature Selection 
-----------------

Following is more detailed information on meta data for dataset. This information is collected from original source data set 'ReadMe.txt' and 'features_info.txt'. 
 
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

Based on instructions in course project the data set contains mean and standard deviation for each measurement. Following are the variables that have to do with mean and standard deviation. 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAcc-XYZ
* tGravityAcc-XYZ
* tBodyAccJerk-XYZ
* tBodyGyro-XYZ
* tBodyGyroJerk-XYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc-XYZ
* fBodyAccJerk-XYZ
* fBodyGyro-XYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

The set of variables that were estimated from these signals are: 

* mean(): Mean value
* std(): Standard deviation
* meanFreq(): Weighted average of the frequency components to obtain a mean frequency

Data transformation
-------------------

The raw data sets are processed with run_analysis.r script to create a tidy data set. 

### Merges the training and the test sets to create one data set.

Test and training data (X_train.txt, X_test.txt), subject ids (subject_train.txt,
subject_test.txt) and activity ids (y_train.txt, y_test.txt) are merged to obtain
a single data set. Variables are labelled with the names assigned by original
collectors (features.txt).

### Extracts only the measurements on the mean and standard deviation for each measurement. 

From the merged data set extracted and intermediate data set with column names have mean and standard deviation. 

### Uses descriptive activity names to name the activities in the data set

Based on values in "activity_labels.txt" from source data, lookup operation is performed to read descriptive names of the activity id. The descriptive activity lables are added as new column.

### Appropriately labels the data set with descriptive variable names. 

Labels given in original data set were used as it is, as these descriptive and easy to read. 

### Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

From the intermediate data set, the mean is calculated for a given activity and subject into second data set 'tidy_data_mean.txt'