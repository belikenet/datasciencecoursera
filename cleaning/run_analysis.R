run_analysis <- function (pathToUciHarDataset){
    
    # 1. Merges the training and the test sets to create one data set.
    test <- read.table(paste(pathToUciHarDataset,"/test/X_test.txt", sep=""))
    train <- read.table(paste(pathToUciHarDataset,"/train/X_train.txt", sep=""))
    all_ds <- rbind(test, train)
    
    rm(train)
    rm(test)
    
    # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
    features <- read.csv(paste(pathToUciHarDataset,"/features.txt", sep=""), sep=" ", header=F)
    selected_features <- features[grepl("-mean()|-std()", features[,2]),]
    selected_ds <- all_ds[,selected_features[,1]]
    rm(all_ds)
    
    # 3. Uses descriptive activity names to name the activities in the data set.
    activity_labels <- read.csv(paste(pathToUciHarDataset,"/activity_labels.txt", sep=""), sep=" ", header=F)
    
    test_activities <- read.fwf(paste(pathToUciHarDataset,"/test/y_test.txt", sep=""), header=F,widths=1)
    train_activities <- read.fwf(paste(pathToUciHarDataset,"/train/y_train.txt",sep=""), header=F,widths=1)
    activities <- rbind(test_activities, train_activities)
    rm(test_activities)
    rm(train_activities)
    
    #activity_labels$Activity = factor(activity_labels[,1], labels=str_activity(activity_labels[,2]))
    activities$Activity <- factor(activities[,1], labels = str_activity(activity_labels[,2]))
    
    selected_ds$Activity <- activities$Activity
    
    
    # 4. Appropriately labels the data set with descriptive variable names.
    names(selected_ds) <- c(str_feature(selected_features[,2]), "Activity")
    
    # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    test_subjects <- read.fwf(paste(pathToUciHarDataset,"/test/subject_test.txt",sep=""), widths=2, header=F)
    train_subjects <- read.fwf(paste(pathToUciHarDataset,"/train/subject_train.txt",sep=""), widths=2, header=F)
    subjects <- rbind(test_subjects, train_subjects)
    rm(test_subjects)
    rm(train_subjects)
    
    selected_ds$Subject <- subjects[,1]
    
    tidy_ds <- aggregate(selected_ds, by=list(Subject = selected_ds$Subject, Activity = selected_ds$Activity), mean)[,1:81]    

}