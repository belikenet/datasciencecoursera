==================================================================
#Getting and Cleaning Data Course Project 
##Version 1.0
==================================================================
###Francisco José Pérez López
==================================================================

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

[http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones]

Here are the data for the project: 

[https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip]

You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


The project homework includes the following files:
=========================================

- 'README.md': This file

- 'CodeBook.md': Code book that describes the variables, the data, and any transformations or work that you performed to clean up the data

- 'run_analysis.r': Defines main function run_analysis(x) which creates a tidy data set with the average of each variable for each activity and each subject. Use parameter to set up the dataset folder

- 'str_activity.R': Support function for cleaning up data activities.

- 'str_feature.R': Support function for cleaning up data features.


License:
========
Francisco José Pérez López. March, 2015.
