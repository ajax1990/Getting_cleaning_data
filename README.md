Getting_cleaning_data
=====================

for coursera course

Source of the original data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Original description: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The attached R script (run_analysis.R) performs the following to clean up the data:

1. Reads the required files : X_Train,X_Test,Y_Train,Y_Test,Subject_train,Subject_test,features,activity_labels. 
2. Names the columns in Subject_test/Subject_train as "id"
3. Assigns the column names from "features.txt" file to X train/test
4. Y_train/test are hardcoded in numbers. Each number represents an activity. The number to Activity label map
is provided in activity_labels.txt. We map it and replace the numbers with activity labels.
5. Creates the training & test set separately using cbind function.
6. Merges training & test data using rbind function.
7. Cleans special characters from the column names (brackets,dashes etc.)
8. A list of only mean/std dev. variables is obtained from the merged dataset using grep function. 
9. The aforementioned list is used to subset the merged dataset for only the mean/std dev variables. 
10. The final dataset looks like : id(Subject) Activity(Labelled : Walking/Laying/Sitting etc.) Features..... 
11. Finally for the averages data set  - sqldf package is used to compile a query that gives us the averages of the measurements
by "id(Subject)" & "Activity".
