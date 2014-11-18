########################################################################################
## I have labeled data sets according to the steps numbers to facilitate code tracking##
########################################################################################

##Loading the necessary data packages
if (!require("data.table")) {
  install.packages("data.table")
}

if (!require("reshape2")) {
  install.packages("reshape2")
}
require("data.table")
require("reshape2")

## Setting the working directory to be inside the folder containing the data
setwd ("./UCI HAR Dataset")

## Reading the features file
features<- read.table("features.txt")

## Reading the 3 files inside the test folder
xtest<-read.table("./test/X_test.txt")
ytest<-read.table("./test/Y_test.txt")
subjecttest<-read.table("./test/subject_test.txt")

## Changing the coulmn names for binding purposes
colnames(xtest)<- features[,2]
colnames(ytest)<-"Activity"
colnames(subjecttest)<-"Subject"
##Binding the 3 datasets of the test folder into a 1 dataset test
test<-cbind(xtest,ytest,subjecttest)
rm (xtest);rm(ytest);rm(subjecttest) ## Removing the objects that will not be used

## Reading the 3 files inside the train folder
xtrain<-read.table("./train/X_train.txt")
ytrain<-read.table("./train/Y_train.txt")
subjecttrain<-read.table("./train/subject_train.txt")
## Changing the coulmn names for binding purposes
colnames(xtrain)<- features[,2]
colnames(ytrain)<-"Activity"
colnames(subjecttrain)<-"Subject"

##Binding the 3 datasets of the test folder into a 1 dataset  train
train<-cbind(xtrain,ytrain,subjecttrain)
rm (xtrain);rm(ytrain);rm(subjecttrain);rm(features) ## Removing the objects that will not be used

##Step 1-Creating 1 Data Set for Testing & Training 
## This also covers step 4 Appropriately labels the data set with descriptive variable names.
step1<-rbind (test,train)
rm (test);rm(train) ## Removing the objects that will not be used
###################################################################################################




##Step 2  Extracts only the measurements on the mean and standard deviation for each measurement. 
names<-colnames(step1) ## Getting coulmn names
step2<-step1[,grep("mean",names,invert=FALSE)]  ## Creating a new Data set that have the mean coulmns
step2 <-cbind (step2,step1[,grep("std",names,invert=FALSE)])## Adding to the new Data set that have the Std coulmns
step2<-cbind(step1[,562:563],step2)## Adding to the new Data set the Activity & subject coulmns
rm(step1) ## Removing the objects that will not be used
###################################################################################################



##Step 3  Uses descriptive activity names to name the activities in the data set. 
step3<-step2
rm (step2) ## Removing the objects that will not be used
## Replacing the numbers in the activity coulmns with the Activities from the Activity file
step3$Activity <- factor(step3$Activity, labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
###################################################################################################



## Step 4 already covered in step 1, so will use the data set step3 instead of creating a new one
## Extracting the merged data set to a tab Delimited TXT file only for review
write.table(step3 ,file="./merged_data.txt",sep="\t",row.names= FALSE)
###################################################################################################



## Step 5  creates a second, independent tidy data set with the average of each variable for each activity and each subjec
id_labels=c("Activity","Subject")
data_labels = setdiff(colnames(step3), id_labels) ## getting the data labels of all the coulmns exept Activity & Subject
melt_data<-melt(step3, id = id_labels, measure.vars = data_labels) ## Applying the met function to produce a different data frame
# Apply mean function to dataset using dcast function
avg_data<-dcast(melt_data, Subject + Activity ~ variable, mean)
## Extracting the "Required" average data set to a tab Delimited TXT file
write.table(avg_data, file = "./average_data.txt",sep="\t",row.names= FALSE)
rm(avg_data);rm(melt_data);rm(step3);rm(data_labels);rm(id_labels);rm(names)## Removing all objects
###################################################################################################
###################################################################################################
