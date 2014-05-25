Getting and Cleaning Data- Course Project
===========================================



###Assumtions
#####We begin the process with 2 asumptions:
* "_UCI HAR Dataset_" folder, which is extracted from the zip file provided is set as the working directory.
* "plyr" package for reshaping data is installed and loaded

##### This working directory has two folders(test and train) and 4 text files("features.txt", "features_info.txt","Readme.txt" and "activity_labels.txt" .Test folder has three text files("X_test.txt","y_test.txt" and "subject_test.txt") that we'll be loading for test data. Similarly Train folder has 3 files("X_train.txt","Y_train.txt" and "subject_train.txt")
#####X_test and X_train are the largest files which contain data for 561 variables received from accelorometer and gyrometer of samsung phone.Y_test and Y_train contains the information about the type of activity performed and subject_test and subject _train provides information about the subjects in test and training set respectively.

###Step 1- Creating the complete data set from _Training_ and _Test_ data

##### We begin by importing the coloumn names from "_features.txt_" to _colnm_ vector.This file has a total of 561 column names to be used ahead
colnm<-read.table("features.txt")

#####We begin creating the test Data Frame by uploading the X_test.txt file and naming the coloumns using the colnm vector.X_test is read first to match the column names to the repective data. Then we proceed to read in the subject_test and y_test files, while combining them with xtest to using cbind to create the test matrix. the test data set has 2947 rows and 563 coloumns
xtest<-read.table("./test/X_test.txt")
colnames(xtest)<-colnm[[2]]
test<- cbind(read.table("./test/subject_test.txt"),read.table("./test/y_test.txt"),xtest)
##### Now we name the columns from subject_test and y-test files
colnames(test)[c(1,2)]<- c("subject","activityid")

#####The process to create the train dataset is exactly the same as the process for test dataset.the train dataset has 7352 rows and 563 coloumns.
xtrain<-read.table("./train/X_train.txt")
colnames(xtrain)<-colnm[[2]]
train<-cbind(read.table("./train/subject_train.txt"),read.table("./train/y_train.txt"),xtrain)
colnames(train)[c(1,2)]<- c("subject","activityid")

##### the final task in Step 1 involves rowbinding the train and the test dataframe to create one big dataframe containing 10299 rows and 563 coloumns
data<-rbind(test,train)

###Step 2- Extract only the measurements on the mean and standard deviation for each measurement. 
##### The problem requires us to work with data containg mean and standard deviation data alone. Hence we subset the data in a way that allows us to seperate all the variables associated with mean and standard deviation. We use the **_grep()_** function for this purpose.On runnng the grep function we find that 46 columns are associated with mean. But on closer inspection, we realize that 13 of the 46 observations are using meanFreq which gives the _**weighted average**_ of the frequency components to obtain a mean frequency. Since weighted average is involved we have leave them out of the subset. This leaves us with 33 variables for both mean and standard devaition . We thus use the indices obtained from the grep function to subset the old data set and include the mean and standard deviation variables along subject and activity id variables.The new datafrme has 10299 rows and 68 coloumns.

colwithmean<- grep("mean()",colnames(data),fixed =T)
colwithstd<-grep("std()",colnames(data),fixed =T)
data<- data[,c(1,2,colwithmean,colwithstd)]

###Step 3- Uses descriptive activity names to name the activities in the data set
#####The next step involves using the _"activity_labels.txt"_ file in the home directory to assign activty labels to activity id.
##### We use control structures(for and else if commands) in r to perform this task.  The labels from activity_labels.txt are assigned to a new column
##### which is named as -"activity" .
for(i in 1:nrow(data)){
        if(data[i,2]== 1){data[i,69]<- "WALKING"}
        else if(data[i,2]== 2){data[i,69]<- "WALKING_UPSTAIRS"}
        else if(data[i,2]== 3){data[i,69]<- "WALKING_DOWNSTAIRS"}
        else if(data[i,2]== 4){data[i,69]<- "SITTING"}
        else if(data[i,2]== 5){data[i,69]<- "STANDING"}
        else if(data[i,2]== 6){data[i,69]<- "LAYING"}
                }
colnames(data)[69]<-"activity"

##### Once the new column is created we rearrange the columns in the "data" datafrme to bring together "activity_id" and "activity" variables
data$activity<-as.factor(data$activity)
d1<-data[,c(1:2,69,3:68)] 

###Step 4- Appropriately labels the data set with descriptive activity names. 
###### Once the dataset is ready we begin the data cleaning process. Since the variable names are incoherent with tidy data rules we perform a 
#####number of data cleaning tasks.
* Ensure all letters of variable names are in lowercase using tolower()function
colnames(data)<-tolower(colnames(data))

* We use gsub to make the following variable names more descriptive  
colnames(data)<-sub("fbody","frequencyDomain",colnames(data))
colnames(data)<-sub("tbody","timeDomainBody",colnames(data))
colnames(data)<-sub("tgravity","timeDomainGravity",colnames(data))
colnames(data)<-gsub("gyro","Gyroscope",colnames(data), fixed=T)    
colnames(data)<-gsub("acc","Accelaration",colnames(data) ,fixed=T)
colnames(data)<-gsub("mag","Magitude",colnames(data) ,fixed=T)
colnames(data)<-gsub("-x","Xcoordinate",colnames(data) ,fixed=T)
colnames(data)<-gsub("-y","Ycoordinate",colnames(data) ,fixed=T)
colnames(data)<-gsub("-z","Zcoordinate",colnames(data) ,fixed=T)
colnames(data)<-gsub("-std","StandardDeviation",colnames(data) ,fixed=T)

*gsub is used here to remove unwanted symbols from variable names
colnames(data)<-gsub("()","",colnames(data) ,fixed=T)
colnames(data)<-gsub("-","",colnames(data) ,fixed=T)

*gsub is used here to uppercase the first letters of the following words and hence make the entire variable names in Camel Case
colnames(data)<-gsub("jerk","Jerk",colnames(data) ,fixed=T)
colnames(data)<-gsub("mean","Mean",colnames(data) ,fixed=T)

#####  **_Please note that the only reason we have decided to use CamelCase and not all lower case here is to improve the readability 
##### of the variable names_**


###Step 5-Create a second, independent tidy data set with the average of each variable for each activity and each subject

##### Inorder to obtain data in the required form we must first melt and the recast data.This is done using the functions in plyr package
##### which we assumed is already installed and loaded

#####We begin by converting the data from wide format to long format using the melt function. We use subjects, activityid and activity as id variables .
#####  The remainig become measured variables by default
datamelt<- melt(data,id= c("subject","activityid","activity"))

###### Now we cast the matrix back to wide data form using dcast function. The fun.aggregate function allows us to calcuate the average
#####of each variable for each activity and each subject.
tidydataset<-dcast(datamelt,subject+activityid+activity~variable,fun.aggregate=mean)

##### "tidydataset" is the desired matrix which is then written a text file in working directory using the write.table() function
write.table(tidydataset,"./tidydataset.txt")

                  
