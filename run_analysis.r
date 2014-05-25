
#Assumtion
##The assumption here is that "UCI HAR Dataset" folder, which is extracted from the zip file provided is set as the working directory.

#Step 1- Creating the complete 
##Importing the columnnames to r
colnm<-read.table("features.txt")

##Creating the test Data Frame and naming the columns
xtest<-read.table("./test/X_test.txt")
colnames(xtest)<-colnm[[2]]
test<- cbind(read.table("./test/subject_test.txt"),read.table("./test/y_test.txt"),xtest)
### name the columns from subject_test.txt and y-test.txt
colnames(test)[c(1,2)]<- c("subject","activityid")

##Creating the train Data Frame and naming of columns
xtrain<-read.table("./train/X_train.txt")
colnames(xtrain)<-colnm[[2]]
train<-cbind(read.table("./train/subject_train.txt"),read.table("./train/y_train.txt"),xtrain)
### name the columns from subject_train.txt and y_train.txt
colnames(train)[c(1,2)]<- c("subject","activityid")

##Merging the train and the test dataframe to create one dataframe
data<-rbind(test,train)


#Step 2- Extracts only the measurements on the mean and standard deviation for each measurement. 
colwithmean<- grep("mean()",colnames(data),fixed =T)
colwithstd<-grep("std()",colnames(data),fixed =T)
data<- data[,c(1,2,colwithmean,colwithstd)]

#Step 3- Uses descriptive activity names to name the activities in the data set
for(i in 1:nrow(data)){
        if(data[i,2]== 1){data[i,69]<- "WALKING"}
        else if(data[i,2]== 2){data[i,69]<- "WALKING_UPSTAIRS"}
        else if(data[i,2]== 3){data[i,69]<- "WALKING_DOWNSTAIRS"}
        else if(data[i,2]== 4){data[i,69]<- "SITTING"}
        else if(data[i,2]== 5){data[i,69]<- "STANDING"}
        else if(data[i,2]== 6){data[i,69]<- "LAYING"}
                }
colnames(data)[69]<-"activity"
data$activity<-as.factor(data$activity)
d1<-data[,c(1:2,69,3:68)] 

#Step 4- Appropriately labels the data set with descriptive activity names. 
colnames(data)<-tolower(colnames(data))
colnames(data)<-gsub("()","",colnames(data) ,fixed=T)
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
colnames(data)<-gsub("-","",colnames(data) ,fixed=T)
colnames(data)<-gsub("jerk","Jerk",colnames(data) ,fixed=T)
colnames(data)<-gsub("mean","Mean",colnames(data) ,fixed=T)

#Step 5-Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
#Writing the data to a text file
datamelt<- melt(data,id= c("subject","activityid","activity"))
tidydataset<-dcast(datamelt,subject+activityid+activity~variable,fun.aggregate=mean)
write.table(tidydataset,"./tidydataset.txt")

                  
