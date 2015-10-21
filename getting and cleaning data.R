install.packages("reshape")
library(reshape)
#1.  Merges the training and the test sets to create one data set.
train<-read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)
test<-read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
data_of_two<-rbind(train,test)

# indication of 2. is behind 4.

#3.   merge the label of activities in data_of_two and Uses descriptive activity names to name the activities in the data set
# Store labels of the training set and testing set
lab_training<-read.table("./UCI HAR Dataset/train/y_train.txt")
lab_testing<-read.table("./UCI HAR Dataset/test/y_test.txt")
labels<-rbind(lab_training,lab_testing)
# activity_labs=descriptive activity names of the labels
activity_labs<-read.table("./UCI HAR Dataset/activity_labels.txt")
# merge data of two with the labels and
# make sure the colums of activitynum in the merged dataset and activity_labs are in the same class
data_labels<-cbind(labels,data_of_two)
ifelse(class(data_training$activitynum)==class(data_training$activitynum),TRUE,FALSE)
# merge the label of activities in data_of_two and Uses descriptive activity names to name the activities in the data set
# store the completely merged data_merge in data
names(data_labels)[1]<-"activity_num"
names(activity_labs)<-c("activity_num","activity")
str(data_merge)
data_merge<-merge(activity_labs,data_labels,by="activity_num")

#4.	Appropriately labels the data set with descriptive variable names. 
feature<-read.table("./UCI HAR Dataset/features.txt")
f<-feature[,2]
names(data_merge)[-c(1:2)]<-as.character(f)

#2.	Extracts only the measurements on the mean and standard deviation for each measurement.
# except for "activity" which is character(factor) vectors
table(is.na(data_merge[-2]))# make sure it has not na values
dm<-apply(data_merge[-2],2,mean)
dsd<-apply(data_merge[-2],2,sd)
data_dealing<-rbind(dm,dsd)

#5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
subject<-rbind(subject_train,subject_test)
names(subject)<-"subject"

library(dplyr)
newdata<-cbind(subject,data_merge[,-1])
newdata<-mutate(newdata,subject=as.factor(newdata[,1]))
# make 2 functions(get the mean by group) to be applied on each variable in the measurements
t1<-function(x){
  tapply(x,newdata$subject,mean)
}
t2<-function(x){
  tapply(x,newdata$activity,mean)
}
fdata<-newdata[,-c(1:2)]
# get the final results
result_subject<-as.data.frame(lapply(fdata,t1))
result_activity<-as.data.frame(lapply(fdata,t2))

write.table(result_subject,file="result_subject.txt",row.name=FALSE)
write.table(result_activity,file="result_activity.txt",row.name=FALSE)