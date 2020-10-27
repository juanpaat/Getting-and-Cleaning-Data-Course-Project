library(dplyr)

#upload the raw data to the envirolment
#train
X_train<-read.table("./train/X_train.txt")#Training set.
y_train<-read.table("./train/y_train.txt")#Training labels.
#load the feautures names
features<-read.table("./features.txt")
#test
X_test<-read.table("./test/X_test.txt")#Test set.
y_test<-read.table("./test/y_test.txt")#Test labels.

#bind rows of the test and train datasets
table<-bind_rows(X_test,X_train,.id="subject")

#bind rows of the test an train labels
table2<-bind_rows(y_test,y_train,.id="subject")

#change the 1s and 2s for test and train respectively
table2<-table2%>%
        mutate(subject=ifelse(subject==1,"test","train"))

#add new variable(col) so we have a single table with all the information,
#labels and data, and put the col activity before 
complete_table<-table%>%
        mutate(subject=ifelse(subject==1,"test","train"),activity=table2$V1)%>%
        relocate(activity, .before = V1)


##Uses descriptive activity names to name the activities in the data set
complete_table$activity<-factor(complete_table$activity,
                                levels = c(1,2,3,4,5,6),
                                labels = c("WALKING",
                                          "WALKING_UPSTAIRS",
                                          "WALKING_DOWNSTAIRS",
                                          "SITTING",
                                          "STANDING",
                                          "LAYING"))

##check the table
str(complete_table)

#means of each variable
means<-colMeans(complete_table[,3:563])

#sd of each variable
desviations<-sapply(complete_table[,3:563], sd, na.rm = TRUE)

#grouped dataset
grouped_table<-complete_table%>%
                group_by(activity,subject)%>%
                summarise(mean=colMeans(complete_table[,3:563]))

##Creating a data set
write.table(x = grouped_table,file="DataSet.txt",row.name=FALSE) 





