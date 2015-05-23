run_analysis<-function()
{
  x_test<-read.table("./test/X_test.txt");
  y_test<-read.table("./test/y_test.txt");
  y_train<-read.table("./train/y_train.txt");
  x_train<-read.table("./train/X_train.txt");
  
  sub_test<-read.table("./test/subject_test.txt");
  sub_train<-read.table("./train/subject_train.txt")
  features<-read.table("./features.txt");
  train_data<-cbind(y_train,sub_train,x_train);
  test_data<-cbind(y_test,sub_test,x_test);
  
  act_labels<-read.table("./activity_labels.txt");
  act_data<-rbind(test_data,train_data);
  
  colnames(train_data)[1]="activity";
  colnames(test_data)[1]="activity";
  colnames(train_data)[2]="subject"
  colnames(test_data)[2]="subject"
  colnames(train_data)[3:563]=as.character(features);
  colnames(test_data)[3:563]=as.character(features);
  
  activity_data<-merge(x=act_data,y=act_labels,by="id",all.x=TRUE);
  features<-read.table("./features.txt");
  
  colnames(activity_data)[4:564]=as.character(features[,2]);
  x<-grep("mean",colnames(activity_data),value=TRUE)
  y<-grep("std",colnames(activity_data),value=TRUE)
  data2<-activity_data[,1:2]
  for(i in x)
     {
     data2<-cbind(data2,activity_data[,i])
       }
  colnames(data2)[3:48]=x
  for(i in y)
  {
    data2<-cbind(data2,activity_data[,i])
  }
  colnames(data2)[49:81]<-y;
  
  data2[,1]<-factor(data2[,1])
  levels(data2[,1])<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
  final_data<-group_by(data2,subject,activity) %>% summarise_each(funs(mean));
  write.table(final_data,file="./final_data.txt");
  
}