setwd(dir = "/Users/sudakshsingh/Desktop/Coursera/data/course_project")
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,destfile = "/Users/sudakshsingh/Desktop/Coursera/data/datprog.zip",method = 'curl')

unzip(zipfile = "/Users/sudakshsingh/Desktop/Coursera/data/datprog.zip")
files<-list.files(recursive = T)
#install.packages('reshape')
library(reshape)
sub_nam<-function(x)
{
  nam <- unlist(strsplit(x,split = "/"))
  nam<-nam[length(nam)]
  nam_sub<-gsub(pattern = "_test",replacement = "",x = nam)
  nam_sub<-unlist(strsplit(nam_sub,split="\\."))
  nam_sub<-nam_sub[1] 
  return(nam_sub)
}

for (i in 1:(length(files)-1))
{
  edit_i<-gsub(pattern = "test",replacement = "train",x = files[i])
 for(j in (i+1):(length(files)))
 {
   
   if (edit_i==gsub(pattern = "test",replacement = "train",x = files[j]))
  {
   print(paste(files[i],files[j],sep = "----"))
   new_name<-sub_nam(files[i])
   assign(x = new_name,value = rbind(read.table(paste0("./",files[i]),header = F),read.table(paste0("./",files[j]),header = F)))
   print(paste("---->",new_name,"--->",paste(dim(get(new_name)),collapse = "X")))
   }
 }
}

features<-read.table(files[3])
colnames(X)<-features$V2
X<-cbind(y,X)
colnames(X)[1]<-"Activity_ind"
X_subset<-X[,c(1,grep("mean",colnames(X)),grep("std",colnames(X)))]

activity_labels<-read.table(files[2])

X_subset_labelled<-merge(y = X_subset,x = activity_labels,by.y = "Activity_ind",by.x = "V1",all.y = T)
X_subset_labelled<-X_subset_labelled[,-1]
colnames(X_subset_labelled)[1]<-"Activity_labels"

X_subset_labelled<-cbind(subject,X_subset_labelled)
colnames(X_subset_labelled)[1]<-"Subject_ID"

X_tidy<-aggregate(x = X_subset_labelled[,-c(1,2)],
                  by = list(Subject_ID=X_subset_labelled$Subject_ID,
                            Activity_labels=X_subset_labelled$Activity_labels),FUN = mean)

write.table(X_tidy,"tidy_dataset.txt",row.names=F)
