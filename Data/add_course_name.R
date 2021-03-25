library(stringr)
library(stringi)
library(dplyr)
all_courses <- read_csv("Data/all_courses.csv")
all_data_full <- read_csv("Data/all_data_full.csv")
all_data<-read_csv("Data/all_data.csv")
#new column
library(dplyr)
all_data<-mutate(all_data,course_name=" ")
n<-nrow(all_data)
for (i in 1:n){
  decode<-str_extract(all_data$`Course Location`[i],"^[A-Z]+")
  if(!is.na(decode)){
    index<-indexTable%>%filter(subject.name==decode)
    for (j in index$base:index$bound) {
      deter2<-str_contains(all_data$`Course Location`[i],all_courses$name[j])
      if(deter2 ==T){
        all_data$course_name[i]=all_courses$name[j]
      }
    }
  }
  else{
    all_data$course_name[i]="na"
  }
}
all_data

write.csv(all_data,file = "Data/all_data_name.csv")
