library(tidyverse)
library(rvest)
library(RSelenium)
library(stringr)

## Index List
all_courses = read_csv("all_courses.csv")
colnames(all_courses) = c("index","name")
all_courses = all_courses %>% mutate(subject.name = str_extract(name,"^[A-z]+")) %>% 
  mutate(course.number = str_extract(name,"[0-9]+$"))
all_courses

indexTable = all_courses %>% group_by(subject.name) %>% summarise(base = first(index), bound = last(index))

## Format Time
format_time <- function(time,days) {
  formated_list = c()
  day_list = str_split(days, "")[[1]]
  for(i in day_list) {
    formated_list = append(formated_list,paste(i,time,sep = " "))
  }
  #return(paste(formated_list, collapse='#'))
  return(formated_list)
}

format_time_list <- function(time,days) {
  time_list = str_split(time,"\\|")[[1]]
  days_list = str_split(days,"\\|")[[1]]
  result_list = c()
  for(i in 1:length(time_list)) {
    if(time_list[i] != "TBA"){
      result_list = append(result_list,format_time(time_list[i],days_list[i]))
    }
  }
  return(result_list)
}

all_data = read.csv("all_data_name.csv")
all_new = all_data %>% group_by(CRN) %>% mutate(Section = first(SEC)) %>% 
  mutate(Seats = first(Seats)) %>% mutate(Course.Location = first(Course.Location)) %>% 
  summarise(all_time = paste(Time, collapse = "|"), all_days = paste(Days, collapse = "|"), Section = first(SEC), Seats = first(Seats), course_name = first(course_name))
  

all_new = all_new %>% mutate(format.time = "TBA") %>% mutate(has.tba = 0)                                                                                      
for(i in 1:nrow(all_new)){
  if(all_new[i,"all_time"] != "TBA"){
    all_new[i,"format.time"] = paste(format_time_list(all_new[i,"all_time"],all_new[i,"all_days"]),collapse = "#")
  }
  if(str_detect(all_new[i,"all_time"],"TBA")){
    all_new[i,"has.tba"] = 1
  }
}
write.csv(all_new,"data_final.csv")
