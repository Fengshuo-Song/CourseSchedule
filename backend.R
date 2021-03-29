library(tidyverse)
library(rvest)
library(RSelenium)
library(stringr)

# Get 3-letter subject names
html = read_html("https://ucdavis.pubs.curricunet.com/Catalog/courses-subject-code")
subject_names_raw <- html %>%
  html_nodes("div.col-3") %>% 
  html_nodes("a") %>%
  html_text()

subject_names = c()
for(item in subject_names_raw){
  if(item != "back to top"){
    print(item)
    subject_names = append(subject_names, substr(item, 1, 3))
    
  }
}

# df after removing "@" rows and unnecessary columns
all_df = data.frame(matrix(ncol = 6, nrow = 0))

x = c("CRN","Time","Days","SEC","Seats","Course Location")
colnames(all_df) = x

port = httpuv::randomPort()
server = wdman::chrome(port = port, version = "88.0.4324.27", verbose = FALSE)
rd = remoteDriver(port = port)

rd$open(silent = TRUE)
# Check if there is any error messages
stopifnot(is.null(rd$sessionInfo$message))
course_search_list = subject_names
# Get all courses data using 3-letter subject names
for(item in course_search_list) {
  rd$navigate("https://registrar-apps.ucdavis.edu/courses/search/index.cfm")
  
  rd$findElement("css", "input#course_number")$clickElement()
  rd$sendKeysToActiveElement(list(item))
  rd$findElement(
    "css", 
    'table#home_tablez_bz input[name="search"]'
  )$clickElement()
  
  retry::retry({
    df = rd$getPageSource()[[1]] %>% 
      read_html() %>% 
      html_node("div#courseResultsDiv table") %>% 
      html_table()
    # If there is no courses found, then continue next search!!!
    if(nrow(df) == 0 || str_detect(df[1],"Please refine your search") ||
       str_detect(df[1],"Please modify your search")) {
      next
    }
    
    df_colnames = df %>% slice(4)
    df = df %>%
      slice(., -(1:4))
    names(df) = df_colnames
    df = df %>%
      separate(`CRN Time/Days`, c("CRN", "Time/Days"), "\n\\s+\t") %>% 
      separate(`Time/Days`, c("Time", "Days"), ",+\\s") %>% 
      separate(`Sec. Seats Avail.`, c("SEC", "Seats"), "\\s+") %>% 
      separate(`Title Former GE Credit . New GE Credit`, c("Title.OldGE", " New GE"), "\\s+.\\s") %>% 
      separate(Title.OldGE, c("Title", "Old GE"), "\\s{2,}") %>% 
      separate(`Instructor Course Units`, c("Instructor", "Units"), " \n\t\t\t\t\t") %>% 
      select(-Detail,-Save) %>% mutate(CRN=str_remove(CRN,"\\s+$"))
  },
  when = "xml_missing",
  timeout = 5
  )

  ## Remove @ column
  df = df  %>% select(CRN,Time,Days,SEC,Seats,`Course Location`) %>% 
    filter(str_detect(CRN,"^[0-9]+"))
  all_df = rbind(all_df,df)
  
  # Check the progress
  cat("END: ")
  cat(item)
  cat("\n")
}

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

## Add course name
get_course_name <- function(info) {

  key_word = str_sub(info,1,3)
  cur_item = indexTable %>% filter(subject.name == key_word)
  base = cur_item$base[1]
  bound = cur_item$bound[1]
  for(i in base:bound){
    if(startsWith(info,all_courses[i,"name"] %>% unlist())){
      return(all_courses[i,"name"] %>% unlist())
    }
  }
  return("")
}

all_data = all_df %>% mutate(course_name = "")
for(i in 1:nrow(all_data)){
  all_data[i,"course_name"] = get_course_name(all_data[i,"Course Location"])
}

all_new = all_data %>% group_by(CRN) %>% mutate(Section = first(SEC)) %>% 
  mutate(Seats = first(Seats)) %>% mutate(`Course Location` = first(`Course Location`)) %>% 
  summarise(all_time = paste(Time, collapse = "|"), all_days = paste(Days, collapse = "|"), Section = first(SEC), Seats = first(Seats), course_name = first(course_name))


all_new = all_new %>% mutate(format.time = "TBA") %>% mutate(has.tba = 0)                                                                                      
for(i in 1:nrow(all_new)){
  if(all_new[i,"all_time"] %>% str_detect("[0-9]")){
    all_new[i,"format.time"] = paste(format_time_list(all_new[i,"all_time"],all_new[i,"all_days"]),collapse = "#")
  }
  if(str_detect(all_new[i,"all_time"],"TBA")){
    all_new[i,"has.tba"] = 1
  }
}
write.csv(all_new,"data_final.csv")
