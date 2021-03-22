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

subject_names_lower = c()
for(item in subject_names_raw){
  if(item != "back to top"){
    subject_names_lower = append(subject_names_lower, tolower(substr(item, 1, 3)))
    
  }
}
subject_names_lower[which(subject_names_lower == "fse")] = "fsu"
all_course = c()
for(item in subject_names_lower) {
  if(item != "pun"){
    link = str_glue("https://ucdavis.pubs.curricunet.com/Catalog/{name}-courses-sc",name = item)
  }
  else{
    link = str_glue("https://ucdavis.pubs.curricunet.com/Catalog/{name}-courses-sp",name = item)
  }
    html = read_html(link)
  courses <- html %>%
    html_nodes("div.col-xs-10") %>% 
    html_text() %>% str_extract("^[A-z0-9\\s]+") %>% str_remove("-")
  all_course = append(all_course,courses)
  
  cat("END: ")
  cat(item)
  cat("\n")
}
all_course
write.csv(all_course,"all_courses.csv")
