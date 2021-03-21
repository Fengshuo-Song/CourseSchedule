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

# full df
all_df_full = data.frame(matrix(ncol = 10, nrow = 0))

x = c("CRN","Time","Days","SEC","Seats","Course Location")
colnames(all_df) = x
x = c("CRN","Time","Days","Course Location","SEC","Seats","Title","Old GE",
      " New GE","Instructor","Units",)
colnames(all_df_full) = x

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
      separate(`Title Former GE Credit . New GE Credit`, c("Title.OldGE", " New GE"), "\\s+ . \\s") %>% 
      separate(Title.OldGE, c("Title", "Old GE"), "\\s{2,}") %>% 
      separate(`Instructor Course Units`, c("Instructor", "Units"), " \n\t\t\t\t\t") %>% 
      select(-Detail,-Save) %>% mutate(CRN=str_remove(CRN,"\\s+$"))
  },
  when = "xml_missing",
  timeout = 5
  )
  
  all_df_full = rbind(all_df_full,df)

  ## Remove @ column
  df = df  %>% select(CRN,Time,Days,SEC,Seats,`Course Location`) %>% 
    filter(str_detect(CRN,"^[0-9]+"))
  all_df = rbind(all_df,df)
  
  # Check the progress
  cat("END: ")
  cat(item)
  cat("\n")
}

write.csv(all_df,"all_data.csv")
write.csv(all_df_full,"all_data_full.csv")
