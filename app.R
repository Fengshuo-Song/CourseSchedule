library(tidyverse)
library(rvest)
library(RSelenium)
library(stringr)
library(shiny)

format_str <- function(s){
  if(nchar(s) < 4){
    return(s)
  }
  s = str_replace_all(s," ","")
  s = toupper(s)
  p = " "
  num_len = nchar(str_extract(str_sub(s,4,-1), "^[0-9]+"))
  if(num_len == 1){
    p = " 00"
  }
  else if(num_len == 2){
    p = " 0"
  }
  return(paste(str_sub(s,1,3),p,str_sub(s,4,-1),sep=""))
}

time2int <- function(hour, minute, AMPM){
  h = as.numeric(hour)
  m = as.numeric(minute)
  if(AMPM == "PM" && h != 12){
    h = h + 12
  }
  return(h * 100 + m)
}

is_time_conflict <- function(t1,t2){
  if(identical(t1,t2)){
    return(TRUE)
  }

  str_list1 = (t1 %>% str_replace(" - ","@") %>% str_replace_all(" ","@") %>%
                 str_replace_all(":","@") %>% str_split("@"))[[1]]
  str_list2 = (t2 %>% str_replace(" - ","@") %>% str_replace_all(" ","@") %>%
                 str_replace_all(":","@") %>% str_split("@"))[[1]]

  # Not in the same day
  if(str_list1[1] != str_list2[1]){
    return(FALSE)
  }

  starttime1 = time2int(str_list1[2], str_list1[3], str_list1[6])
  endtime1   = time2int(str_list1[4], str_list1[5], str_list1[6])
  starttime2 = time2int(str_list2[2], str_list2[3], str_list2[6])
  endtime2   = time2int(str_list2[4], str_list2[5], str_list2[6])
  if(starttime2 >= endtime1 || endtime2 <= starttime1){
    return(FALSE)
  }
  return(TRUE)

}

is_timeList_conflict <- function(tList1,tList2){
  if(tList1 == "" || tList2 == ""){
    return(FALSE)
  }
  tList1 = str_split(tList1,"#")[[1]]
  tList2 = str_split(tList2,"#")[[1]]
  for(t1 in tList1){
    for(t2 in tList2){
      if(is_time_conflict(t1,t2)){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}

add_time <- function(t1,t2){
  if(t1 == ""){
    return(t2)
  }
  if(t2 == ""){
    return(t1)
  }
  return(paste(t1,t2,sep = "#"))
}

combList <<- c()
get_comb <- function(df_list, total, current, timeList, CRNList){

  if(current > total){
    combList <<- append(combList,CRNList)
    return(0)
  }
  curr_df = df_list[[current]]
  for(i in 1:nrow(curr_df)){
    if(!is_timeList_conflict(curr_df[i,"format.time"],timeList)){
      get_comb(df_list,total,current+1,add_time(timeList,curr_df[i,"format.time"]),append(CRNList,curr_df[i,"CRN"]))
    }
  }
}

check_preferred <- function(tList, mint, maxt)
{
  if(mint == 6 && maxt == 24) {
    return(TRUE)
  }
  if(tList == "TBA" || tList == "") {
    return(TRUE)
  }
  time_list = str_split(tList,"#")[[1]]
  for(item in time_list) {
    str_list = (item %>% str_replace(" - ","@") %>% str_replace_all(" ","@") %>%
                   str_replace_all(":","@") %>% str_split("@"))[[1]]
    starttime = time2int(str_list[2], str_list[3], str_list[6])
    endtime   = time2int(str_list[4], str_list[5], str_list[6])
    if(starttime < mint * 100 || endtime > maxt * 100)
      return(FALSE)
  }
  return(TRUE)
}

check_all_preferred <- function(s_table)
{
  for(item in s_table$time.preferred){
    if(item == FALSE){
      return(FALSE)
    }
  }
  return(TRUE)
}

ui <- fluidPage(
  navbarPage(
    title = "Course Schedule Building Assistant",
    tabPanel(
      title = "Schedule Building Assistant",

      sidebarLayout(
        sidebarPanel(
          textInput(
            inputId = "course1",
            label = "Full Course Name1:",
            value = ""
          ),
          textInput(
            inputId = "course2",
            label = "Full Course Name2:",
            value = ""
          ),
          textInput(
            inputId = "course3",
            label = "Full Course Name3:",
            value = ""
          ),
          textInput(
            inputId = "course4",
            label = "Full Course Name4:",
            value = ""
          ),
          textInput(
            inputId = "course5",
            label = "Full Course Name5:",
            value = ""
          ),
          textInput(
            inputId = "course6",
            label = "Full Course Name6:",
            value = ""
          ),
          textInput(
            inputId = "course7",
            label = "Full Course Name7:",
            value = ""
          ),
          textInput(
            inputId = "course8",
            label = "Full Course Name8:",
            value = ""
          ),
          sliderInput("time",
                      "Time Preference",
                      min = 6,
                      max = 24,
                      value = c(6,24)
          ),
          actionButton("do", "Click Me")

        ),
        mainPanel(
          textOutput("noclass"),
          verbatimTextOutput("course_schedule")
        )
      )
    ),
    tabPanel(
      title = "Instructions"
    )
  )
)
get_data <- function(input_list, time_range){
  mint = time_range[1]
  maxt = time_range[2]
  combList <<- c()
  df = read.csv("data_final.csv")
  df_list = list()
  course_num = 0
  for(i in 1:length(input_list)){
    if(input_list[i] == ""){
      next
    }
    input_list[i] = format_str(input_list[i])
    if(input_list[i] %in% df$course_name){
      temp = df %>% filter(course_name == input_list[i])
      course_num = course_num + 1
      df_list[[course_num]] = temp
    }
    else{
      return(i)
    }
  }
  if(course_num == 0){
    return(data.frame(matrix(ncol=0,nrow=0)))
  }

  get_comb(df_list,course_num,1,"",c())

  tableList = c()
  pos = 1
  num_comb = length(combList)%/%course_num
  if(num_comb == 0){
    return(123)
  } else
  {
    for(i in 1:num_comb){
      temp = data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("course_name", "Section", "format.time"))))
      for(j in 1:course_num){

        df_temp = df_list[[j]] %>% filter(CRN == combList[pos])
        is_preferred = check_preferred(df_temp$format.time, mint, maxt)
        df_temp = df_temp %>% rename(time = all_time,days = all_days) %>%
          select(CRN,course_name,Section, time, days) %>% mutate(time.preferred = is_preferred)
        temp = rbind(temp,df_temp)

        pos = pos + 1
      }

      tableList[[i]] = temp
    }
  }
  return(tableList)
}

server <- function(input, output) {
  inputList <- reactive({
    c(input$course1,input$course2,input$course3,input$course4,input$course5,input$course6,input$course7,input$course8)
  })

  data1 <- eventReactive(input$do, {
    get_data(c(input$course1,input$course2,input$course3,input$course4,input$course5,input$course6,input$course7,input$course8), input$time)
  })

  output$course_schedule<-renderPrint({
    req(!is.numeric(data1()))
    req(length(data1()) > 0)
    for(i in 1:length(data1())){
      if(check_all_preferred(data1()[[i]])){
        print(str_glue(">>>>>>>>>>>>>>>>>>>>>>>>Option {num_op}<<<<<<<<<<<<<<<<<<<<<<<<",num_op = i))
      }
      else{
        print(str_glue("========================Option {num_op}========================",num_op = i))
      }
      print(data1()[[i]])
    }

  })

  output$noclass<-renderPrint({
    req(is.numeric(data1()))
    req(inputList()[data1()] != "")
    if(data1() == 123){
      cat("Sorry, there is no combination!")
    }
    else{
      cat("Sorry, \"")
      cat(inputList()[data1()])
      cat("\" Is Not Found!")
    }
  })
}
shinyApp(ui = ui, server = server)
