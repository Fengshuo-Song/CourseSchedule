library(tidyverse)
library(rvest)
library(RSelenium)
library(stringr)
library(shiny)

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
numop <<- 1
get_comb <- function(df_list, total, current, timeList, CRNList){

  if(current > total){
    combList <<- append(combList,CRNList)
    print(str_glue("========Option {num_op}========",num_op = numop))
    for(j in 1:length(CRNList)){
      print(df_list[[j]] %>% filter(CRN == CRNList[j]) %>%
              select(course_name,Section, format.time))

    }
    numop <<- numop + 1
    return(0)
  }
  curr_df = df_list[[current]]
  for(i in 1:nrow(curr_df)){
    if(!is_timeList_conflict(curr_df[i,"format.time"],timeList)){
      get_comb(df_list,total,current+1,add_time(timeList,curr_df[i,"format.time"]),append(CRNList,curr_df[i,"CRN"]))
    }
  }
}


df<-read_csv("data_final.csv")
ui <- fluidPage(

    titlePanel("Course Schedule Building Assistant"),

    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId="course1",
                label="Full Course Name:",
                value=""
            ),
            textInput(
                inputId="course2",
                label="Full Course Name:",
                value=""
            ),
            textInput(
                inputId="course3",
                label="Full Course Name:",
                value=""
            ),
            textInput(
                inputId="course4",
                label="Full Course Name:",
                value=""
            ),
            textInput(
              inputId="course5",
              label="Full Course Name:",
              value=""
            ),
            textInput(
              inputId="course6",
              label="Full Course Name:",
              value=""
            ),
            textInput(
              inputId="course7",
              label="Full Course Name:",
              value=""
            ),
            textInput(
              inputId="course8",
              label="Full Course Name:",
              value=""
            ),
            actionButton("do", "Click Me")
        ),

        mainPanel(
            textOutput("noclass"),
            verbatimTextOutput("course_schedual")
        )
    )
)

get_data <- function(input_list){
  numop <<- 1
  df = read.csv("data_final.csv")
  df_list = list()
  course_num = 0
  for(item in input_list){
    if(item == ""){
      next
    }
    temp = df %>% filter(course_name == item)
    if(nrow(temp) == 0){
      cat(item)
      cat(" Not Found!\n")
    }
    else{
      course_num = course_num + 1
      df_list[[course_num]] = temp
    }
  }
  get_comb(df_list,course_num,1,"",c())
  return(df_list)
}

get_comb_list <- function(input_list){

  return(combList)
}

server <- function(input, output) {
  data1 <- eventReactive(input$do, {
    get_data(c(input$course1,input$course2,input$course3,input$course4,input$course5,input$course6,input$course7,input$course8))
  })
  # data2 <- eventReactive(input$do, {
  #   get_comb_list()
  # })
  # input1<-reactive(toupper(input$course1))
  # input2<-reactive(toupper(input$course2))
  # input3<-reactive(toupper(input$course3))
  # input4<-reactive(toupper(input$course4))
  # input5<-reactive(toupper(input$course5))
  # input6<-reactive(toupper(input$course6))
  # input7<-reactive(toupper(input$course7))
  # input8<-reactive(toupper(input$course8))
  # input = c(input1,input2,input3,input4,input5,input6,input7,input8)
  # df_list = list()
  # course_num = 0
  # output$noclass<-renderText(
  #   if (input$do == 0){
  #     return()
  #   }
  #   isolate({
  #     for (i in input){
  #       temp=df%>%filter(course_name == i)
  #       if(i == ""){
  #         next
  #       }
  #       if (nrow(temp) == 0){
  #         cat(i)
  #         print("NOT FOUND")
  #       }
  #     }
  #   })
  # )
  #
  output$course_schedual<-renderPrint(
    print(length(data1()))

  )
}
shinyApp(ui = ui, server = server)





