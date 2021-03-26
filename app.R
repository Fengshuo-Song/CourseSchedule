
library(tidyverse)
library(rvest)
library(RSelenium)
library(stringr)
library(shiny)

source("course_combination.R")

df<-read_csv("data_final.csv")
ui <- fluidPage(
    
    titlePanel("Course Schedual Building Assistant"),
    
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
            textOutput("course_schedual")
        )
    )
)


server <- function(input, output) {
  input1<-reactive(toupper(input$course1))
  input2<-reactive(toupper(input$course2))
  input3<-reactive(toupper(input$course3))
  input4<-reactive(toupper(input$course4))
  input5<-reactive(toupper(input$course5))
  input6<-reactive(toupper(input$course6))
  input7<-reactive(toupper(input$course7))
  input8<-reactive(toupper(input$course8))
  input = c(input1,input2,input3,input4,input5,input6,input7,input8)
  df_list = list()
  course_num = 0
  output$noclass<-renderText(
    if (input$do == 0){
      return()
    }
    isolate({
      for (i in input){
        temp=df%>%filter(course_name == i)
        if(i == ""){
          next
        }
        if (nrow(temp) == 0){
          cat(i)
          print("NOT FOUND")
        }
      }
    })
  )
  
  output$course_schedual<-renderPrint(
    input$do
    
  )
}
shinyApp(ui = ui, server = server)





