

library(shiny)

ui <- fluidPage(
    
    titlePanel("Course Schedual Building Assistant"),
    
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId="course1",
                label="Full Course Name:",
                value="",
            ),
            textInput(
                inputId="course1",
                label="Full Course Name:",
                value="",
            ),
            textInput(
                inputId="course1",
                label="Full Course Name:",
                value="",
            ),
            textInput(
                inputId="course1",
                label="Full Course Name:",
                value="",
            ),
            actionButton("do", "Click Me")
        ),
        
        mainPanel(
            tableOutput("course_schedual")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({
  
})
}
shinyApp(ui = ui, server = server)
