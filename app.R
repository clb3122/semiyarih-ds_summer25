#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
  # title of app
  tags$h1("Homework Assignment", style = "color: red;"),
  p("By: Collin Brown"),
  tags$style("body {background-color: yellow;}"),
  
  # Sidebar variable selection for discrete and continuous variables
  sidebarLayout(
    sidebarPanel(
      selectInput("disc_var", "Discrete Variable", choices = c("cyl", "vs", "am", "gear", "carb")),
      selectInput("cont_var", "Continuous Variable", choices = c("mpg", "disp", "hp", "drat", "wt", "qsec")),
      sliderInput("bins", "Number of Bins", min = 1, max = 50, value = 20)
    ),
    
    # Main panel with tab navigation
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data", 
          tableOutput("datatable")),
        tabPanel(
          "Summary Statistics", 
          verbatimTextOutput("summary")),
        tabPanel(
          "Box Plot", 
          plotOutput("boxplot")),
        tabPanel(
          "Bar Plot", 
          plotOutput("barplot")),
        tabPanel(
          "Histogram", 
          plotOutput("histogram"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Data table of selected vars
  output$datatable <- renderTable({
    mtcars[, c(input$disc_var, input$cont_var)]
  })

  # summary
  output$summary <- renderPrint({
    cat("Discrete Variable Summary:\n")
    print(summary(mtcars[[input$disc_var]]))
    cat("\nContinuous Variable Summary:\n")
    print(summary(mtcars[[input$cont_var]]))
  })
  
  # box plot
  output$boxplot <- renderPlot({
    ggplot(mtcars, 
           aes_string(
             x = input$disc_var, 
             y = input$cont_var)) +
      geom_boxplot() +
      labs(
        title = paste("Boxplot of", input$cont_var, "by", input$disc_var),
        x = input$disc_var, 
        y = input$cont_var)
  })
  
  # bar chart
  output$barplot <- renderPlot({
    ggplot(mtcars, 
           aes_string(x = input$disc_var)) +
      geom_bar() +
      labs(
        title = paste("Bar Plot of", input$disc_var), 
           x = input$disc_var, 
           y = "Frequency")
  })
  
  # histogram
  output$histogram <- renderPlot({
    ggplot(mtcars, aes_string(x = input$cont_var)) +
      geom_histogram() +
      labs(
        title = paste("Histogram of", input$cont_var), 
           x = input$cont_var, y = "Count")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
