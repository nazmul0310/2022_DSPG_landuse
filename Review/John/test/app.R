#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(usmap)

sidebar <- dashboardSidebar(titlePanel("Interns"), textInput("name", "Enter your name:", value = "Christopher"),
                            selectInput("state", "Enter your State:", choices = c("Virginia", "Alabama"))
                            )

body <- dashboardBody(plotOutput("myplot"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Land Use"),
  sidebar = sidebar,
  body = body



   
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$myplot <- renderPlot({plot_usmap(include = "AL") + ggtitle(sprintf("%s's plot in %s", input$name, input$state))})

}

# Run the application 
shinyApp(ui,  server)
