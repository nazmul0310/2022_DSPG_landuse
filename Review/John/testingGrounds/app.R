#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(shinyjs)
library(ggplot2)
library(plotly)
library(rsconnect)
library(rgdal)
library(sf)
library(plyr)
library(tigris)
library(dplyr)
library(leaflet)
library(tidycensus)
library(tidyverse)
library(stringr)
library(viridis)
library(readxl)
library(RColorBrewer)
options(scipen=999)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test Grounds for Current Project Implementations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "p.parcellationRange",
                        label = "Years of Parcellation:",
                        min = 2012,
                        max = 2020,
                        value = c(2012, 2020),
                        sep = "", 
                        width = "150%")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("p.parcellationPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$p.parcellationPlot <- renderLeaflet({
    pow_parcellation <- read_sf("../Powhatan_Parcel_Data/Powhatan_all_parcellation_Lite/Powhatan_Parcellation_LT.shp") %>%
      st_transform(crs = st_crs("EPSG:4326"))
    pow_parcellation$year <- substr(pow_parcellation$UNIQ_ID_12, 1, 4)
    
    yearRange <- input$p.parcellationRange[1]:input$p.parcellationRange[2]
    
    
    
    parc.func <- function(data, range){
      
      # Declares initial leaflet, nothing added to it.
      my.parc.plt <- leaflet()%>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng=-77.9188, lat=37.5415 , zoom=10)
      
      # for loop to add polygons based on what the max year is vs. subsequent years prior
      for(i in range){
        # Adds most recent year's parcellations
        if(i == max(range)){
          my.parc.plt <- my.parc.plt %>%
            addPolygons(data = data %>% filter(year == i), 
                        fillColor = "red", smoothFactor = 0.1, fillOpacity = 1, stroke = FALSE)
        }
        # Adds subsequent year's parcellations
        else {
          my.parc.plt <- my.parc.plt %>%
            addPolygons(data = data %>% filter(year == i), 
                        fillColor = "red", smoothFactor = 0.1, fillOpacity = .25, stroke = FALSE)
        }
      }
      my.parc.plt
    }
    parc.func(pow_parcellation, yearRange)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
