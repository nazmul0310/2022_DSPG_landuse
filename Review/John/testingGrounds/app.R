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

setwd("../")

travelTimes <- st_read("Powhatan_Parcel_Data/Powhatan_Travel_Time/Powhatan_Travel_Times.shp")
Rmnd30 <- st_read("Powhatan_Parcel_Data/Richmond_Travel_Times/30MinuteTravelTime.shp")
Rmnd45 <- st_read("Powhatan_Parcel_Data/Richmond_Travel_Times/45MinuteTravelTime.shp")
Rmnd60 <- st_read("Powhatan_Parcel_Data/Richmond_Travel_Times/60MinuteTravelTime.shp")



# Define UI for application that draws a histogram
ui <- fluidPage(
  radioButtons(inputId = "drivingTimeInput", label = "Select Time from Richmond: ", 
                       choices = c("Within 30 minutes", "Within 45 minutes", "Within 60 minutes", "More than 60 mintues"), 
                       selected = "More than 60 minutes"),
          
  leafletOutput("p.drivingPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Driving Time
  # ==================================================================
  
  drvTimeInput <- reactive({
    input$drivingTimeInput
  })
  
  driveRange <- drvTimeInput()
  
  if(driveRange == "Within 30 minutes"){
    travelTimes <- travelTimes %>% filter(Trv2RcMnd == "30 minutes")
    drv.color <- "#35b779"
    iso.file <- Rmnd30
  }
  else if(driveRange == "Within 45 minutes"){
    travelTimes <- travelTimes %>% filter(Trv2RcMnd == c("30 minutes", "45 minutes"))
    drv.color <- "#31688e"
    iso.file <- Rmnd45
  }
  else if(driveRange == "Within 60 minutes"){
    travelTimes <- travelTimes %>% filter(Trv2RcMnd == c("30 minutes", "45 minutes", "One hour"))
    drv.color <- "#440154"
    iso.file <- Rmnd60
  }
  
  
  # Function============
  drivingPlot.func <- function(data, county, richmondISO, lnColor){
    
    driving.plt <- leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri") %>%
      addPolygons(richmondISO, fillColor = "transparent") %>%
      addPolygons(data, color = lnColor)
    driving.plt

  }
  
  
  # Output=========
  output$p.drivingPlot <- renderLeaflet({
    
    drivingPlot.func(travelTimes, "Powhatan", iso.file, drv.color)
    
  })
  
  
  
  # ==================================================================
  
}

# Run the application 
shinyApp(ui = ui, server = server)
