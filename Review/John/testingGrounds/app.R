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

setwd("../../../ShinyApp")

pow.travelTimes <- st_read("data/travelTimes/Powhatan_Travel_Time/Powhatan_Travel_Times.shp") %>% st_transform("+proj=longlat +datum=WGS84")
gooch.travelTimes <- st_read("data/travelTimes/Goochland_Travel_Time/Goochland_Travel_Times.shp") %>% st_transform("+proj=longlat +datum=WGS84")
Rmnd30 <- st_read("data/travelTimes/Richmond_Travel_Times/30MinuteTravelTime.shp") %>% st_transform("+proj=longlat +datum=WGS84")
Rmnd45 <- st_read("data/travelTimes/Richmond_Travel_Times/45MinuteTravelTime.shp") %>% st_transform("+proj=longlat +datum=WGS84")
Rmnd60 <- st_read("data/travelTimes/Richmond_Travel_Times/60MinuteTravelTime.shp") %>% st_transform("+proj=longlat +datum=WGS84")

travelTime.func <- function(data, county){
  
  # Initial plot
  travelTime.plt <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri")  
  
  if(county == "Powhatan"){
    travelTime.plt <- travelTime.plt %>% setView(lng=-77.885376, lat=37.684143, zoom = 10)
  }
  else{
    travelTime.plt <- travelTime.plt %>% setView(lng=-77.949, lat=37.742, zoom=10)
  }
  
  data$Trv2RcMnd <- factor(data$Trv2RcMnd, levels = c("30 minutes", "45 minutes", "One hour", "More than an hour"))
  # Custom color palette
  mypalette <- colorBin(palette = "viridis", as.numeric(data$Trv2RcMnd), bins = 5)
  colors <- mypalette(unclass(data$Trv2RcMnd))
  sorted_colors <- c("#440154", "#2A788E", "#7AD151", "#FDE725")
  
  travelTime.plt <- travelTime.plt %>%
    addPolygons(data = data, color = "black",
                fillColor = colors,
                smoothFactor = 0.1, fillOpacity=.6, weight = 1,stroke = T) %>%
    
    addPolygons(data = Rmnd30, color = "Yellow",
                opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1, 
                group = "Within 30") %>%
    addPolygons(data = Rmnd45, color = "Orange",
                opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1,
                group = "Within 45") %>%
    addPolygons(data = Rmnd60, color = "Red",
                opacity = 1, weight = 2, fillColor = "white",fillOpacity = .1,
                group = "Within 60")
  
  
  
  travelTime.plt
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput(outputId = "p.travelPlot"),
  leafletOutput(outputId = "g.travelPlot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$p.travelPlot <- renderLeaflet({
    
    travelTime.func(pow.travelTimes, "Powhatan")
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
