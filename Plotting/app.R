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
library(plyr)
library(tigris)
library(dplyr)
library(leaflet)
library(tidycensus)
library(tidyverse)
library(stringr)
library(viridis)
library(RColorBrewer)
options(scipen=999)





g.luPlotFunction <- function(year.g) {
  
  GoochlandAllParcel <- read_sf("../ShinyApp/data/luParcelData/GoochAll.shp")
  Gooch <- GoochlandAllParcel %>% filter(year == year.g)
  
  LUC_values <- c("Single Family Residential Urban", 
                  "Single Family Residential Suburban", 
                  "Multi-Family Residential", 
                  "Commerical / Industrial", 
                  "Agricultural / Undeveloped (20-99 Acres)", 
                  "Agricultural / Undeveloped (100+ Acres)", 
                  "Other", 
                  "Undefined")
  
  LUC_values <- factor(LUC_values, levels = LUC_values)
  
  mypalette <- colorBin(palette = "viridis", as.numeric(LUC_values), bins = 9)
  colors <- mypalette(unclass(LUC_values))
  colors[8] <- "#addc30"
  
  MyMap <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addPolygons(data = Gooch %>% filter(LUC_FIN == "Single Family Residential Urban"), 
                fillColor = colors[1], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Single Family Urban") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Single Family Residential Suburban"), 
                fillColor = colors[2], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Single Family Suburban") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Multi-Family Residential"), 
                fillColor = colors[3], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Multi-Family Residential") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Commerical / Industrial") ,
                fillColor = colors[4], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Commercial & Industrial") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Agricultural / Undeveloped (20-99 Acres)"),
                fillColor = colors[5], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Agriculture/Undeveloped (20-99 Acres)") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Agricultural / Undeveloped (100+ Acres)") ,
                fillColor = colors[6], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Agriculture/Undeveloped (100+ Acres)") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Other"),
                fillColor = colors[7], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Other") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Undefined") ,
                fillColor = colors[8], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Unknown") %>%
    addLayersControl(
      overlayGroups = c("Single Family Urban", "Single Family Suburban", "Multi-Family Residential", "Commercial & Industrial", "Agriculture/Undeveloped (20-99 Acres)", "Agriculture/Undeveloped (100+ Acres)", "Other", "Unknown"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)
    )
}






# ========================================================================================

# Define UI for application that draws a histogram
ui <- fluidPage(
  sliderInput(inputId = "luYear.g", label = "Year:", 
              min = 2018, max = 2021, value = 2018, 
              sep = ""),
  leafletOutput(outputId = "luPlot.g")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$luPlot.g <- renderLeaflet({
    luPlot <- g.luPlotFunction(input$luYear.g)
    luPlot
  }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
