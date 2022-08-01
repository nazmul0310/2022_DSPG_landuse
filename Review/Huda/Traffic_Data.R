library(shiny)
library(leaflet)
library(dplyr)
library(sf)

setwd("data") # you can change it. It only affects import county boundary data

#Read Cropland Data for Goochland


road_layers<- st_read("Traffic_Hotspot/Gooch_roads.shp")
road_layers<- st_transform(road_layers, "+proj=longlat +datum=WGS84")

gooch_traffic<- st_read("Traffic_Hotspot/Goochland_Traffic_Heatmap.shp")
gooch_traffic<- st_transform(gooch_traffic, "+proj=longlat +datum=WGS84")

pow_traffic<- st_read("Traffic_Hotspot/Powhatan_Traffic_Heatmap.shp")
pow_traffic<- st_transform(pow_traffic, "+proj=longlat +datum=WGS84")


leaflet()%>%
  addTiles() %>%
  setView(lng=-77.9739, lat=37.5016 , zoom=10.43) %>% 
  addPolygons(data=gooch_traffic,
              fillColor = "blue")


ui <- fluidPage(
  sliderInput(inputId = "year", 
              label = "Choose the starting and ending years",
              min = 2012,
              max = 2021,
              step = 9,
              value = 2021,
              sep = "", ticks = FALSE, width = "5%"),
  leafletOutput("mymap",height = 1000)
)




server <- function(input,output){
  
  output$mymap <- renderLeaflet({
    
    begin_year <- 2012
    end_year <- 2021
    yr <- c(begin_year,end_year)
    file_list <- paste(getwd(),"/data/Cropland/Gooch/Gooch_Ag_",yr,".shp",sep = "")
    
    for (file in file_list){
      #import the cropdata maps of the selected years
      gl<- st_read(file) 
      gl <- st_transform(gl, "+proj=longlat +datum=WGS84")
      m <- addPolygons(m,
                       stroke = FALSE,
                       data = gl,
                       weight = 1,
                       smoothFactor=1,
                       fillColor = "red",
                       fillOpacity = 0.1)
    }
    m
  })
  
}


shinyApp(ui, server)