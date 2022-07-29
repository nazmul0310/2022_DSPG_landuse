# Libraries 

library(stringr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(rgdal)
library(plyr)
library(tigris)
library(dplyr)
library(leaflet)
library(tidyverse)
library(viridis)
library(readxl)
library(RColorBrewer)
library(highcharter)
library(sf) #for importing shp file
library(highcharter) #for transition matrix
library(htmlwidgets) #for transition matrix
library(mapview)
library(webshot)

## Data Imports ============================================

gl_cnty<- st_read("data/cnty_bndry/Goochland_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")
po_cnty<- st_read("data/cnty_bndry/Powhatan_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")

GoochlandAllParcel <- read_sf("data/luParcelData/GoochAll.shp")
names(GoochlandAllParcel) <- c("FIN_MLUSE", "year", "geometry")
PowhatanAllParcel <- read_sf("data/luParcelData/PowAll.shp")






tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))





## Plotting functions ======================================

luPlotFunction <- function(inputYear, county) {
  
  lu.plt <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # Sets view based on county
  if(county == "Powhatan"){
    lu.plt <- lu.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10.5) %>% addPolygons(data = po_cnty, stroke = TRUE, weight = 2, opacity = 1, fillOpacity = 0)
    parcelData <- PowhatanAllParcel %>% filter(year == inputYear)
  }
  else{
    lu.plt <- lu.plt %>% setView(lng=-77.885376, lat=37.73, zoom = 10.5) %>% addPolygons(data = gl_cnty, stroke = TRUE, weight = 2, opacity = 1, fillOpacity = 0)
    parcelData <- GoochlandAllParcel %>% filter(year == inputYear)
  }
  
  LUC_values <- c("Single Family Residential Urban",
                  "Single Family Residential Suburban",
                  "Multi-Family Residential",
                  "Commerical / Industrial",
                  "Agricultural / Undeveloped (20-99 Acres)",
                  "Agricultural / Undeveloped (100+ Acres)",
                  "Other",
                  "Undefined")
  
  LUC_values <- factor(LUC_values, levels = LUC_values)
  
  mypalette <- colorBin(palette = "viridis", as.numeric(LUC_values), bins = 8)
  colors <- mypalette(unclass(LUC_values))
  colors[8] <- "#4D4D4D" # undefined gets a grayed out color
  legendpalette <- colorFactor(palette = colors,levels=LUC_values)
  
  title <- tags$div(
    tag.map.title, HTML(as.character(inputYear))
  )  
  
  lu.plt <- lu.plt %>%
    addPolygons(data = parcelData %>% filter(FIN_MLUSE == "Single Family Residential Urban"),
                fillColor = colors[1], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Single Family Urban") %>%
    addPolygons(data=parcelData %>% filter(FIN_MLUSE == "Single Family Residential Suburban"),
                fillColor = colors[2], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Single Family Suburban") %>%
    addPolygons(data=parcelData %>% filter(FIN_MLUSE == "Multi-Family Residential"),
                fillColor = colors[3], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Multi-Family Residential") %>%
    addPolygons(data=parcelData %>% filter(FIN_MLUSE == "Commerical / Industrial") ,
                fillColor = colors[4], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Commercial & Industrial") %>%
    addPolygons(data=parcelData %>% filter(FIN_MLUSE == "Agricultural / Undeveloped (20-99 Acres)"),
                fillColor = colors[5], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Agriculture/Undeveloped (20-99 Acres)") %>%
    addPolygons(data=parcelData %>% filter(FIN_MLUSE == "Agricultural / Undeveloped (100+ Acres)") ,
                fillColor = colors[6], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Agriculture/Undeveloped (100+ Acres)") %>%
    addPolygons(data=parcelData %>% filter(FIN_MLUSE == "Other"),
                fillColor = colors[7], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Other") %>%
    addPolygons(data=parcelData %>% filter(FIN_MLUSE == "Undefined") ,
                fillColor = colors[8], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Undefined") %>%
    addLegend(data=parcelData, "bottomleft",
              pal = legendpalette, values = LUC_values,
              title = "Land Use Type",
              labFormat = labelFormat(),
              opacity = 1) %>%
    addControl(title, position = "topleft", className="map-title")
  lu.plt
}



## PNG Saving ===============================================


# Goochland Land use

filenames <- c(paste0("data/luParcelData/luPNGs/Gooch_LU", 18:21, ".png"))
for(year in 2018:2021){
  
  curr.plt <- luPlotFunction(year, "Goochland")
  
  saveWidget(curr.plt, "data/luParcelData/luPNGs/temp.html", selfcontained = FALSE, title = "2018")
  webshot("data/luParcelData/luPNGs/temp.html", file = filenames[year-2017],
          cliprect = "viewport")
}

# Powhatan Land use

filenames <- c(paste0("data/luParcelData/luPNGs/Pow_LU", 15:21, ".png"))
for(year in 2015:2021){

  curr.plt <- luPlotFunction(year, "Powhatan")

  saveWidget(curr.plt, "data/luParcelData/luPNGs/temp.html", selfcontained = FALSE)
  webshot("data/luParcelData/luPNGs/temp.html", file = filenames[year-2014],
          cliprect = "viewport")
}


