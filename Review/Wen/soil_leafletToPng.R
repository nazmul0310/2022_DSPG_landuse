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

#gl_cnty<- st_read("D:/2022_DSPG_landuse/ShinyApp/data/cnty_bndry/Goochland_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")
#po_cnty<- st_read("D:/2022_DSPG_landuse/ShinyApp/data/cnty_bndry/Powhatan_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")

g.soil <- read_sf("D:/2022_DSPG_landuse/Review/Wen/Soil Qual/gooch_soil_merge.shp") %>% select(NirrCpCls, geometry)%>% st_transform("+proj=longlat +datum=WGS84")
p.soil <- read_sf("D:/2022_DSPG_landuse/Review/Wen/Soil Qual/pow_soil_merge.shp") %>% select(NirrCpCls, geometry)



## Plotting functions ======================================

luPlotFunction <- function(county) {
  
  lu.plt <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # Sets view based on county
  if(county == "Powhatan"){
    lu.plt <- lu.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10.5) 
    parcelData <- p.soil
  }
  else{
    lu.plt <- lu.plt %>% setView(lng=-77.885376, lat=37.73, zoom = 10.5) 
    parcelData <- g.soil
  }
  
  SoilClass <- c("Good Agriculture",
                  "Limited Agriculture",
                  "Pasture, rangeland, and wildlife habitat",
                 "NODATA")
  
  SoilClass <- factor(SoilClass, levels = SoilClass)
  
  mypalette <- colorBin(palette = "viridis", as.numeric(SoilClass), bins = 4)
  colors <- mypalette(unclass(SoilClass))
  colors[4] <- "#4D4D4D" # undefined gets a grayed out color
  legendpalette <- colorFactor(palette = colors,levels=SoilClass)
  
  
  lu.plt <- lu.plt %>%
    addPolygons(data = parcelData %>% filter(NirrCpCls == "1, 2"),
                fillColor = colors[1], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Good Agriculture") %>%
    addPolygons(data=parcelData %>% filter(NirrCpCls == "3, 4"),
                fillColor = colors[2], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Limited Agriculture") %>%
    addPolygons(data=parcelData %>% filter(NirrCpCls == "5, 6, 7, 8"),
                fillColor = colors[3], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Pasture, rangeland, and wildlife habitat") %>% 
    addPolygons(data=parcelData %>% filter(NirrCpCls == "NoData"),
                fillColor = colors[4], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "NODATA") %>% 
    addLegend(data=parcelData, "bottomleft",
              pal = legendpalette, values = SoilClass,
              title = "Soil Quality Class",
              labFormat = labelFormat(),
              opacity = 1) 
  lu.plt
}



## PNG Saving ===============================================


# Goochland Land use

filenames <- "D:/2022_DSPG_landuse/ShinyApp/data/Soil_Quality/Goochland.png"
curr.plt <- luPlotFunction("Goochland")
  
  saveWidget(curr.plt, "D:/2022_DSPG_landuse/ShinyApp/data/Soil_Quality/Goochland/temp.html", selfcontained = FALSE)
  webshot("D:/2022_DSPG_landuse/ShinyApp/data/Soil_Quality/Goochland/temp.html", file = filenames,
          cliprect = "viewport")
# Powhatan Land use

filenames <- "D:/2022_DSPG_landuse/ShinyApp/data/Soil_Quality/Powhatan.png"

  curr.plt <- luPlotFunction("Powhatan")
  saveWidget(curr.plt, "D:/2022_DSPG_landuse/ShinyApp/data/Soil_Quality/Powhatan/temp.html", selfcontained = FALSE)
  webshot("D:/2022_DSPG_landuse/ShinyApp/data/Soil_Quality/Powhatan/temp.html", file = filenames,
          cliprect = "viewport")



