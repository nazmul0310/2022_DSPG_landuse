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

  # Land use
GoochlandAllParcel <- read_sf("data/luParcelData/GoochAll.shp")
names(GoochlandAllParcel) <- c("FIN_MLUSE", "year", "geometry")
PowhatanAllParcel <- read_sf("data/luParcelData/PowAll.shp")

  # Crop layer

g.cropMap12 <- read_sf("data/Cropland/Gooch/Gooch_Ag_2012.shp") %>% st_transform("+proj=longlat +datum=WGS84") %>% rename(cropLabel = New_Label)
g.cropMap21 <- read_sf("data/Cropland/Gooch/Gooch_Ag_2021.shp") %>% st_transform("+proj=longlat +datum=WGS84") %>% rename(cropLabel = New_Label) %>% select(-Comb_class)

p.cropMap12 <- read_sf("data/Cropland/Pow/Powhatan_Ag_2012.shp") %>% st_transform("+proj=longlat +datum=WGS84") %>% rename(cropLabel = New.Label) %>% select(-c(Id, gridcode))
p.cropMap21 <- read_sf("data/Cropland/Pow/Powhatan_Ag_2021.shp") %>% st_transform("+proj=longlat +datum=WGS84") %>% rename(cropLabel = Comb_Class) %>% select(-c(Id, gridcode))



  # map title
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
gl_cnty<- st_read("data/cnty_bndry/Goochland_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")
po_cnty<- st_read("data/cnty_bndry/Powhatan_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84")


  ### Land Use ===============================================

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


  ### Crop layer ==================================================


cropLayerFunction <- function(inputYear, county){
  cropLabels <- unique(sort(c(g.cropMap21$cropLabel, g.cropMap12$cropLabel, p.cropMap21$cropLabel, p.cropMap12$cropLabel)))
  mapPal <- colorFactor(palette = "viridis", factor(cropLabels))
  
  title <- tags$div(
    tag.map.title, HTML(as.character(inputYear))
  )  
  
  my.crop.plt<- leaflet(options = leafletOptions(zoomControl = FALSE))%>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  if(county == "Powhatan"){
    my.crop.plt <- my.crop.plt %>%
      addPolygons(data = po_cnty, fillColor = "transparent", weight = 2, color = "Black") %>% 
      setView(lng=-77.8888, lat=37.5315 , zoom=11)
    legend.pos <- "bottomleft"
    if(inputYear == 2012){
      data <- p.cropMap12 %>% group_by(cropLabel)
    }
    else{
      data <- p.cropMap21 %>% group_by(cropLabel)
    }
  }
  else{
    legend.pos <- "topright"
    my.crop.plt <- my.crop.plt %>%
      addPolygons(data = gl_cnty, fillColor = "transparent", weight = 2, color = "Black") %>%
      setView(lng=-77.885376, lat=37.73143, zoom = 11)
    if(inputYear == 2012){
      data <- g.cropMap12 %>% group_by(cropLabel)
    }
    else{
      data <- g.cropMap21 %>% group_by(cropLabel)
    }
  }
  
  my.crop.plt <- my.crop.plt %>% 
    addPolygons(data = data, color = mapPal(cropLabels), fillOpacity = 1, smoothFactor = .1, stroke = FALSE) %>%
    addLegend(position = legend.pos, color = mapPal(cropLabels), labels = cropLabels) %>%
    addControl(title, position = "topleft", className="map-title")
  
  my.crop.plt
}


## PNG Saving ===============================================


# Goochland Land use

filenames <- c(paste0("data/luParcelData/luPNGs/Gooch_LU", 18:21, ".png"))
for(year in 2018:2021){
  
  curr.plt <- luPlotFunction(year, "Goochland")
  
  saveWidget(curr.plt, "data/luParcelData/luPNGs/temp.html", selfcontained = FALSE)
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


curr.plt <- cropLayerFunction(2012, "Powhatan")
saveWidget(curr.plt, "data/Cropland/CroplandPngs/temp.html", selfcontained = FALSE)
webshot("data/Cropland/CroplandPngs/temp.html", file = "data/Cropland/CroplandPngs/powCrop12.png", 
        cliprect = "viewport")

curr.plt <- cropLayerFunction(2021, "Powhatan")
saveWidget(curr.plt, "data/Cropland/CroplandPngs/temp.html", selfcontained = FALSE)
webshot("data/Cropland/CroplandPngs/temp.html", file = "data/Cropland/CroplandPngs/powCrop21.png",
        cliprect = "viewport")


curr.plt <- cropLayerFunction(2012, "Goochland")
saveWidget(curr.plt, "data/Cropland/CroplandPngs/temp.html", selfcontained = FALSE)
webshot("data/Cropland/CroplandPngs/temp.html", file = "data/Cropland/CroplandPngs/goochCrop12.png")

curr.plt <- cropLayerFunction(2021, "Goochland")
saveWidget(curr.plt, "data/Cropland/CroplandPngs/temp.html", selfcontained = FALSE)
webshot("data/Cropland/CroplandPngs/temp.html", file = "data/Cropland/CroplandPngs/goochCrop21.png")









pow.travelTimes <- st_read("data/travelTimes/Powhatan_Travel_Time/Powhatan_Travel_Times.shp") %>% st_transform("+proj=longlat +datum=WGS84")
gooch.travelTimes <- st_read("data/travelTimes/Goochland_Travel_Time/Goochland_Travel_Times.shp") %>% st_transform("+proj=longlat +datum=WGS84")


travelTime.func <- function(county){
  
  # Initial plot
  travelTime.plt <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles()
  
  if(county == "Powhatan"){
    travelTime.plt <- travelTime.plt %>% setView(lng=-77.8888, lat=37.5315 , zoom=11) %>% addPolygons(data = po_cnty, fillColor = "transparent", weight = 2, color = "Black") 
    position <- "bottomright"
    data <- pow.travelTimes
  } else {
    travelTime.plt <- travelTime.plt %>% setView(lng=-77.885376, lat=37.73143, zoom = 11) %>% addPolygons(data = gl_cnty, fillColor = "transparent", weight = 2, color = "Black") 
    data <- gooch.travelTimes
    position <- "bottomleft"
  }
  
  
  data$Trv2RcMnd <- factor(data$Trv2RcMnd, levels = c("30 minutes", "45 minutes", "One hour", "More than an hour"))
  # Custom color palette
  mypalette <- colorBin(palette = "viridis", as.numeric(data$Trv2RcMnd), bins = 5)
  colors <- mypalette(unclass(data$Trv2RcMnd))
  sorted_colors <- c("#440154", "#2A788E", "#7AD151", "#FDE725")
  
  travelTime.plt <- travelTime.plt %>%
    addPolygons(data = data, color = "black",
                fillColor = colors,
                smoothFactor = 0.1, fillOpacity=.6, weight = 1,stroke = FALSE) %>%
    addCircleMarkers(lng = -77.44071077873014, lat = 37.534379575044426, label = "Richmond") %>%
    addLegend(position = position, labels = c("Within 30 minutes", "Within 45 minutes", "Within 1 hour", "More than 1 hour"), 
              colors = sorted_colors)
  
  travelTime.plt
}

curr.plt <- travelTime.func("Goochland")
saveWidget(curr.plt, "www/trafficPNGs/temp.html", selfcontained = FALSE)
webshot("www/trafficPNGs/temp.html", file = "www/trafficPNGs/goochProx.png")

curr.plt <- travelTime.func("Powhatan")
saveWidget(curr.plt, "www/trafficPNGs/temp.html", selfcontained = FALSE)
webshot("www/trafficPNGs/temp.html", file = "www/trafficPNGs/powProx.png")




filenames <- c(paste0("data/Cropland/CroplandPngs/", "goochCrop", c("12", "21"), ".png"),
               paste0("data/Cropland/CroplandPngs/", "powCrop", c("12", "21"), ".png"))
for(i in 1:2){
  
  curr.plt <- cropLayerFunction(year, "Powhatan")
  saveWidget(curr.plt, "data/Cropland/CroplandPngs/temp.html", selcontained = FALSE)
  webshot("data/Cropland/CroplandPgs/temp.html", file = filenames[])
}



gtraffic<- st_read("data/Traffic_Hotspot/Goochland_Traffic_Heatmap.shp") %>% st_transform("+proj=longlat +datum=WGS84")
groads<- st_read("data/Traffic_Hotspot/Gooch_roads.shp") %>% st_transform("+proj=longlat +datum=WGS84")
ptraffic<- st_read("data/Traffic_Hotspot/Powhatan_Traffic_Heatmap.shp") %>% st_transform("+proj=longlat +datum=WGS84")
proads<- st_read("data/Traffic_Hotspot/Pow_roads.shp") %>%  st_transform("+proj=longlat +datum=WGS84")

trafficVol.func <- function(county){
  trafficVol.plt <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addTiles()
  
  if(county == "Powhatan"){
    trafficVol.plt <- trafficVol.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=11) %>% addPolygons(data = po_cnty, fillOpacity = 0)
    roadData <- proads
    trafficData <- ptraffic
    position <- "bottomright"
  }
  else{
    trafficVol.plt <- trafficVol.plt %>% setView(lng=-77.885376, lat=37.73143, zoom = 11) %>% addPolygons(data = gl_cnty, fillOpacity = 0)
    roadData <- groads
    trafficData <- gtraffic
    position <- "bottomleft"
  }
  
  trafficVol.plt <- trafficVol.plt %>%
    addPolygons(data=roadData, weight=1, color = "black", fillOpacity=0)%>%
    addPolygons(data=filter(trafficData, gridcode==1), weight=0, fillOpacity = 0.5, fillColor = "green", group = "Less than 1,000")%>%
    addPolygons(data=filter(trafficData, gridcode==2), weight=0, fillOpacity = 0.5, fillColor = "yellow", group = "1,000 to 5,000")%>%
    addPolygons(data=filter(trafficData, gridcode==3), weight=0, fillOpacity = 0.5, fillColor = "orange", group = "5,000 to 10,000")%>%
    addPolygons(data=filter(trafficData, gridcode==4), weight=0, fillOpacity = 0.5, fillColor = 'red', group= "10,000 to 25,000")%>%
    addPolygons(data=filter(trafficData, gridcode==5), weight=0, fillOpacity = 0.5, fillColor ='maroon', group= "More than 25,000")%>%
    addLegend(position = position, labels = c("Less than 1,000", "1,000 to 5,000", "5,000 to 10,000", "10,000 to 25,000", "More than 25,000"),
              colors = c("green", "yellow", "orange", "red", "maroon"))
  trafficVol.plt
}

curr.plt <- trafficVol.func("Powhatan")
saveWidget(curr.plt, "www/trafficPNGs/temp.html", selfcontained = FALSE)
webshot("www/trafficPNGs/temp.html", file = "www/trafficPNGs/powVol.png")

curr.plt <- trafficVol.func("Goochland")
saveWidget(curr.plt, "www/trafficPNGs/temp.html", selfcontained = FALSE)
webshot("www/trafficPNGs/temp.html", file = "www/trafficPNGs/goochVol.png")

