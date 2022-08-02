# File used for all plots that are seen in LandUse_2022DSPG project that aren't within app.r script:
# Mainly used for leaflets

## LAND USE ============================================================

luPlotFunction <- function(inputYear, county) {
  
  lu.plt <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # Sets view based on county
  if(county == "Powhatan"){
    lu.plt <- lu.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10) %>% addPolygons(data = po_cnty, fillOpacity = 0)
    parcelData <- PowhatanAllParcel %>% filter(year == inputYear)
  }
  else{
    lu.plt <- lu.plt %>% setView(lng=-77.885376, lat=37.684143, zoom = 10) %>% addPolygons(data = gl_cnty, fillOpacity = 0)
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
  
  lu.plt <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
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
    addLayersControl(
      overlayGroups = c("Single Family Urban", "Single Family Suburban", "Multi-Family Residential", "Commercial & Industrial", "Agriculture/Undeveloped (20-99 Acres)", "Agriculture/Undeveloped (100+ Acres)", "Other", "Undefined"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend(data=parcelData, "bottomright",
              pal = legendpalette, values = LUC_values,
              title = "Land Use Type",
              labFormat = labelFormat(),
              opacity = 1)
  lu.plt
}

## TRAFFIC VOL ============================================================

trafficVol.func <- function(county){
  trafficVol.plt <- leaflet() %>%
    addTiles()
  
  if(county == "Powhatan"){
    trafficVol.plt <- trafficVol.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10) %>% addPolygons(data = po_cnty, fillOpacity = 0)
    roadData <- proads
    trafficData <- ptraffic
  }
  else{
    trafficVol.plt <- trafficVol.plt %>% setView(lng=-78, lat=37.75, zoom = 10) %>% addPolygons(data = gl_cnty, fillOpacity = 0)
    roadData <- groads
    trafficData <- gtraffic
  }
  
  trafficVol.plt <- trafficVol.plt %>%
    addPolygons(data=roadData, weight=1, color = "black", fillOpacity=0)%>%
    addPolygons(data=filter(trafficData, gridcode==1), weight=0, fillOpacity = 0.5, fillColor = "green", group = "Less than 1,000")%>%
    addPolygons(data=filter(trafficData, gridcode==2), weight=0, fillOpacity = 0.5, fillColor = "yellow", group = "1,000 to 5,000")%>%
    addPolygons(data=filter(trafficData, gridcode==3), weight=0, fillOpacity = 0.5, fillColor = "orange", group = "5,000 to 10,000")%>%
    addPolygons(data=filter(trafficData, gridcode==4), weight=0, fillOpacity = 0.5, fillColor = 'red', group= "10,000 to 25,000")%>%
    addPolygons(data=filter(trafficData, gridcode==5), weight=0, fillOpacity = 0.5, fillColor ='maroon', group= "More than 25,000")%>%
    addLegend(position = "bottomright", labels = c("Less than 1,000", "1,000 to 5,000", "5,000 to 10,000", "10,000 to 25,000", "More than 25,000"),
              colors = c("green", "yellow", "orange", "red", "maroon"))
  trafficVol.plt
}

## TRAVEL TIME ============================================================



travelTime.func <- function(county){
  
  # Initial plot
  travelTime.plt <- leaflet() %>%
    addTiles()
  
  if(county == "Powhatan"){
    travelTime.plt <- travelTime.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10)
    data <- pow.travelTimes
  } else {
    travelTime.plt <- travelTime.plt %>% setView(lng=-78, lat=37.75, zoom = 10)
    data <- gooch.travelTimes
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
    addLegend(position = "bottomright", labels = c("Within 30 minutes", "Within 45 minutes", "Within 1 hour", "More than 1 hour"), 
              colors = sorted_colors)
  
  travelTime.plt
}