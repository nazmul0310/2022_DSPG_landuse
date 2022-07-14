library(shiny)
library(leaflet)
library(dplyr)
library(sf)

setwd("C:/DSPG/LUSE_RSTUDIO/2022_DSPG_landuse/Review/Wen/Heatmap/www") # you can change it. It only affects import county boundary data

#Read county boundary map
gl_cnty<- st_read("Goochland_Boundary.shp")
gl_cnty <- st_transform(gl_cnty, "+proj=longlat +datum=WGS84") 

m <- leaflet()%>%
  addTiles() %>%
  setView(lng=-77.9739, lat=37.5016 , zoom=10.43) %>% 
  addPolygons(data=gl_cnty,
              fillColor = "transparent") 

ui <- fluidPage(
  sliderInput(inputId = "year", 
              label = "Choose the starting and ending years",
              min = as.numeric(2019),
              max = as.numeric(2022),
              step = 1,
              value = c(2019,2022)),
  leafletOutput("mymap",height = 1000)
)


server <- function(input,output){
  
  output$mymap <- renderLeaflet({
    
    begin_year <- input$year[1]-as.numeric(2000)
    end_year <- input$year[2]-2000
    yr <- c(begin_year:end_year)
    file_list <- paste(getwd(),"/www/gooch_hotspot_",yr,".shp",sep = "")
    
    for (file in file_list){
      #import the heatspot maps of the selected years
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