library(sf)
library(ggplot2)
library(raster)
library(leaflet)

setwd("C:/DSPG/Ag_Data")

# Reading each year's parcellation hotspots

gooch_ag_2012 <- st_read(  "Goochland_Ag_2012.shp")
gooch_ag_2021 <- st_read(  "Goochland_Ag_2021.shp")
pow_ag_2012 <- st_read(  "Powhatan_Ag_2012.shp")
pow_ag_2021 <- st_read(  "Powhatan_Ag_2021.shp")

# Plotting the files together in ggplot
ggplot() + 
  geom_sf(data = gooch_ag_2012, fill = "red", color = "NA", alpha = 1/10) +
  geom_sf(data = gooch_ag_2021, fill = "red", color = "NA", alpha = 1/10) +
  geom_sf(data = pow_ag_2012, fill = "red", color = "NA", alpha = 1/10) +
  geom_sf(data = pow_ag_2021, fill = "red", color = "NA", alpha = 1/10) +
  scale_color_manual(values = road_colors) +
  scale_fill_manual(values = "black") +
  ggtitle("Parcellation Hotspots in Goochland", subtitle = "2018 ~ 2022") + 
  coord_sf()


# Reprojecting as leaflet only works in this projection system
gl_hs_19_rp <- st_transform(gooch_ag_2012, 4326)
gl_hs_20_rp <- st_transform(gooch_ag_2021, 4326)
gl_hs_21_rp <- st_transform(pow_ag_2012, 4326)
gl_hs_22_rp <- st_transform(pow_ag_2021, 4326)

pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)

leaflet(gl_hs_22_rp) %>%
  
  addPolygons(
    stroke = FALSE, # remove polygon borders
    #fillColor = ~pal_fun(gridcode), # set fill color with function from above and value
    fillOpacity = 0.5, smoothFactor = 0.5,
  ) %>% # Works until this point but doesn't add tiles
  
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto")

# Need to put multiple shapefiles with leaflet and have the tiles in the background
