library(sf)
library(ggplot2)
library(raster)
library(leaflet)
library(tigris)

# Reading each year's parcellation hotspots

gl_hs_19 <- st_read(  "gooch_hotspot_19.shp")
gl_hs_20 <- st_read(  "gooch_hotspot_20.shp")
gl_hs_21 <- st_read(  "gooch_hotspot_21.shp")
gl_hs_22 <- st_read(  "gooch_hotspot_22.shp")

# Plotting the files together in ggplot
ggplot() + 
  geom_sf(data = gl_hs_19, fill = "red", color = "NA", alpha = 1/10) +
  geom_sf(data = gl_hs_20, fill = "red", color = "NA", alpha = 1/10) +
  geom_sf(data = gl_hs_21, fill = "red", color = "NA", alpha = 1/10) +
 # scale_color_manual(values = road_colors) +
  scale_fill_manual(values = "black") +
  ggtitle("Parcellation Hotspots in Goochland", subtitle = "2018 ~ 2022") + 
  coord_sf()


# Reprojecting as leaflet only works in this projection system
gl_hs_19_rp <- st_transform(gl_hs_19, 4326)
gl_hs_20_rp <- st_transform(gl_hs_20, 4326)
gl_hs_21_rp <- st_transform(gl_hs_21, 4326)
gl_hs_22_rp <- st_transform(gl_hs_22, 4326)

goochland.sf <- county_subdivisions(state = "VA", county = "Goochland")

leaflet() %>%

  addPolygons(data = gl_hs_22_rp, fillColor = "red",
    stroke = FALSE, # remove polygon borders
    #fillColor = ~pal_fun(gridcode), # set fill color with function from above and value
    fillOpacity = 0.2, smoothFactor = 0.5,
  ) %>%
  addPolygons(data = gl_hs_21_rp, fillColor = "red",
            stroke = FALSE, # remove polygon borders
            #fillColor = ~pal_fun(gridcode), # set fill color with function from above and value
            fillOpacity = 0.2, smoothFactor = 0.5,
  ) %>% 
  addPolygons(data = gl_hs_20_rp, fillColor = "red",
              stroke = FALSE, # remove polygon borders
              #fillColor = ~pal_fun(gridcode), # set fill color with function from above and value
              fillOpacity = 0.2, smoothFactor = 0.5,
  ) %>%
  addPolygons(data = gl_hs_21_rp, fillColor = "red",
              stroke = FALSE, # remove polygon borders
              #fillColor = ~pal_fun(gridcode), # set fill color with function from above and value
              fillOpacity = 0.2, smoothFactor = 0.5,
  ) %>%
# Works until this point but doesn't add tiles

  addTiles(group = "OSM") %>%
  addProviderTiles(providers$CartoDB.Positron)
  
  # Need to put multiple shapefiles with leaflet and have the tiles in the background
