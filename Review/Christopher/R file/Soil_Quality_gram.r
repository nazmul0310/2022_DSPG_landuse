library(ggplot2)
library(viridis)
library(readxl)
library(plotly)
options(scipen = 999)

soil_quality <- read.csv("C:/Users/Christopher Vest/Documents/Land_use_project/2022_DSPG_landuse/ShinyApp/data/Soil_Quality_Analysis.csv")

g <- ggplot(soil_quality, aes(x = `G_Value`, y = `G_Area_acre`, fill = `G_Area_acre`)) +
  geom_bar(stat = "identity", aes(text = paste0(`G_Value`, "\n", "Total Acres: ", round(`G_Area_acre`, 0))))+
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev) +
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Soil Quality Classification", y = "Acreage", x = "Soil Quality Classification")
ggplotly(g, tooltip = "text")

p <- ggplot(soil_quality, aes(x = `P_Value`, y = `P_Area_acre`, fill = `P_Area_acre`)) +
  geom_bar(stat = "identity", aes(text = paste0(`P_Value`, "\n", "Total Acres: ", round(`P_Area_acre`, 0))))+
  geom_errorbarh(aes(xmax=as.numeric(`P_Value`)+0.45,xmin=as.numeric(`P_Value`)-0.45,height=0),position=position_dodge(width=0.9)) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev) +
  scale_fill_viridis() +
  labs( title = "Total Acreage by Soil Quality Classification", y = "Acreage", x = "Soil Quality Classification")
ggplotly(p, tooltip = "text")
 