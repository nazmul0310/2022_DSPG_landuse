library(ggplot2)
library(viridis)
library(readxl)
library(plotly)
library(tidyverse)
library(dplyr)
options(scipen = 999)   

croplayer1 <- read.csv("C:/Users/Christopher Vest/Documents/Land_use_project/2022_DSPG_Landuse/Review/Christopher/Croplayer/ag_analysis.csv")

p_21 <- croplayer1 %>% 
  filter(County == "Powhatan", Year==2021) %>%
  ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() + 
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type") %>% 
plot_ly(p_21, tooltip = c("text")) %>%
  layout(annotations = list(x = 1, y = -0.1, text = "Source: National Agricultural Statistics Service", showarrow = F, xref='paper', yref='paper', 
      xanchor='right', yanchor='auto'))
p_21 + theme(axis.text.y = element_text(hjust = 0))

g_21 <- croplayer1 %>% 
  filter(County == "Goochland", Year==2021) %>%
ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() +  
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") +     
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type") %>% 
plot_ly(g_21, tooltip = c("text")) %>%
  layout(annotations = list(x = 1, y = -0.1, text = "Source: National Agricultural Statistics Service", showarrow = F, xref='paper', yref='paper', 
      xanchor='right', yanchor='auto'))
g_21 + theme(axis.text.y = element_text(hjust = 0))

p_12 <- croplayer1 %>% 
  filter(County == "Powhatan", Year== 2012) %>%
  ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() + 
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type") %>% 
plot_ly(p_12, tooltip = c("text")) %>%
  layout(annotations = list(x = 1, y = -0.1, text = "Source: NASS", showarrow = F, xref='paper', yref='paper', 
      xanchor='right', yanchor='auto'))
p_12 + theme(axis.text.y = element_text(hjust = 0))

g_12 <- croplayer1 %>% 
  filter(County == "Goochland", Year== 2012) %>%
  ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() + 
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type") %>% 
plot_ly(g_12, tooltip = c("text")) %>%
  layout(annotations = list(x = 1, y = -0.1, text = "Source: NASS", showarrow = F, xref='paper', yref='paper', 
      xanchor='right', yanchor='auto'))
g_12 + theme(axis.text.y = element_text(hjust = 0))
