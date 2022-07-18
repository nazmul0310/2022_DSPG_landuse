#
# This is a Shiny web application. You can run the application by clicking on
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# LAND USE PROJECT APP


library(shiny)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(shinyjs)
library(ggplot2)
library(plotly)
library(rsconnect)
library(rgdal)
library(plyr)
library(tigris)
library(dplyr)
library(leaflet)
library(tidycensus)
library(tidyverse)
library(stringr)
library(viridis)
library(readxl)
library(RColorBrewer)
library(sf) #for importing shp file

options(scipen=999)
options(shiny.maxRequestSize = 100*1024^2)

# data --------------------------------------------------------------------------------------------------------------------
popdist<-read.csv("data/popdist.csv", header = TRUE) #for Shiny ap
industry <- read.csv("data/industry.csv", header=TRUE) #for Shiny app
inc <- read.csv("data/inc.csv", header=TRUE) 
educ_earn <- read.csv("data/educ_earn.csv", header=TRUE) 

# Sociodemographic

age.func <- function(inputYear, inputCounty) {
  
  age <- popdist %>% # code for Shiny app
    filter(county == inputCounty, year==inputYear) %>%
    ggplot(aes(x=agecat , y=value, fill=agecat))+
    geom_bar(stat="identity") + 
    coord_flip() + 
    scale_fill_viridis(discrete=TRUE) + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    labs(title="Age Distribution of Population", y= "Percent", x= "Age Group", caption="Source: ACS5 2016-2020") +
    ylim(0,35)
  age
}

ind.func <- function(inputYear, inputCounty) {
  
  ind <- industry %>% 
    filter(county == inputCounty, year==inputYear) %>%
    ggplot(aes(x = reorder(name, desc(name)), y = value, fill = value)) + 
    geom_bar(stat = "identity") + theme(legend.position = "none") +
    coord_flip() + scale_fill_viridis()  + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    labs(title="Employment By Industry", y = "Percent", x = "Industry", caption="Source: ACS5 2016-2020") +
    ylim(0,25)
  ind
}

inc.func <- function(inputYear, inputCounty) {
  
  inc <- inc %>% 
    filter(county == inputCounty, year==inputYear) %>%
    mutate(inccat = fct_relevel(inccat, "<35K", "35K - 50K", "50K - 75K","75K-100K", ">100K")) %>%
    ggplot(aes(x = inccat, y = estimate, fill = inccat))+ 
    geom_bar(stat = "identity") + 
    theme(legend.position = "none") + 
    scale_fill_viridis(discrete=TRUE) + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    labs(title = "Income Distribution", y = "Percent", x = "Income", caption="Source: ACS5 2016-2020") +
    coord_flip() +
    ylim(0,50)
  inc
}

edu.func <- function(inputYear, inputCounty) {
  
  edu <- educ_earn %>% 
    filter(county == inputCounty, year==inputYear) %>%
    ggplot(aes(x = name, y = values)) + 
    geom_bar(stat = "identity", mapping=(aes(fill = name))) + 
    theme(legend.position = "none") + scale_fill_viridis(discrete=TRUE) +
    labs(title = "Median Earnings By Educational Attainment (Age > 25 years)", x = "Highest Education", y = "Median Earnings", caption = "Source: ACS5 2016-2020") + 
    geom_text(aes(label = values), vjust = -0.25) +
    scale_x_discrete(labels = c("Below\nhighschool", "Highschool\ngraduate", "Some college/\nAssociates'", "Bachelor's", "Graduate")) + 
    theme_light() + 
    theme(legend.position="none") + 
    theme(axis.text.y = element_text(hjust=0)) +
    ylim(0, 200000)
  edu
}



# Land use

#Goochland Land Use 

gooch_boundary<- st_read("data/cnty_bndry/Goochland_Boundary.shp")
gooch_boundary <- st_transform(gooch_boundary, "+proj=longlat +datum=WGS84")

m <- leaflet()%>%
  addTiles() %>%
  setView(lng=-77.949, lat=37.742, zoom=10.48) %>% 
  addPolygons(data=gooch_boundary,
              fillColor = "transparent") 

croplayer1 <- read.csv("data/ag_analysis.csv")

gcrop21 <- croplayer1 %>% 
  filter(County == "Goochland", Year==2021) %>%
  ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", hoverinfo = "text", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() +  
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") +     
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type") 
gcrop21 <-ggplotly(gcrop21, tooltip = c("text")) 

gcrop12 <- croplayer1 %>% 
  filter(County == "Goochland", Year== 2012) %>%
  ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", hoverinfo = "text", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() + 
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type")
gcrop12 <-ggplotly(gcrop12, tooltip = c("text"))

pcrop21 <- croplayer1 %>% 
  filter(County == "Powhatan", Year==2021) %>%
  ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", hoverinfo = "text", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() + 
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type")
pcrop21 <-ggplotly(pcrop21, tooltip = c("text"))

pcrop12 <- croplayer1 %>% 
  filter(County == "Powhatan", Year== 2012) %>%
  ggplot(aes(x = reorder(`Combined`, `Area.Acre`), y = `Area.Acre`, fill = `Area.Acre`)) + 
  geom_bar(stat = "identity", hoverinfo = "text", aes(text = paste0(`Combined`, "\n", "Total Acres: ", round(`Area.Acre`, 0)))) + 
  coord_flip() + 
  theme_light() +
  theme(axis.text.y = element_text(hjust=0)) +
  theme(legend.position = "none") + 
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Land type", x = "Acreage", y = "Land type")
pcrop12 <- ggplotly(pcrop12, tooltip = c("text"))


soil_quality <- read.csv("data/Soil_Quality_Analysis.csv")

gsoil <- ggplot(soil_quality, aes(x = `G_Value`, y = `G_Area_acre`, fill = `G_Area_acre`)) +
  geom_bar(stat = "identity",hoverinfo = "text", aes(text = paste0(`G_Value`, "\n", "Total Acres: ", round(`G_Area_acre`, 0))))+
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev) +
  scale_fill_viridis() + 
  labs( title = "Total Acreage by Soil Quality Classification", y = "Acreage", x = "Soil Quality Classification")
gsoil <-ggplotly(gsoil, tooltip = "text")

psoil <- ggplot(soil_quality, aes(x = `P_Value`, y = `P_Area_acre`, fill = `P_Area_acre`)) +
  geom_bar(stat = "identity",hoverinfo ="text", aes(text = paste0(`P_Value`, "\n", "Total Acres: ", round(`P_Area_acre`, 0))))+
  geom_errorbarh(aes(xmax=as.numeric(`P_Value`)+0.45,xmin=as.numeric(`P_Value`)-0.45,height=0),position=position_dodge(width=0.9)) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(limits = rev) +
  scale_fill_viridis() +
  labs( title = "Total Acreage by Soil Quality Classification", y = "Acreage", x = "Soil Quality Classification") 
psoil <-ggplotly(psoil, tooltip = "text")


# Powhatan Land Use



harbour<- leaflet() %>% 
  addTiles() %>% 
  setView(lng=-77.949, lat=37.742, zoom=9)

GoochlandAllParcel <- read_sf("../ShinyApp/data/luParcelData/GoochAll.shp")
goochBoundary <- read_sf("../ShinyApp/data/luParcelData/Goochland_Boundary.shp")


g.luPlotFunction <- function(year.g) {
  
  Gooch <- GoochlandAllParcel %>% filter(year == year.g)
  
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
  colors[8] <- "#ffffff" # the color is similar to 
  legendpalette <- colorFactor(palette = colors,levels=LUC_values)
  
  MyMap <- leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data=goochBoundary,
                fillColor = "transparent") %>%
    addPolygons(data = Gooch %>% filter(LUC_FIN == "Single Family Residential Urban"), 
                fillColor = colors[1], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Single Family Urban") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Single Family Residential Suburban"), 
                fillColor = colors[2], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Single Family Suburban") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Multi-Family Residential"), 
                fillColor = colors[3], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Multi-Family Residential") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Commerical / Industrial") ,
                fillColor = colors[4], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Commercial & Industrial") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Agricultural / Undeveloped (20-99 Acres)"),
                fillColor = colors[5], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Agriculture/Undeveloped (20-99 Acres)") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Agricultural / Undeveloped (100+ Acres)") ,
                fillColor = colors[6], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Agriculture/Undeveloped (100+ Acres)") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Other"),
                fillColor = colors[7], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Other") %>%
    addPolygons(data=Gooch %>% filter(LUC_FIN == "Undefined") ,
                fillColor = colors[8], smoothFactor = 0.1, fillOpacity=1, stroke = FALSE,
                group = "Unknown") %>%
    addLayersControl(
      overlayGroups = c("Single Family Urban", "Single Family Suburban", "Multi-Family Residential", "Commercial & Industrial", "Agriculture/Undeveloped (20-99 Acres)", "Agriculture/Undeveloped (100+ Acres)", "Other", "Unknown"),
      position = "bottomleft",
      options = layersControlOptions(collapsed = FALSE)) %>% 
    addLegend("bottomright", pal = legendpalette, values = LUC_values,
              title = "Land Use Type",
              labFormat = labelFormat(),
              opacity = 1,
              data=Gooch) #need to change for show the correct label
}

harbour<- leaflet() %>% 
  addTiles() %>% 
  setView(lng=-77.949, lat=37.742, zoom=9)



# Land Parcellation Imports

# gooch
gooch_parcellation <- st_read("data/parcellationData/Gooch_Parcellation_LT.shp") %>%
  st_transform(crs = st_crs("EPSG:4326"))
gooch_parcellation$year <- substr(gooch_parcellation$UNIQID_1, 1, 4)
gooch_bndry <- st_read("data/cnty_bndry/Goochland_Boundary.shp" )%>%
  st_transform(crs = st_crs("EPSG:4326"))

# pow
pow_parcellation <- read_sf("data/parcellationData/Powhatan_Parcellation_LT.shp") %>%
  st_transform(crs = st_crs("EPSG:4326"))
pow_parcellation$year <- substr(pow_parcellation$UNIQ_ID_12, 1, 4)
pow_bndry <- st_read("data/cnty_bndry/Powhatan_Boundary.shp") %>%
  st_transform(crs = st_crs("EPSG:4326"))






# ui --------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(title = "DSPG 2022",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
                 ## Tab Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   h1(strong("Land Use in Rural Counties on the Urban Fringe: the case of Goochland and Powhatan Counties​"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   align = "justify",
                                   column(4,
                                          h2(strong("Project Background")),
                                          p(strong("The setting:"), "Powhatan and Goochland County are two counties on the urban fringe outside of Richmond, Virginia. Both counties are known for their 
                                            rich agricultural histories. Communities on the urban fringe are located between both rural and urban areas. Although Powhatan County has been growing and devolving faster than the average rate, they like Goochland County would like to 
                                            keep their agricultural roots.  "),
                                          p(),
                                          p(strong("The problem:"), "Powhatan and Goochland are both rural counties that are located close to Richmond. Being that close to a big city like 
                                            Richmond has its advantages, but also its drawbacks. One of the biggest drawbacks is land conversion. Land conversion is when land shifts its use from one 
                                            use to another. In the case of Powhatan and Goochland, it means land is changing from agricultural land to residential. This really could hurt the economies 
                                            of the mostly agricultural counties. Both counties have enacted policies to help combat land conversion. Powhatan with its land tax deferral and Goochland with 
                                            its eighty-five- fifteen comprehensive plan. Both counties keep administrative data, but do not have the resources or the knowledge to analyze the data. This 
                                            is a key step in understanding the factors that cause land conversation or the probability that a parcel of land will convert. "),
                                          p(),
                                          p(strong("The project:"), "This Virginia Tech", a(href = "https://aaec.vt.edu/index.html", "Department of Argicultural and Applied Economics", target = "_blank"),
                                            "Data Science for Public Good (DSPG) project uses data science to analyze land conversion in Goochland and Powhatan counties in order to provide stakeholders with 
                                            a better understanding of how land is being distributed, specifically in regards to agriculture.")
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p("Our research team worked with Powhatan County and Goochland County to help find a way to minimize land conversion. Both counties want to stay mostly agricultural even going as far to introduce 
                                            policies including Powhatan’s land use tax deferral program and Goochland’s eighty-five- fifteen comprehensive plan. Our research team studied background information and past reports to get an 
                                            understanding and feel for the project and the data we would be analyzing. The team also meet with their stakeholders on a regular basic to make sure the project was finished on schedule and 
                                            that it pleased the stakeholders.  In addition to the meetings with the stakeholders, there was also contact by email to answer any questions pertaining to the project. "),
                                          p(),
                                          p("We collected all of our data from the ACS and county level administrative data to create our graphs, maps, and tables. These visualizations allowed us to analyze and present our findings timely and accurately. We:"),
                                          tags$li("Provided census tract and county-level maps of Goochland and Powhatan County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                          tags$li("Used crop layer data to create a map of", strong("all crops grown (and acreage)"), "in the counties. "),
                                          tags$li("Mapped locations of", strong("land parcels"), "at census block group level to highlight the surface water sources and the potential contaminations sources in the county.  "),
                                          tags$li("Mapped traffic data to show ", strong("commute times"), "to Richmond, Virginia from both counties."),
                                          tags$li("Created a", strong("Multinomial Logistic regression model"), "to show probability of land conversion. "), 
                                          p(),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively."),
                                   ),
                                   column(4,
                                          h2(strong("Dashboard Aims")),
                                          p("Our dashboard is aimed at:"),
                                          p(strong("Powhatan and Goochland county’s government."), "This dashboard will show them the probability of a land parcel converting and the factors 
                                            that make them convert. These factors will help make more policies to combat land conversion. "),
                                          p(strong("Researchers working on land use conversion."), "Land conversion is a problem all over the country not just Powhatan and Goochland counties. 
                                            Those could use our dashboard as an idea on how to show their findings and what data to use to calculate the probability of land use conversion.")
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2022')))
                          ) 
                 ),
                 
                 ## Tab sociodemographics --------------------------------------------
                 # need a different name
                 navbarMenu("Sociodemographics" , 
                            tabPanel("Goochland", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Goochland"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("County Background")),
                                                     p("Goochland County is located in the Piedmont of the Commonwealth of Virginia. It covers 281.42 square miles. This makes Goochland the 71st biggest
                                                       county in Virginia. The county is known for its fertile land and mineral deposits. The James River flows through the center of the county which
                                                       supplied water to farmlands and to mills. Coal was mined in the east and gold in the west. Today, agriculture is still 
                                                       important to the county economy. Goochland has updated its voting districts in 2022 to better represent the population of all 5 districts. 
                                                       Goochland Country also has a vast summer program with plenty of activates. The activities are located all over the county at different facilities 
                                                       including the skate park, gymnasium, the baseball fields, weight room, trails, and many more [1][2]. "),
                                                     
                                                     h4(strong("Summary Statistics")),
                                                     p("Goochland County’s population is 23,472, which is split between 49.8% male (11,698), and 50.2% female (11,774) [3]. 23,524 identify as one 
                                                       race, where 19,302 are white, 3,267 are African American, 75 are American Indian and Alaska Native, 494 Asian, 3 are Native Hawaiian and Other Pacific Islander, 
                                                       and 383 are some other race [4]." ),
                                                     p("57.9% of the population within Goochland County is employed. The unemployment rate is 3.7% [5]."),
                                                     p("There are 11,001 civilian employed citizens with 418 employed under agriculture, forestry, fishing and hunting, and mining [6]."),
                                                     p("There are a total of 8,711 households in Goochland County. The median income is $97,146 with a margin of error of around 8,582. Approximately 24.1% of the 6,600 
                                                       households have one earner, while 46.1% have two earners [7]. The greatest proportion (20.5%) of earners in Goochland make between $100,000 to $149,999. 18.4% 
                                                       of earners in Goochland earn over $200,000 [8]."),
                                                     p("Nearly 93.1% of the population 25 and over have graduated high school and gone to further their academic career. The highest level of education, a graduate or 
                                                       professional degree, has been attained by around 3,531 people, or 20.1% of the population over 25 years old [9]."),
                                                     p("According to the 2017 agricultural census, there were approximately 355 farms with an average farm size of 160 acres. This makes the total land coverage of farms to be 56,739 acres. 
                                                     $11,740,000 was generated from agricultural products sold to market. 46% of farms sold less than $2,500, and 3% of farms sold over $100,000. Grains, oilseeds, dry beans, and dry peas were the main crops that 
                                                       were sold ($2,846,000) and milk from cows were the main livestock and poultry products sold ($4,936,000) [10]."),
                                                     p("1.0% of Goochland’s population moved within the county, 8.4% moved into the county from a different county in VA, .7% moved from a completely different state, and .3% 
                                                       moved from abroad [11]."),
                                              ) ,
                                              column(8, 
                                                     h4(strong("Sociodemographics")),
                                                     selectInput("goochland_soc", "Select Variable:", width = "100%", choices = c(
                                                       "Age Distribution of Population" = "gage",
                                                       "Employment by Industry" = "gind",
                                                       "Income Distribution" = "ginc",
                                                       "Median Earnings By Educational Attainment (Age > 25 years)" = "gedu")
                                                     ),
                                                     radioButtons(inputId = "yearSelect_gsoc", label = "Select Year: ", 
                                                                  choices = c("2017", "2018", "2019", "2020"), 
                                                                  selected = "2020"),
                                                     plotOutput("gsoc", height = "500px"),
                                                     h4(strong("Visualization Summaries")),
                                                     p("The", strong("age distribution"), "graphs shows that the 45-64 age group has consistently been the largest in the county, making up more than 30% of the population since 2017. 
                                                       The 25-44 age group has been the second largest, but has faced more inconsistency and has seen a decrease since 2019."),
                                                     p("The", strong("employment"), "graphs indicates that the education, health, and social services industry group has been the largest by a wide margin, and specifically saw a large 
                                                       increase between 2018 and 2019. The agricultural, forestal, fishing, hunting, and mining industry group has consistently been the smallest, employing less than 5% of 
                                                       the population every year."),
                                                     p("The" ,strong("income distribution"), "graph illustrates the consistent growth in individuals and households earning at least $100,000 each year. This growth has been accompanied 
                                                       by a decrease in earnings below $75,000. It is also notable that earnings above $100,000 and below $35,000 are the largest categories throughout all years."),
                                                     p("The" ,strong("median earnings"), "graphs highlight the fact that those with a highest educational attainment of Some college/Associates earn the most. The median earnings for this 
                                                       group were significantly higher than others in 2017 and 2018, but saw a significant decrease to $65,890 in 2019. This number goes back up to $75,313 in 2020; still much lower than the first two years."),
                                                     
                                              ),
                                     ),
                                     column(12, 
                                            h4("References: "), 
                                            p(tags$small("[1] United States Department of Agriculture. Goochland County Virginia - National Agricultural Statistics Service. National Agricultural Statistics Survey. Retrieved July 6, 2022, from https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/Virginia/cp51075.pdf")), 
                                            p(tags$small("[2] United States Department of Agriculture. Goochland County Virginia - National Agricultural Statistics Service. National Agricultural Statistics Survey. Retrieved July 6, 2022, from https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/Virginia/cp51075.pdf")), 
                                            p(tags$small("[3] U.S. Census Bureau (2022). Age and Sex, 2020: ACS 5-Year Estimates Subject Tables. Retrieved from https://data.census.gov/cedsci/table?t=Populations%20and%20People&g=0500000US51075&tid=ACSST5Y2020.S0101.")), 
                                            p(tags$small("[4] U.S. Census Bureau (2022). Race, 2020: DEC Redistricting Data (PL 94-171). Retrieved from https://data.census.gov/cedsci/table?t=Populations%20and%20People&g=0500000US51075.")) ,
                                            p(tags$small("[5] U.S. Census Bureau (2022). Employment Status, 2020: ACS 5-Year Estimates Subject Tables. Retrieved from https://data.census.gov/cedsci/table?t=Employment%3AEmployment%20and%20Labor%20Force%20Status&g=0500000US51075&y=2020&tid=ACSST5Y2020.S2301&moe=false.")) ,
                                            p(tags$small("[6] ")),
                                            p(tags$small("[7]")),
                                            p("", style = "padding-top:10px;")) 
                            ), 
                            tabPanel("Powhatan", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Powhatan"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("County Background")),
                                                     p("Powhatan County, located in the Virginia’s Central Piedmont, was founded in 1777 by the Virginia General Assembly. It is 272 square miles and is home to a population 
                                                       of 29,253. The county is approximately 20 miles from Richmond and 2 hours from Washington, D.C. The James River borders the north end of the county, contributing to the formation of many creeks stretching southward. 
                                                       There are five districts within the county and 12 polling places interspersed through them. Powhatan is rich in history and offers an abundance of tourist 
                                                       attractions contributing to its thriving economy. There are three parks/wildlife management areas within the county: Powhatan Wildlife Management Area, Fighting Creek Park, 
                                                       and Powhatan State Park. Where once were several farms, the Powhatan Wildlife Management Area is 4,462 acres that provide many experiences such as hunting, fishing and other 
                                                       forested activities that aim to maintain the diversity of its natural wildlife species. [1][2]"),
                                                     
                                                     h4(strong("Summary Statistics")),
                                                     p("The total population  of Powhatan County is 29,253, split between 51% male (15,188), and 49% female (14,065) [3]. 28,762 identify as one race, where 25,732 are white, 2,505 are African 
                                                       American, 64 are American Indian and Alaska Native, 169 are Asian, 24 are Native Hawaiian and Other Pacific Islander, and 268 are some other race [4]."),
                                                     p("24,715 or 57.3% of the population within Powhatan County is employed. The unemployment rate is 1.4%. [5]"),
                                                     p("Of the 13,938 civilian employed population, there are very few that are employed in agriculture, forestry, fishing, hunting, and mining. Around .94% of the civilian employed 
                                                       population fall under that category. Of that .94% of the workers, around half of them serve roles in management, business, science, and art occupations while 14.5% of that 
                                                       population work in natural resources, construction, and maintenance occupations [6]."),
                                                     p("Of the 10,392 households, the median income is $93,833 with a margin of error of around 5,342. Approximately 30.2% of the 8,220 family have one earner, while 44.8% have two earners [7]. 
                                                       The greatest proportion (23.4%) of earners in Powhatan make between $100,000 to $149,999 [8]."),
                                                     p("Nearly 89.6% of the population 25 and over have graduated high school and gone to further their academic career. The highest level of education, a graduate or professional degree, has 
                                                       been attained by around 2,032 people, or 9.3% of the population over 25 years old [9]."),
                                                     p("According to the 2017 agricultural census, there were approximately 263 farms with an average farm size of 132 acres in 2017. This makes the total land coverage of farms to be 34,585 acres. $11,249,000 was generated from agriultural products sold to market. 
                                                       54% of farms sold less than $2,500, and 13% of farms sold between $25,000 and $24,999. Grains, oilseeds, dry beans, and dry peas were the main crops that were sold ($2,542,000) 
                                                       and poultry and eggs were the main livestock and poultry products sold ($6,056,000) [10]."),
                                                     p("1.9% of Powhatan’s population moved within the county, 7.4% moved into the county from a different county in VA, .8% moved from a completely different state, and .1% moved from abroad [11]."),
                                                     
                                              ) ,
                                              column(8, 
                                                     h4(strong("Sociodemographics")),
                                                     selectInput("powhatan_soc", "Select Variable:", width = "100%", choices = c(
                                                       "Age Distribution of Population" = "page",
                                                       "Employment by Industry" = "pind",
                                                       "Income Distribution" = "pinc",
                                                       "Median Earnings By Educational Attainment (Age > 25 years)" = "pedu")
                                                     ),
                                                     radioButtons(inputId = "yearSelect_psoc", label = "Select Year: ", 
                                                                  choices = c("2017", "2018", "2019", "2020"), 
                                                                  selected = "2020"),
                                                     plotOutput("psoc", height = "500px"),
                                                     h4(strong("Visualization Summaries")),
                                                     p("The", strong("age distribution"), "graphs shows that the 45-64 age group has consistently been the largest in the county, making up more than 30% of the population since 2017. The 25-44 age group has been 
                                                       the second largest, but has faced more inconsistency and has seen a decrease since 2018."),
                                                     p("The", strong("employment"), "graphs indicates that the education, health, and social services industry group has been the largest by a wide margin, and specifically saw a large increase in 2019. The agricultural, forestal,
                                                       fishing, hunting, and mining industry group has consistently been the smallest with the exception of 2018 when the information industry was smaller."),
                                                     p("The", strong("income distribution"), "graph illustrates the consistent growth in individuals and households earning at least $100,000 each year. This growth has been accompanied by a consistent decrease in earnings below $75,000."),
                                                     p("The", strong("median earnings"), "graphs highlight the fact that those with a highest educational attainment of Some college/Associates earn the most. The median earnings for this group were significantly higher than others up until 2019, but saw 
                                                       a significant decrease to $66,915 in 2020. This number is nearly identical to the median earnings for those with less than a high school education at $66,716."),
                                              ),
                                              column(12, 
                                                     h4("References: "), 
                                                     p(tags$small("[1] About Powhatan. About Powhatan | Powhatan County, VA - Official Website. (n.d.). Retrieved July 15, 2022, from http://www.powhatanva.gov/317/About-Powhatan")),
                                                     p(tags$small("[2] Powhatan WMA. Virginia Department of Wildlife Resources. (n.d.). Retrieved July 15, 2022, from https://dwr.virginia.gov/wma/powhatan/")),
                                                     p(tags$small("[3] American Community Survey 5-Year Estimates 2016/2020")),
                                                     p(tags$small("United States Department of Agriculture. Powhatan County Virginia - National Agricultural Statistics Service. National Agricultural Statistics Survey. Retrieved July 6, 2022, from https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/County_Profiles/Virginia/cp51145.pdf")) ,
                                                     p("", style = "padding-top:10px;")) 
                                     ), 
                            ) 
                            
                            
                            
                 ),
                 
                 ## Tab Policy --------------------------------------------
                 tabPanel("Policy", value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Land Use & Environmental Policies"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   p("Policy plays a key role in land-use. At every level- federal, state, and local- officials develop land use plans, with a wide 
                                     variety of different objectives and long-term visions. These plans drive changes in land-use, and it is important to investigate 
                                     policies at every level to get a full picture of land-use conversion."),
                                   tabsetPanel(
                                     tabPanel("Federal",
                                              column(6, 
                                                     p("", style = "padding-top:10px;"),
                                                     p(strong("The Conservation Reserve Program (CRP):")), 
                                                     p("The CRP is a federal land conversion program administered by the Farm Service Agency (FSA). 
                                                       The goal of this program is to reduce cropland acreage- a land retirement program that pays farmers to retire some of their crop land. 
                                                       This program has been a major driver of land retirement since it was implemented in 1985. The program is motivated by environmental 
                                                       protection goals. To get approved for the land retirement program, your land must hit specific criteria based on targeted environmental 
                                                       factors. There is then a bidding process. To farmers, this is an incentive to retire land. Studies show that this policy has led to farmers 
                                                       retire their less productive land. In 2005, “CRP paid $1.7 billion to keep a land area almost the size of Iowa out of production” (source). 
                                                       This federal land conversion program incentivizes farmers to retire their land- and lower production. The goal is to protect the environment."),
                                                     br(),
                                                     p(strong("Federal Crop Insurance Program:")),
                                                     p("This program is a partnership between the federal government and insurers- connecting the public and private sectors. 
                                                       This program does the opposite of the CRP and raises incentives to grow crops. The goal of the Federal Crop Insurance Program is not directly to affect 
                                                       land-use, but it does influence conversion rates. In 1993, after some catastrophic flooding, congress passed the Federal Crop Insurance Reform Act. This 
                                                       act increased the premium subsidies for all crop insurance products- now the program includes a revenue insurance option and catastrophic coverage. About 
                                                       60% of cultivated cropland in the Unites States is covered by the Federal Crop Insurance Program. This program raises incentives to grow crops, and could 
                                                       influence farmers to cultivate riskier, less productive land (source)."),
                                                     br(),
                                                     p(strong("Emergency Relief Program (ERP):")), 
                                                     p("The purpose of this program is to help agriculture producers offset damage caused by natural disasters such as drought or 
                                                     flooding (source). Funds are distributed in two phases, to aid livestock producers impacted by natural disasters. The USDA announced in May of 2022 that 
                                                       “commodity and specialty crop producers impacted by natural disaster events in 2020 and 2021 will soon begin receiving emergency relief payments totaling 
                                                       approximately $6 billion through the Farm Service Agency’s (FSA) new Emergency Relief Program (ERP) to offset crop yield and value losses” (source)."),
                                                     p(),
                                                     p(),
                                                     h4("References:"),
                                                     p(tags$small("[1] How Can You Help Protect Source Water? (n.d.). Retrieved July 29, 2021, from https://www.epa.gov/sourcewaterprotection/how-can-you-help-protect-source-water")), 
                                                     p(tags$small("[2] Well maintenance. (2009, April 10). Retrieved July 29, 2021, from https://www.cdc.gov/healthywater/drinking/private/wells/maintenance.html#:~:text=Wells%20should%20be%20checked%20and,example%2C%20arsenic%20and%20radon).")) ,
                                                     p(tags$small("[3] A Guide to Private Wells (pp. 5-25, Publication). (1995). Blacksburg, VA: Virginia Water Resources Research Center.")) ,
                                                     p("", style = "padding-top:10px;"),
                                                     
                                                     
                                              ) , 
                                              column(6, 
                                                     p("", style = "padding-top:10px;"),
                                                     p(strong("Emergency Conservation Program (ECP):")), 
                                                     p("This program “provides funding and technical assistance for farmers and ranchers to restore farmland damaged by natural disasters and for emergency water 
                                                       conservation measures in severe droughts” (source). This program does so by giving landowners funding to install water conservation systems or to repair 
                                                       damaged farmland. This is another example of a conservation program that gives farmers insurance, which could incentive farmers to continue to cultivate their 
                                                       land- regardless of the potential risks associated with damage from storms and droughts. Farms are eligible for this assistance if the damage is affecting 
                                                       productivity, there is evidence that conditions will worsen without intervention, and the repairs will be too costly without federal assistance (source). 
                                                       Up to 75% of the costs can be provided. The FSA County Committee can “approve applications up to $125,000 while $125,001 to $250,000 requires state committee 
                                                       approval (source)."),
                                                     br(), 
                                                     p(strong("Source Water Protection Program (SWPP):")),
                                                     p("This program is a joint project with the U.S. Department of Agriculture (USDA) Farm Service Agency (FSA) and the National Rural Water Association (NRWA), 
                                                       a non-profit water and wastewater utility membership organization (source). It was designed with the goal of protecting surface and ground water that is 
                                                       used as drinking water by rural residents. The NRWA employs full-time rural source water technicians that work with state and county FSA staff to make decisions 
                                                       on where pollution prevention is needed. The SWPP works at the local level, to educate and encourage farmers to prevent source water prevention. With this program, 
                                                       it is the local community to create and invest in a water protection plan."),
                                                     br(),
                                                     p(strong("Agriculture Risk Coverage (ARC) and Price Loss Coverage (PLC):")),
                                                     p("ARC program is an “income support program that provides payments when actual crop revenue declines below a specified guaranteed level” (source). PLC program “provides 
                                                     income support payments when the effective price for a covered commodity falls below its effective reference price” (source). Both programs provide financial protection 
                                                       to farmers. They serve as a safety net from drops in crop revenues and prices."),
                                              )), 
                                     tabPanel("State",
                                              p(),
                                              p('State level officials work within the confines of both federal and local policy. They aim to simultaneously enhance federal policy, while enabling local officials to make comprehensive 
                                              land-use plans. The state of Virginia is under the Dillon Rule which states that local ordinances must be consistent with state law (source). Local officials are the ones approving parcel-specific 
                                              land use plans, but state and federal officials play a key role (source). “The state courts are the "referees" to determine if land use decisions violated some aspect of various state laws, or if 
                                                the land use rules violated the state constitution in some way (source).'),
                                              column(6, 
                                                     p("", style = "padding-top:10px;"),
                                                     p(strong("Conservation Reserve Enhancement program (CREP):")), 
                                                     p("This is a state sponsored enhancement to the federal CRP. It is a cost-share program where federal reimbursement are made through the FSA for up to 
                                                       “50% of a participant's eligible expenses for implementing best management practices (BMP)”. BMP examples include adding fencing, alternative watering 
                                                       systems, and restoring wetlands. Participation in this program is voluntary, and the contract period is around 10-15 years (source)."),
                                                     br(),
                                                     p(strong("Agriculture and Forestal Districts (AFD):")),
                                                     p("The AFD program in Virginia was designed to “preserve and protect open spaces, forested areas, and agricultural lands” (source). This program makes 
                                                       it so land taxes are based on use rather than taxing solely on the market value. Land used for growing crops, for example, is taxed differently than 
                                                       developed property. This state level policy encourages localities to be purposeful with their property taxes. The hope is that this policy will be used 
                                                       to conserve and protect agricultural and forest land. These lands can be valued as “natural and ecological resources which provide essential open spaces 
                                                       for clean air sheds, watershed protection, wildlife habitat, aesthetic quality and other environmental purposes” (source). This program was formed in 1977 
                                                       (source). The potential benefits are to lower property taxes, safeguard the rural character of the community, and offer protection from eminent domain (source)."),
                                                     br(),
                                                     p(strong("Nonpoint Source (NPS) Pollution Management Program:")), 
                                                     p("This is a diverse network of state and local government programs that . Collectively, these programs “help to prevent water quality degradation and to restore 
                                                       the health of lakes, rivers, streams and estuaries by promoting and funding state and local watershed planning efforts, stream and wetland restoration and protection, 
                                                       education and outreach, and other measures to reduce and prevent NPS pollution from affecting the Commonwealth’s waters” (source)."),
                                                     p(),
                                                     p(),
                                                     h4("References:"),
                                                     p(tags$small("[1] How Can You Help Protect Source Water? (n.d.). Retrieved July 29, 2021, from https://www.epa.gov/sourcewaterprotection/how-can-you-help-protect-source-water")), 
                                                     p(tags$small("[2] Well maintenance. (2009, April 10). Retrieved July 29, 2021, from https://www.cdc.gov/healthywater/drinking/private/wells/maintenance.html#:~:text=Wells%20should%20be%20checked%20and,example%2C%20arsenic%20and%20radon).")) ,
                                                     p(tags$small("[3] A Guide to Private Wells (pp. 5-25, Publication). (1995). Blacksburg, VA: Virginia Water Resources Research Center.")) ,
                                                     p("", style = "padding-top:10px;"),
                                                     
                                                     
                                              ) , 
                                              column(6, 
                                                     p("", style = "padding-top:10px;"),
                                                     p(strong("Chesapeake Bay Preservation Act:")),
                                                     p("This program was developed in 1988 as an element of Virginia's NPS management program. The goal is to protect and improve water quality in the Chesapeake 
                                                     Bay by requiring effective land use management practices (source)."), 
                                                     p('"The Bay Act program is the only program administered by the Commonwealth of Virginia that comprehensively addresses the effects of land use planning and 
                                                     development on water quality. The Bay Act recognizes that local governments have the primary responsibility for land use decisions and expands their authority 
                                                     to manage water quality, and establish a direct relationship between water quality protection and local land use decision-making" (source).'),
                                                     br(),
                                                     p(strong("Total Maximum Daily Load (TMDL):")),
                                                     p("Significant portions of the Chesapeake Bay have been identified as not meeting water quality standards. Despite the Chesapeake Bay program, water quality goals 
                                                     have not been met. In December of 2010, the EPA issued a TMDL, a “pollution diet” to protect the Bay (source). This TMDL is divided among all the Bay states. However,
                                                       “regional or statewide consistency is rare in Virginia's land use planning process - even statewide requirements such as the Chesapeake Bay Regulations are interpreted 
                                                       differently by different jurisdictions” (source)."),
                                              )) ,
                                     tabPanel("County",
                                              p(),
                                              p('"In urbanizing areas such as the suburbs near Richmond, Hampton Roads, and Northern Virginia, control over how private property 
                                                is developed may be a contentious process involving landowners and their lawyers, neighbors or local residents upset over additional 
                                                development, and local officials. In Fairfax, Loudoun, and Prince William counties over the last 30 years, the Board of County Supervisor 
                                                election campaigns have been based on growth management issues. Local officials have reacted to citizen complaints, and incumbents have 
                                                been voted out of office because they were either too supportive of growth or too restrictive.” (source)."'),
                                              column(6,
                                                     h1(strong("Goochland"), align = "center"),
                                                     p("", style = "padding-top:10px;"),
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                              p("Goochland County runs a land use program which assesses land based on use value as opposed to market value. The program was adopted by the county in 1978. There are multiple requirements for land to be eligible for the program as established by the State Land Evaluation Advisory Council:"),
                                                              tags$ul(
                                                                
                                                                tags$li("Land must be in production for sale 5 years prior to entering the program as agriculture or horticulture"),
                                                                
                                                                tags$li("Land must be zoned as agricultural"),
                                                                
                                                                tags$li("Land must meet minimum acreages for each land use category "),
                                                                
                                                                tags$li("All real estate taxes have been paid on parcel "),
                                                                
                                                              ),
                                                              p("There are also multiple land use categories including agriculture, horticulture, and forest use (source)."),
                                                              p("The main agricultural districts in the county include A1 (General), A2 (Limited), and A3 (Intensive) (source). These districts promote the protection
                                                     of agricultural land and encourage agribusiness. The Goochland County 2035 Comprehensive Plan includes an agricultural commitment to maintaining approximately
                                                     85% of the county in the Rural Enhancement Land Use Designation through 2035 (source). The county also supports economic development and tourism through the
                                                     ACRES initiative which “[Supports] Goochland’s Agricultural Community through Accessibility, Connectivity, Readiness, Education, and Sustainability” (source).
                                                     The initiative encourages the recognition of Goochland County’s agricultural history and identity and promotes rural economic development/tourism."))),
                                              column(6,
                                                     h1(strong("Powhatan"), align = "center"),
                                                     p("", style = "padding-top:10px;"),
                                                     fluidRow(style = "margin: 6px;", align = "justify",
                                                              p('Powhatan County land use policy includes a land use deferral program, Powhatan County code Section 70-76, which states that the purpose of land use is
                                                     to “preserve real estate devoted to agricultural, horticultural, forest and open space uses within its boundaries in the public interest....". 
                                                     The land use deferral program “offers a deferral of a portion of the real estate taxes for qualifying properties”. This ordinance was adopted by the
                                                     county in 1976 and approximately 40% of the county is in land use today (source). Powhatan County also has an Agricultural and Forestal District (AFD)
                                                     Program which allows the county, with the landowner’s consent, to take land out of development in exchange for a land use tax rate as opposed to market
                                                     value tax rate. As of September/October 2020, there are approximately 5640 acres of AFD land. This program serves to protect natural lands as well as prevent
                                                     landowners from having to sell their land as market values and tax rates continue to increase. One benefit that the AFD program has over the land use deferral
                                                     program is that it is officially included in the County’s Comprehensive Plan (source). '),
                                                              p('The county’s zoning ordinance categorizes rural districts into 6 groups. The main agricultural districts are A-20 (min 20 ac), A-10 (min 10 ac), and A-C.
                                                     The 3 other rural districts are largely dedicated to residential zoning. The 2010 long range comprehensive plan also includes sections on natural conservation
                                                     and rural preservation which outline land use policies to be “used when addressing development and land use issues” (source). These policies promote the
                                                     conservation of open land and farmland and recognize agriculture as an economic driver of the community.'))),
                                              column(12, 
                                                     h4("References:"),
                                                     p(tags$small("[1] How Can You Help Protect Source Water? (n.d.). Retrieved July 29, 2021, from https://www.epa.gov/sourcewaterprotection/how-can-you-help-protect-source-water")), 
                                                     p(tags$small("[2] Well maintenance. (2009, April 10). Retrieved July 29, 2021, from https://www.cdc.gov/healthywater/drinking/private/wells/maintenance.html#:~:text=Wells%20should%20be%20checked%20and,example%2C%20arsenic%20and%20radon).")) ,
                                                     p(tags$small("[3] A Guide to Private Wells (pp. 5-25, Publication). (1995). Blacksburg, VA: Virginia Water Resources Research Center.")) ,
                                                     p("", style = "padding-top:10px;")) 
                                     )
                                   ) 
                          ) 
                          
                          
                 ),
                 ## Tab Land Use --------------------------------------------
                 
                 navbarMenu("Land Use" , 
                            tabPanel("Goochland", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Variables to Consider"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                tabPanel("Land Use",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Land Use in Goochland County")),
                                                                p("Insert text ")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Land Use Distribution and Change by Year")),
                                                                sliderInput(inputId = "luYear.g", label = "Year:", 
                                                                            min = 2018, max = 2021, value = 2018, 
                                                                            sep = ""),
                                                                
                                                                h4(strong("Land Uses Over the Years")),
                                                                
                                                                
                                                                leafletOutput(outputId = "luPlot.g"),
                                                                p(tags$small("Data Source: Goochland County Administrative Data")))  ,
                                                         column(12,
                                                                
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )
                                                         
                                                ), 
                                                tabPanel("Crop Layer",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Crops Grown in Goochland County")),
                                                                p("The map and histogram on the right show the crop layer data for Goochland County. Goochland County is heavily forested, 
                                                                with forested lands accounting for 63.94% of all land. This number is a decrease from the 69.63% in 2012. 
                                                                  Developed land in Goochland increased from 7.28% to 9.29% in 10 years. Most of the developed land is in the east side of 
                                                                  the county closer to Richmond, VA. Forages is the second biggest crop layer category with 14.99%. Forage is bulky food 
                                                                  such as grass or hay for horses and cattle. Croplands are spread out throughout the county and only make up 4.1% of 
                                                                  the land. From an agricultural perspective, the land is more often used for raising livestock instead of 
                                                                  growing crops. There is a heavy concentration of row crops on the south boundary of county. The James River also acts as a 
                                                                   boundary between Powhatan County and Goochland County.")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Crop Layer Map")),
                                                                
                                                                
                                                                leafletOutput("harbour"),
                                                                br(),
                                                                h4(strong("Crop Layer Graphs")),
                                                                selectInput("gcrop", "Select Variable:", width = "100%", choices = c(
                                                                  "Total Acreage by Land Type 2021" = "gcrop21",
                                                                  "Total Acreage by Land Type 2012" = "gcrop12")
                                                                ),
                                                                
                                                                plotlyOutput("gcrop_graph", height = "500px"),
                                                         ),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )) ,
                                                tabPanel("Soil Quality",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Soil Quality in Goochland County")),
                                                                p("Good quality soil is essential for crops to produce. Which makes soil quality a factor that could result in land conversion. 
                                                                  The National Cooperative Soil Survey is a survey done to classify soil into classes based on its usefulness. Those classes are: "),
                                                                tags$ul(
                                                                  
                                                                  tags$li(strong("Class 1"), "soils have few limitations that restrict their use."),
                                                                  
                                                                  tags$li(strong("Class 2"), "soils have moderate limitations that reduce the choice of plants or that require moderate conservation practices."),
                                                                  
                                                                  tags$li(strong("Class 3"), "soils have severe limitations that reduce the choice of plants, require special conservation practices, or both."),
                                                                  
                                                                  tags$li(strong("Class 4"), "soils have very severe limitations that reduce the choice of plants, require very careful management, or both."),
                                                                  
                                                                  tags$li(strong("Class 5"), "soils are subject to little or no erosion but have other limitations, impractical to remove, that restrict their use mainly to pasture, rangeland, forestland, or wildlife habitat."),
                                                                  
                                                                  tags$li(strong("Class 6"), "soils have severe limitations that make them generally suitable for cultivation and that restrict their use mainly to pasture, rangeland, forestland, or wildlife habitat."),
                                                                  
                                                                  tags$li(strong("Class 7"), "soils have very severe limitations that make them unsuitable for cultivation and that restrict their use mainly to grazing, forestland, or wildlife habitat."),
                                                                  
                                                                  tags$li(strong("Class 8"), "soils and miscellaneous areas have limitations that preclude commercial plant production and that restrict their use to recreational purposes, wildlife habitat, watershed, or esthetic purposes."),
                                                                  
                                                                ),
                                                                p("Most of Goochland County’s soil is in Class 2 or 3. This means that most of the land in Goochland is farmable, but it has limitations that reduce the choice of plants or that require very careful 
                                                                  management, or both.  On the other end of the spectrum, Goochland has zero acres of land in Class 8. Goochland also has a low number of acres with no data with 5,237 acres. Despite the limitations, 
                                                                  it still is possible to farm and for Goochland to be mostly agricultural."),
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Soil Quality Map")),
                                                                
                                                                sliderInput(inputId = "year", 
                                                                            label = "Choose the starting and ending years",
                                                                            min = 2012,
                                                                            max = 2021,
                                                                            step = 9,
                                                                            value = 2021,
                                                                            sep = "", ticks = FALSE),
                                                                leafletOutput("mymap",height = 500), 
                                                                h4(strong("Soil Quality Graph")),
                                                                plotlyOutput("gsoil", height = "500px"),
                                                                p(tags$small("Data Source: National Cooperative Soil Survey"))),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )) ,
                                                tabPanel("Traffic Data",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Traffic in Goochland County")),
                                                                p("Insert text")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Traffic Visualizations")),
                                                                selectInput("econ1", "Select Variable:", width = "100%", choices = c(
                                                                  "Traffic Volume" = "gvol",
                                                                  "Proximity to Richmond" = "grich")
                                                                ),
                                                                #                plotlyOutput("trend1", height = "600px")
                                                                
                                                         ),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) ) 
                                                )
                                              ) 
                                     )), 
                            tabPanel("Powhatan", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Variables to Consider"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                tabPanel("Land Use",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Land Use in Powhatan County")),
                                                                p("Insert Text")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Land Use Distribution and Change by Year")),
                                                                selectInput("econ1", "Select Variable:", width = "100%", choices = c(
                                                                  "2014" = "p2014",
                                                                  "2015" = "p2015",
                                                                  "2016" = "p2016", 
                                                                  "2017" = "p2017", 
                                                                  "2018" = "p2018", 
                                                                  "2019" = "p2019",
                                                                  "2020" = "p2020",
                                                                  "2021" = "p2021")
                                                                ),
                                                                #          plotlyOutput("trend1", height = "600px")
                                                                h4(strong("Land Use Transition Matrix")),
                                                                #withSpinner(leafletOutput("mines")),
                                                                p(tags$small("Data Source: Powhatan County Administrative Data")))  ,
                                                         
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )
                                                         
                                                ), 
                                                tabPanel("Crop Layer",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Crops Grown in Powhatan County")),
                                                                p("The map and histogram on the right show the crop layer data for Powhatan County. Powhatan County is heavily forested with forested lands account for 67.84% of all 
                                                                  land. This number is a decrease from the 75.82% in 2012. A big reason why that number is reduced is that Powhatan is rapidly developing. 
                                                                  Developed land in Powhatan increased from 3.46% to 6.88% in 10 years. Most of this developed land is in the east side of the county closer to Richmond, VA. Forages 
                                                                  is the second biggest crop layer category with 15.42%. Forage is bulky food such as grass or hay for horses and cattle. Croplands are spread out throughout the 
                                                                  county andmake up only use 4.1% of the land in the county. From an agricultural perspective, the land is most often used for raising livestock instead of growing crops. 
                                                                  There is a heavy concentration of row crops on the north boundary of Powhatan. The James River also acts as a boundary between Powhatan County and Goochland County.")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Crop Layer Map")),
                                                                
                                                                #                leafletOutput("trend1", height = "600px")
                                                                h4(strong("Crop Layer Graphs")),
                                                                selectInput("pcrop", "Select Variable:", width = "100%", choices = c(
                                                                  "Total Acreage by Land Type 2021" = "pcrop21",
                                                                  "Total Acreage by Land Type 2012" = "pcrop12")
                                                                ),
                                                                
                                                                plotlyOutput("pcrop_graph", height = "500px"),
                                                                p(tags$small("Data Source: ACS 2016-2020")),
                                                         ),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )) ,
                                                tabPanel("Soil Quality",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Soil Quality in Powhatan County")),
                                                                p("Good quality soil is essential for crops to produce. Which makes soil quality a factor that could result in land conversion. 
                                                                  The National Cooperative Soil Survey is a survey done to classify soil into classes based on its usefulness. Those classes are: "),
                                                                tags$ul(
                                                                  
                                                                  tags$li(strong("Class 1"), "soils have few limitations that restrict their use."),
                                                                  
                                                                  tags$li(strong("Class 2"), "soils have moderate limitations that reduce the choice of plants or that require moderate conservation practices."),
                                                                  
                                                                  tags$li(strong("Class 3"), "soils have severe limitations that reduce the choice of plants, require special conservation practices, or both."),
                                                                  
                                                                  tags$li(strong("Class 4"), "soils have very severe limitations that reduce the choice of plants, require very careful management, or both."),
                                                                  
                                                                  tags$li(strong("Class 5"), "soils are subject to little or no erosion but have other limitations, impractical to remove, that restrict their use mainly to pasture, rangeland, forestland, or wildlife habitat."),
                                                                  
                                                                  tags$li(strong("Class 6"), "soils have severe limitations that make them generally suitable for cultivation and that restrict their use mainly to pasture, rangeland, forestland, or wildlife habitat."),
                                                                  
                                                                  tags$li(strong("Class 7"), "soils have very severe limitations that make them unsuitable for cultivation and that restrict their use mainly to grazing, forestland, or wildlife habitat."),
                                                                  
                                                                  tags$li(strong("Class 8"), "soils and miscellaneous areas have limitations that preclude commercial plant production and that restrict their use to recreational purposes, wildlife habitat, watershed, or esthetic purposes."),
                                                                  
                                                                ),
                                                                p("Powhatan County soil is mostly in Class 2. As mentioned above, Class 2 has moderate limitations so crops can be grown here. Powhatan also has land that is in Class 1. This is the best land
                                                                  in the county, but it only makes up 1,686 acres. Class 4 soil is also prevalent in Powhatan. However, this soil class is unfavorable for farming as it has very severe limitations. The graph 
                                                                  on the right can be zoomed in on Class 8. This class is the least suitable soil class for any activity. Powhatan has 29 acres in the class. Overall, Powhatan has good farmland and can remain agricultural. "),
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Soil Quality Map")),
                                                                
                                                                #                plotlyOutput("trend1", height = "600px")
                                                                h4(strong("Soil Quality Graph")),
                                                                plotlyOutput("psoil", heigh = "500px"),
                                                                p(tags$small("Data Source: National Cooperative Soil Survey"))),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )) ,
                                                tabPanel("Traffic Data",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Traffic in Powhatan County")),
                                                                p("Insert text")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Traffic Visualizations")),
                                                                selectInput("econ1", "Select Variable:", width = "100%", choices = c(
                                                                  "Traffic Volume" = "pvol",
                                                                  "Proximity to Richmond" = "prich")
                                                                ),
                                                                #                plotlyOutput("trend1", height = "600px")
                                                                
                                                         ),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) ) 
                                                )
                                              ) 
                                     ), 
                            ) 
                            
                            
                            
                 ),
                 
                 ## Tab Parcellation --------------------------------------------
                 
                 navbarMenu("Parcellation" , 
                            tabPanel("Goochland", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Land Parcellation                 "), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                tabPanel("Parcels",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Land Parcels in Goochland County")),
                                                                p("Insert text")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Land Parcellation Map")),
                                                                sliderInput(inputId = "g.parcellationRange",
                                                                            label = "Years of Parcellation:",
                                                                            min = 2019,
                                                                            max = 2022,
                                                                            value = c(2019, 2022),
                                                                            sep = "", 
                                                                            width = "150%"),
                                                                leafletOutput("g.parcellationPlot")
                                                                
                                                         ),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )
                                                         
                                                ), 
                                                
                                                tabPanel("Hot Spots",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Parcellation Hot Spots in Goochland County")),
                                                                p("Insert text")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Parcellation Hot Spot Map")),
                                                                sliderInput(inputId = "g.hotspotInput", 
                                                                            label = "Choose the starting and ending years",
                                                                            min = 2019,
                                                                            max = 2022,
                                                                            step = 1,
                                                                            value = c(2019,2022),
                                                                            width = "150%",
                                                                            sep = ""),
                                                                leafletOutput("g.hotspotMap"),
                                                                p(tags$small("Data Source: Goochland County Administrative Data"))),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) ) 
                                                )
                                              ) 
                                     )), 
                            tabPanel("Powhatan", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Land Parcellation"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              tabsetPanel(
                                                tabPanel("Parcels",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Land Parcels in Powhatan County")),
                                                                p("Insert text")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Land Parcellation Map")),
                                                                sliderInput(inputId = "p.parcellationRange",
                                                                            label = "Years of Parcellation:",
                                                                            min = 2012,
                                                                            max = 2020,
                                                                            value = c(2012, 2020),
                                                                            sep = "", 
                                                                            width = "150%", ticks = FALSE),
                                                                leafletOutput("p.parcellationPlot")
                                                                
                                                         ),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) )
                                                         
                                                ), 
                                                
                                                tabPanel("Hot Spots",
                                                         p("", style = "padding-top:10px;"),
                                                         column(4, 
                                                                h4(strong("Parcellation Hot Spots in Powhatan County")),
                                                                p("Insert text")
                                                         ), 
                                                         column(8, 
                                                                h4(strong("Parcellation Hot Spot Map")),
                                                                
                                                                #                plotlyOutput("trend1", height = "600px")
                                                                
                                                         ),
                                                         column(12, 
                                                                
                                                                h4("References") , 
                                                                p(tags$small("[1] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[2] U.S. Census Bureau, Weldon Cooper Center for Public Service")), 
                                                                p(tags$small("[3] U.S Census Bureau")), 
                                                                p(tags$small("[4]  2010 Census")), 
                                                                p(tags$small("[5] U.S. Census Bureau, OnTheMap Application and LEHD Origin-Destination Employment Statistics, 2014")), 
                                                                p(tags$small("[6] Virginia Employment Commission, Economic Information & Analytics, Quarterly Census of Employment and Wages (QCEW), 4th Quarter (October, November, December) 2020.")), 
                                                                p(tags$small("[7] American Community Survey 5-year Estimates 2014/2019")), 
                                                                p(tags$small("[8]  Virginia Employment Commission")) ) 
                                                )
                                              ) 
                                     ), 
                            ) 
                            
                            
                            
                 ),
                 
                 ## Tab Findings --------------------------------------------
                 tabPanel("Findings & Predictions", value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Project Findings and Predictions"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   p(),
                                   h4(strong("Title")),
                                   fluidRow(style = "margin: 6px;", align = "justify",
                                            p("The CRP is a federal land conversion program administered by the Farm Service Agency (FSA). 
                                                       The goal of this program is to reduce cropland acreage- a land retirement program that pays farmers to retire some of their crop land. 
                                                       This program has been a major driver of land retirement since it was implemented in 1985. The program is motivated by environmental 
                                                       protection goals. To get approved for the land retirement program, your land must hit specific criteria based on targeted environmental 
                                                       factors. There is then a bidding process. To farmers, this is an incentive to retire land. Studies show that this policy has led to farmers 
                                                       retire their less productive land. In 2005, “CRP paid $1.7 billion to keep a land area almost the size of Iowa out of production” (source). 
                                                       This federal land conversion program incentivizes farmers to retire their land- and lower production. The goal is to protect the environment."),
                                            br(),
                                            h4(strong("Title 2")),
                                            p("The CRP is a federal land conversion program administered by the Farm Service Agency (FSA). 
                                                       The goal of this program is to reduce cropland acreage- a land retirement program that pays farmers to retire some of their crop land. 
                                                       This program has been a major driver of land retirement since it was implemented in 1985. The program is motivated by environmental 
                                                       protection goals. To get approved for the land retirement program, your land must hit specific criteria based on targeted environmental 
                                                       factors. There is then a bidding process. To farmers, this is an incentive to retire land. Studies show that this policy has led to farmers 
                                                       retire their less productive land. In 2005, “CRP paid $1.7 billion to keep a land area almost the size of Iowa out of production” (source). 
                                                       This federal land conversion program incentivizes farmers to retire their land- and lower production. The goal is to protect the environment."),
                                   ), 
                                   
                                   
                                   
                          )),
                 
                 ## Tab Data Sources --------------------------------------------
                 tabPanel("Data Sources", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data Sources"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                          p(strong("American Community Survey"), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census Bureau. ACS samples households to compile 1-year and 5-year datasets 
                                      providing information on population sociodemographic and ocioeconomic characteristics including employment, disability, and health insurance coverage. We used ACS 2014/18 5-year
                                      estimates to obtain census tract and census block group-level to explore Floyd County resident characteristics."),
                                          br(""),
                                          img(src = "usgs.jpg", style = "display: inline; float: left;", width = "150px"),
                                          p(strong("USGS National Land Cover Database"), "The USGS National Land Cover Database provided us with land cover data which allowed us to look into crop layer data for both counties. This was important for 
                                            us to gain a better understanding of how prevalent agriculture is in the counties and to visualize any changes in crops over the last several years.")
                                   ),
                                   column(4,
                                          img(src = "goochland.jpg", style = "display: inline; float: left;", width = "150px"),
                                          p(strong("Goochland County Administrative Data"), "Goochland County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                                            that has occured over a 5 year period (2018 - 2022). We used this data to create visualizations, specifically focusing on the distribution and change in land use in the county."),
                                          br(""),
                                          img(src = "vdot.png", style = "display: inline; float: left;", width = "200px"),
                                          p(strong("VDOT Traffic Data "), "The Virginia Department of Transportation (VDOT) is responsible for building, maintaining and operating the state's roads, bridges and tunnels. And, through the Commonwealth Transportation Board, it provides funding for airports, seaports, rail and public transportation. Virginia has the third-largest state-maintained highway system in the country, behind Texas and North Carolina."),
                                          br(""),
                                          
                                   ),
                                   column(4,
                                          img(src = "powhatan.jpg", style = "display: inline; float: left;", width = "150px"),
                                          p(strong("Powhatan County Administrative Data"), "Powhatan County provided us with parcel/property data which allowed us to gain a better understanding of the different land uses and parcellation
                                            that has occured over a 8 year period (2014 - 2021). We used this data to create visualizations, specifically focusing on the distribution and change in land use in the county.") 
                                          
                                          
                                   ),
                                   
                          )
                 ),
                 
                 
                 ## Tab Team --------------------------------------------
                 tabPanel("Meet the Team", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   align = "center",
                                   h1(strong("Meet the Team")),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good")),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program held at the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural'), "and", a(href = 'https://ext.vt.edu/','Applied Economics and the Virginia Cooperative Extension Service.'),
                                     "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical
                                social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how 
                                information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply,
                                and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Undergraduate Interns")),
                                          img(src = "john.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "chris.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          br(), 
                                          img(src = "rache.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/esha-dwibedi-83a63476/', 'Rachel Inman', target = '_blank'), "(Virginia Tech, Undergraduate in Smart and Sustainable Cities and Minoring in Landscape Architecture);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/julie-rebstock', 'John Malla', target = '_blank'), "(Virginia Tech, Undergraduate in Computational Modeling and Data Analytics);",
                                            br(), 
                                            a(href = 'www.linkedin.com/in/rachelinman21', 'Christopher Vest', target = '_blank'), "(Jacksonville State University, Undergraduate in Finance)."),
                                          p("", style = "padding-top:10px;") 
                                   ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Members")),
                                          img(src = "team-posadas.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sarah.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/briannaposadas/", 'Dr. Susan Chen', target = '_blank'), "(Associate Professor of Econometrics & Data Analytics);",
                                            br(), 
                                            a(href = '', 'Dr. Wei Zhang', target = '_blank'), "(Assistant Professor of Agricultural & Applied Economics)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Graduate Fellows and Research Assistants")),
                                          img(src = "farm.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-julie.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          br(), 
                                          img(src = "---.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/esha-dwibedi-83a63476/', 'Nazmul Huda', target = '_blank'), "(Virginia Tech, Graduate in Geography);",
                                            br(), 
                                            a(href = 'https://www.linkedin.com/in/julie-rebstock', 'Yuanyuan Wen', target = '_blank'), "(Virgina Tech, Graduate in Agricultural & Applied Economics);",
                                            br(), 
                                            a(href = 'www.linkedin.com/in/rachelinman21', 'Samantha Rippley', target = '_blank'), "(Virginia Tech, Graduate in Agricultural Economics)."),
                                          p("", style = "padding-top:10px;") 
                                   ),
                                   column(6, align = "center",
                                          h4(strong("Project Stakeholders")),
                                          img(src = "team-posadas.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sarah.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/briannaposadas/", 'Rachel Henley', target = '_blank'), "(Virginia Cooperative Extension, Powhatan County);",
                                            br(), 
                                            a(href = '', 'Nichole Shuman', target = '_blank'), "(Virginia Cooperative Extension, Goochland County)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          )) ,
                 inverse = T)


# server --------------------------------------------------------------------------------------------------------------------

server <- function(input, output){
  
  goochland_soc <- reactive({
    input$goochland_soc
  })
  
  output$gsoc <- renderPlot({
    
    if(goochland_soc() == "gage"){
      age.func(input$yearSelect_gsoc, "Goochland")
    }
    else if(goochland_soc() == "gind"){
      ind.func(input$yearSelect_gsoc, "Goochland")
    }
    else if(goochland_soc() == "ginc"){
      inc.func(input$yearSelect_gsoc, "Goochland")
    }
    else if(goochland_soc() == "gedu"){
      edu.func(input$yearSelect_gsoc, "Goochland")
    }
    
  })
  
  
  powhatan_soc <- reactive({
    input$powhatan_soc
  })
  
  output$psoc <- renderPlot({
    
    if(powhatan_soc() == "page"){
      age.func(input$yearSelect_psoc, "Powhatan ")
    }
    else if(powhatan_soc() == "pind"){
      ind.func(input$yearSelect_psoc, "Powhatan ")
    }
    else if(powhatan_soc() == "pinc"){
      inc.func(input$yearSelect_psoc, "Powhatan ")
    }
    else if(powhatan_soc() == "pedu"){
      edu.func(input$yearSelect_psoc, "Powhatan ")
    }
    
  })
  
  output$harbour<- renderLeaflet({
    harbour
  })
  
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
  
  gcrop <- reactive({
    input$gcrop
  })
  
  output$gcrop_graph <- renderPlotly({
    if(gcrop() == "gcrop12"){
      gcrop12
    }
    else if(gcrop() == "gcrop21"){
      gcrop21
    }
  })
  
  pcrop <- reactive({
    input$pcrop
  })
  
  output$pcrop_graph <- renderPlotly({
    if(pcrop() == "pcrop12"){
      pcrop12
    }
    else if(pcrop() == "pcrop21"){
      pcrop21
    }
  })
  
  output$gsoil <- renderPlotly({
    gsoil
  })
  
  output$psoil <- renderPlotly({
    psoil
  })
  
  output$luPlot.g <- renderLeaflet({
    luPlot <- g.luPlotFunction(input$luYear.g)
    luPlot
  })
  
  output$g.hotspotMap <- renderLeaflet({
    gl_cnty<- st_read("data/cnty_bndry/Goochland_Boundary.shp") %>% st_transform("+proj=longlat +datum=WGS84") 
    
    g.hotspot.plt <- leaflet()%>%
      addTiles() %>%
      setView(lng=-77.885376, lat=37.684143 , zoom=10) %>%
      addPolygons(data=gl_cnty,
                  fillColor = "transparent")
    begin_year <- input$g.hotspotInput[1]-2000
    end_year <- input$g.hotspotInput[2]-2000
    yr <- c(begin_year:end_year)
    file_list <- paste("data/Parcel_Hotspot/gooch_hotspot_",yr,".shp",sep = "")
    
    for (file in file_list){
      #import the heatspot maps of the selected years
      gl<- st_read(file) %>% st_transform("+proj=longlat +datum=WGS84")
      g.hotspot.plt <- g.hotspot.plt %>% addPolygons(stroke = FALSE,
                                                     data = gl,
                                                     weight = 1,
                                                     smoothFactor=1,
                                                     fillColor = "red",
                                                     fillOpacity = 0.2)
    }
    g.hotspot.plt
  })
  
  
  # Plotting Parcellations
  
  parc.func <- function(data, range, county, cnty){
    
    # Declares initial leaflet, nothing added to it.
    my.parc.plt <- leaflet()%>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = cnty, fillColor = "transparent")
    
    # Sets view based on county
    if(county == "Powhatan"){
      my.parc.plt <- my.parc.plt %>% setView(lng=-77.9188, lat=37.5415 , zoom=10)
    }
    else{
      my.parc.plt <- my.parc.plt %>% setView(lng=-77.885376, lat=37.684143, zoom = 10)
    }
    
    # for loop to add polygons based on what the max year is vs. subsequent years prior
    for(i in range){
      # Adds most recent year's parcellations
      if(i == max(range)){
        my.parc.plt <- my.parc.plt %>%
          addPolygons(data = data %>% filter(year == i), 
                      fillColor = "red", smoothFactor = 0.1, fillOpacity = 1, stroke = FALSE)
      }
      # Adds subsequent year's parcellations
      else {
        my.parc.plt <- my.parc.plt %>%
          addPolygons(data = data %>% filter(year == i), 
                      fillColor = "red", smoothFactor = 0.1, fillOpacity = .25, stroke = FALSE)
      }
    }
    my.parc.plt
  }
  
  
  output$g.parcellationPlot <- renderLeaflet({
    yearRange <- input$g.parcellationRange[1]:input$g.parcellationRange[2]
    parc.func(gooch_parcellation, yearRange, "Goochland", gooch_bndry)
  })
  
  output$p.parcellationPlot <- renderLeaflet({
    yearRange <- input$p.parcellationRange[1]:input$p.parcellationRange[2]
    parc.func(pow_parcellation, yearRange, "Powhatan", pow_bndry)
    
  })
  
  
}




shinyApp(ui = ui, server = server)