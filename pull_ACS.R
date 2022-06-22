## Pulling and saving data from ACS

setwd("C:/Users/malla/OneDrive/Desktop/DSPG/2022_DSPG_landuse")

## Importing libraries ============

library(tigris)
library(dplyr)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(plotly)
library(readxl)
library(writexl)
library(sf)
options(scipen=999)


## Setting parameters ============

counties <- "Powhatan"

state <- "VA"
#fips <- 51145 I never used this, will take out
  
years <- 2020
endFile <- paste0(years, sep="", ".Rda")

## Output file names ==============

dataDirectory <- "C:/Users/malla/OneDrive/Desktop/DSPG/2022_DSPG_landuse/csv_data/"

popFile <- paste0(paste0(dataDirectory, sep='', "population"), sep='', endFile)
empFile <- paste0(paste0(dataDirectory, sep='', "employment"), sep='', endFile)
occFile <- paste0(paste0(dataDirectory, sep='', "occupation"), sep='', endFile)
incFile <- paste0(paste0(dataDirectory, sep='', "income"), sep='', endFile)
eduFile <- paste0(paste0(dataDirectory, sep='', "education"), sep='', endFile)
popRetFile <- paste0(paste0(dataDirectory, sep='', "populationRetention"), sep='', endFile)         
transFile <- paste0(paste0(dataDirectory, sep='', "transportation"), sep='', endFile)                 
     

## Pulling the data =================


### Population ======================
                
pop.var <- c(total        = "S0101_C01_001E",
             under5       = "S0101_C01_002E", 
             bet5and9     = "S0101_C01_003E", 
             bet10and14   = "S0101_C01_004E", 
             bet15adn19   = "S0101_C01_005E", 
             bet20and24   = "S0101_C01_006E",
             bet25and29   = "S0101_C01_007E",
             bet30and34   = "S0101_C01_008E",
             bet35and39   = "S0101_C01_009E",
             bet40and44   = "S0101_C01_010E",
             bet45and49   = "S0101_C01_011E",
             bet50and54   = "S0101_C01_012E",
             bet55and59   = "S0101_C01_013E",
             bet60and64   = "S0101_C01_014E",
             bet65and69   = "S0101_C01_015E",
             bet70and74   = "S0101_C01_016E",
             bet75and79   = "S0101_C01_017E",
             bet80and84   = "S0101_C01_018E",
             above85      = "S0101_C01_019E")

population <- get_acs(geography = "tract",
                      county = counties, 
                      state = state, 
                      geometry = TRUE,
                      year = years, 
                      cache_table = TRUE,
                      variables = pop.var) %>% select(GEOID, 
                                                      NAME,
                                                      estimate,
                                                      geometry)
population$variable <- names(pop.var)
population$NAME <- str_replace(population$NAME, ", Powhatan County, Virginia", "")

# Converting each tract to have a percent population breakdown instead of total.
population.fnl <- population %>% group_by(NAME) %>% mutate(estimate = estimate / max(estimate))                 

save(population.fnl ,file=popFile)

load(popFile)

population.fnl.df <- as.data.frame(population.fnl)
population.fnl.sf <- population.fnl$geometry

write_xlsx(population.fnl, paste(popFile, sep='', ".xlsx"))


write.csv2(population.fnl, paste(popFile, sep='', ".csv"))
st_write(population.fnl.sf, paste(popFile, sep='', ".shp"))

st.read <- st_read("C:/Users/malla/OneDrive/Desktop/DSPG/2022_DSPG_landuse/csv_data/population2020.shp")


### Employment ======================          
                 
                 
                 
### Occupation ======================


### Income ===========================


### Education ======================


### Population Retention ======================



### Transportation ======================
                 
                 