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

# save(population.fnl ,file=popFile)
# 
# load(popFile) LOAD INTO ONE OBJECT INSTEAD



### Employment ======================          
                 
emp_age.var <- c(total      = "S2301_C01_001E",
                 bet16and19 = "S2301_C01_002E",
                 bet20and24 = "S2301_C01_003E",
                 bet25and29 = "S2301_C01_004E",
                 bet30and34 = "S2301_C01_005E",
                 bet35and44 = "S2301_C01_006E",
                 bet45and54 = "S2301_C01_007E",
                 bet55and59 = "S2301_C01_008E",
                 bet60and64 = "S2301_C01_009E",
                 bet64and74 = "S2301_C01_010E",
                 above75    = "S2301_C01_011E")

employment_age <- get_acs(geography = "tract",
                          county = counties, 
                          state = state, 
                          geometry = TRUE,
                          year = years, 
                          cache_table = TRUE,
                          variables = emp_age.var, 
                          output = "wide") %>% select(GEOID, 
                                                      NAME,
                                                      names(emp_age.var), 
                                                      geometry)

employment_age$NAME <- str_replace(employment_age$NAME, ", Powhatan County, Virginia", "")
df <- data.frame(employment_age)
sum1 <- round(df[1,3:13] / df[1,"total"], 4)
sum2 <- round(df[2,3:13] / df[2,"total"], 4)
sum3 <- round(df[3,3:13] / df[3,"total"], 4)
sum4 <- round(df[4,3:13] / df[4,"total"], 4)
sum5 <- round(df[5,3:13] / df[5,"total"], 4)
sum6 <- round(df[6,3:13] / df[6,"total"], 4)

employment_age[1,3:13] <- sum1
employment_age[2,3:13] <- sum2
employment_age[3,3:13] <- sum3
employment_age[4,3:13] <- sum4
employment_age[5,3:13] <- sum5
employment_age[6,3:13] <- sum6           
                 
### Occupation ======================


### Income ===========================


### Education ======================


### Population Retention ======================



### Transportation ======================
                 
                 