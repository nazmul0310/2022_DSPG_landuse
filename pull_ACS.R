## Pulling and saving data from ACS

setwd("C:/Users/malla/OneDrive/Desktop/DSPG/2022_DSPG_landuse")

## Importing libraries ============

library(dplyr)
library(tidycensus)
options(scipen=999)

## Setting parameters ======================

counties <- "Powhatan"

state <- "VA"
  
years <- 2020

## Output file names =======================

dataDirectory <- "C:/Users/malla/OneDrive/Desktop/DSPG/2022_DSPG_landuse/csv_data/"

fileOutput <- paste0("ACS", years, counties, ".rData")

     

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
population$NAME <- str_replace(population$NAME, paste0(", ", counties, " County, Virginia"), "")

# Converting each tract to have a percent population breakdown instead of total.
population.fnl <- population %>% group_by(NAME) %>% mutate(estimate = estimate / max(estimate))                 

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

employment_age$NAME <- str_replace(employment_age$NAME, paste0(", ", counties, " County, Virginia"), "")
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


ind.var <- c(paste("C24050_00", sep = '', 2:9), paste("C24050_01", sep='', 0:4))

names(ind.var) <- c("Agriculture, forestry, fishing and hunting, and mining", 
                    "Construction", 
                    "Manufacturing", 
                    "Wholesale trade", 
                    "Retail trade", 
                    "Transportation and warehousing, and utilities", 
                    "Information", 
                    "Finance and insurance, and real estate and rental and leasing", 
                    "Professional, scientific, and management, \nand administrative and waste management services", 
                    "Educational services, and health care and social assistance", 
                    "Arts, entertainment, and recreation, \nand accommodation and food services", 
                    "Other services, except public administration", 
                    "Public administration")

industry <- get_acs(geography = "county",
                      county = counties,
                      state = "VA", 
                      year = years,
                      cache_table = TRUE,
                      variables = ind.var)
industry <- industry %>% select(-moe)
industry <- industry %>% mutate(estimate = estimate / sum(estimate) * 100)

### Income ===========================

inc.var <- c(median         = "S1901_C01_012E",
             below10k       = "S1901_C01_002E",
             bet10kand15k   = "S1901_C01_003E",
             bet15kand25k   = "S1901_C01_004E",
             bet25kand35k   = "S1901_C01_005E",
             bet35kand50k   = "S1901_C01_006E",
             bet50kand74k   = "S1901_C01_007E",
             bet75kand100k  = "S1901_C01_008E",
             bet100kand150k = "S1901_C01_009E",
             bet150kand200k = "S1901_C01_010E",
             above200k      = "S1901_C01_011E")

income <- get_acs(geography = "tract",
                  county = counties, 
                  state = state, 
                  geometry = TRUE,
                  year = years, 
                  cache_table = TRUE,
                  variables = inc.var, 
                  output = "wide") %>% select(GEOID, 
                                              NAME,
                                              names(inc.var), 
                                              geometry)

income$NAME <- str_replace(income$NAME, paste0(", ", counties, " County, Virginia"), "")

### Education ======================


edu_earnings.var <- c(belowHigh      = "S1501_C01_060E", 
                      high           = "S1501_C01_061E",
                      someCollege    = "S1501_C01_062E",
                      bachelors      = "S1501_C01_063E",
                      gradSchool     = "S1501_C01_064E")

education_earn <- get_acs(geography = "county",
                     county = counties, 
                     state = state, 
                     year = years, 
                     cache_table = TRUE,
                     variables = edu_earnings.var) %>% select(GEOID,
                                                              NAME,
                                                              variable, 
                                                              estimate)

education_earn$variable <- names(edu_earnings.var)

### Population Retention ======================

ret.var <- c(moved_same = "S0701_C02_001E",
             moved_same_state = "S0701_C03_001E",
             moved_diff_state = "S0701_C04_001E",
             moved_abroad = "S0701_C05_001E")

retention <- get_acs(geography = "county",
                     county = counties, 
                     state = state, 
                     year = years,
                     cache_table = TRUE,
                     output = "wide",
                     variables = ret.var) %>% select(GEOID, 
                                                     NAME,
                                                     names(ret.var))


### Transportation ======================
           
trans.var <- c(total            = "B08101_001E",
               car_tv_alone     = "B08101_009E",
               car_tv_carpooled = "B08101_017E",
               public_transport = "B08101_025E",
               walked           = "B08101_033E",
               bike_taxi        = "B08101_041E",
               from_home        = "B08101_049E")

transportation <- get_acs(geography = "county",
                          county = counties, 
                          state = state,
                          year = years, 
                          cache_table = TRUE,
                          variables = trans.var) %>% select(GEOID, 
                                                            NAME,
                                                            estimate)

transportation$NAME <- str_replace(transportation$NAME, paste0(", ", counties, " County, Virginia"), "")
transportation$travel_type <- names(trans.var)

transportation <- transportation %>% mutate(estimate = estimate / sum(estimate[-1]))
transportation <- transportation[-1,]  

### Occupation ==================================

ocu.vars <- c(total = "B24011_001",
              "Management, business, science, and arts occupations" = "B24011_002",
              "Management, business, and financial occupations" = "B24011_003",
              "Management occupations" = "B24011_004",
              "Business and financial operations occupations" = "B24011_005",
              "Computer, engineering, and science occupations" = "B24011_006",
              "Computer and mathematical occupations" = "B24011_007",
              "Architecture and engineering occupations" = "B24011_008",
              "Life, physical, and social science occupations" = "B24011_009",
              "Education, legal, community service, arts, and media occupations" = "B24011_010",
              "Community and social service occupations" = "B24011_011",
              "Legal occupations" = "B24011_012",
              "Educational instruction, and library occupations" = "B24011_013",
              "Arts, design, entertainment, sports, and media occupations" = "B24011_014",
              "Healthcare practitioners and technical occupations" = "B24011_015",
              "Health diagnosing and treating practitioners and other technical occupations" = "B24011_016",
              "Health technologists and technicians" = "B24011_017",
              "Service occupations" = "B24011_018",
              "Healthcare support occupations" = "B24011_019",
              "Protective service occupations" = "B24011_020",
              "Firefighting and prevention, and other protective service workers including supervisors" = "B24011_021",
              "Law enforcement workers including supervisors" = "B24011_022",
              "Food preparation and serving related occupations" = "B24011_023",
              "Building and grounds cleaning and maintenance occupations" = "B24011_024",
              "Personal care and service occupations" = "B24011_025",
              "Sales and office occupations" = "B24011_026",
              "Sales and related occupations" = "B24011_027",
              "Office and administrative support occupations" = "B24011_028",
              "Natural resources, construction, and maintenance occupations" = "B24011_029",
              "Farming, fishing, and forestry occupations" = "B24011_030",
              "Construction and extraction occupations" = "B24011_031",
              "Installation, maintenance, and repair occupations" = "B24011_032",
              "Production, transportation, and material moving occupations" = "B24011_033",
              "Production occupations" = "B24011_034",
              "Transportation occupations" = "B24011_035",
              "Material moving occupations" = "B24011_036")

occupation <- get_acs(geography = "county",
                                       state = "VA",
                                       county = "Goochland",
                                       variables = ocu.vars,
                                       survey = "acs5",
                                       geometry=TRUE,
                                       year = years) # gets earnings

occupation %>% select(-moe) # These are earnings

  

## Saving the objects ============================

save(population.fnl, employment_age, industry, income, education_earn, retention, 
     transportation, occupation, file = paste0(dataDirectory, fileOutput))



## Loading the data ===============================

#load(paste0(getwd(), "/csv_data/ACS2020Goochland.rData"))
















                 