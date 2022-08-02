# Repository for DSPG 2022 Land Use Project

**Graduate fellows - Nazmul Huda, Samantha Rippley, Wen Yuanyuan**

**Undergraduate interns - Rachel Inman, John Malla, Chrisopher Vest**

**Lead Faculty - Susan Chen**

----
## Visit our website [here](https://dspgtools.shinyapps.io/rural_land_use/ "DSPG 2022 Land Use").

This repository is for the Data Science for Public Good Young Scholars' Program, to view other projects like this visit the [DSPG website](https://dspg.aaec.vt.edu/#select=3.html "Data Science for the Public Good").

### How to use this repository

The repository is meant to be ready to use, simply clone the repository and launch the *app.r* file in Rstudio locally. 

Any changes to how the code and graphs look can be changed in the *code* folder, which has many different files properly labelled. The code in this project takes a generic approach, meant to be reproducible for many uses. There are many parameters within a lot of the app's functions and scripts that can be changed and customized.

#### An example:

*Code/ACS/pull_ACS.R* was used to generate R objects for certain years from the American Community Survey using the following parameters

```R
## Setting ALL needed parameters ======================

counties <- "Powhatan"

state <- "VA"
  
years <- 2016
```

This generic approach allows all of our data to be pulled from the American Community Survey real time, so modifying any of the parameters and running the script will allow for quick and easy changes to what is needed.
