### ### ### ### ### ### ### ### ### ### ### ###

#Create modeling dataframe for linked disturbance biomass modeling
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#3/28/23

# This script prepares a dataframe for modeling of biomass.
# It takes in a variety of datasets, samples them, and
# creates a single joined dataframe

# Prior script: prep_data_linkedDisturbance.R & simplify_disturbance_stack.R

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("tidyverse","beepr", "terra", "sf", "mapview", "here", "tictoc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(beepr) # beep() will make a noise. options(error = beep) will beep on any errors
library(terra) #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
library(sf) #New vector data package
library(mapview) #For quick interactive mapping
library(here) #Relative path best practices
library(tictoc) #Benchmarking
library(doParallel) #Package for multi-threaded computation in R
  #Basics to get started: https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
  #or: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
  #NOTE: Only use parallel processing (eg foreach %dopar% call) for operations that take a long time!
  #parallelized operations on Windows (i.e. w/o forking) must have packages & functions fed to %dopar% using 
  #.export for functions and .packages for packages

## Clean workspace & set up environment ----
rm(list=ls()) #Ensure empty workspace if running from beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep) #activate beep on error

parallel = FALSE #Change to "True" to enable parallel computing at geographic difference choke point


#Set up parallel computing
if(parallel == TRUE) {
  corz <- parallel::detectCores()-2 #set cores for doParallel to X=2 less than system cores (1 for OS, 1 for any other operations)
  registerDoParallel(corz) #Register parallel processing
}



## Set relative directories structure ----

# Set output directory & create if doesn't already exist
outDir <- here("data", "derived")
if (!dir.exists(outDir)){
  dir.create(outDir)
}

# Also set data directories
sharedData <- here("data", "shared")
rawData <- here("data", "raw_GEE")
derivedData <- here("data", "derived")


## Load data ----

#Disturbance stack data
disturbanceStack <- terra::rast(here(derivedData, "simple_disturbance_stack_southern_rockies_EPSG32613.tif"))
datatype(disturbanceStack)

#DEM Data
demDats <- terra::rast(here(derivedData, "topography_southern_rockies.tif"))
datatype(demDats)

#Forest data
forestMask <- terra::rast(here(derivedData, "forest_mask_nlcd_srockies.tif"))
datatype(forestMask)
forestModal <- terra::rast(here(derivedData, "modal_forest_type_nlcd_srockies.tif"))
datatype(forestModal)

#Climate normals data
climNorm <- terra::rast(here(derivedData, "climate_normals_aet_def.tif"))
datatype(climNorm)

#SPEI data
spei <- terra::rast(here(rawData, "spei1y_Annual_SouthernRockies_EPSG_32613.tif"))

#Modis
modisNonVeg <- terra::rast(here(rawData, "Modis_Percent_NonVegetated_SouthernRockies_EPSG_32613.tif"))
modisNonTree <- terra::rast(here(rawData, "Modis_Percent_NonTree_Vegetation_SouthernRockies_EPSG_32613.tif"))
modisTree <- terra::rast(here(rawData, "Modis_Percent_Tree_Cover_SouthernRockies_EPSG_32613.tif"))
modisQuality <- terra::rast(here(rawData, "Modis_Quality_SouthernRockies_EPSG_32613.tif"))

#Roads
roads <- sf::st_read(here(rawData, "Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE.shp")) %>%
  sf::st_transform(crs(demDats))

#GEDI
gedi <- sf::st_read(here(sharedData, "GEDI_4A_Hayman.geojson")) %>% sf::st_transform(crs(demDats))
mapview(gedi)

#NDVI
ndvi <- terra::rast(here(rawData, "greenestNDVI_LandsatDerived_SouthernRockies_2020-05-01_2020-09-30_EPSG_32613_GEE.tif"))

### ### ### ### ### ### ### ### ### ### ### ###

# SCRIPTED ANALYSIS ----

# Manipulate GEDI data ----
## Get only points NOT within road buffer ----
gediBB <- sf::st_bbox(gedi) %>%
  sf::st_as_sfc()

#Perform road buffer
#buffer justification:
#lanes are 3.7m each. 4 lanes = 15m, /2 = 7.5. +  13m = ~ 1/2 of GEDI 25m diameter.
# = 21m buffer on each side of road line
# Increase to 30m to be safe
tic("buffer")
roadBuff <- roads %>%
  sf::st_filter(gediBB) %>%
  sf::st_buffer(30) %>%
  sf::st_union()
toc()



#Perform geographic difference w/ road buffer
#Parallel compute option for very large datasets
if(parallel == TRUE) {
  tic("difference_parallel")
  gedi <- foreach(i = 1:nrow(gedi),
                  .packages = 'sf',
                  .combine = 'rbind') %dopar% {
                    pt <- gedi[i,] %>%
                      sf::st_difference(roadBuff)
                    return(pt)
                  }
  toc()
} else {
  tic("difference")
  gedi <- gedi %>% sf::st_difference(roadBuff)
  toc()
}


## Add raw lat/long to gedi data points ----
#from https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column
gedi <- gedi %>%
  mutate(long = unlist(map(gedi$geometry, 1)),
         lat = unlist(map(gedi$geometry, 2)))



# Wrangle simple data & extract ----

# Modis
modis2020 <- c(modisNonVeg$Percent_NonVegetated_2020_03_05,
               modisNonTree$Percent_NonTree_Vegetation_2020_03_05,
               modisTree$Percent_Tree_Cover_2020_03_05)

modisD <- modis2020 %>%
  terra::extract(gedi) %>%
  setNames(c("id", "modis2020nonVeg", "modis2020nonTreeVeg", "modis2020TreeVeg")) %>%
  select(-"id")

# Climate
clim1 <- climNorm %>%
  terra::extract(gedi) %>%
  setNames(c("id", "aetNorm", "defNorm")) %>%
  select(-"id")

clim2 <- spei %>%
  terra::extract(gedi) %>%
  select(-"ID")

climateD <- cbind(clim1, clim2)

# NDVI
ndviD <- ndvi %>%
  terra::extract(gedi) %>%
  select(-"ID") %>%
  setNames("peakNDVI2020")

# DEM
demD <- demDats %>%
  terra::extract(gedi) %>%
  select(-"ID", -"hillshade")


# Forest data
forest1 <- forestMask %>%
  terra::extract(gedi) %>%
  select(-"ID") %>%
  setNames("forestMask")

forest2 <- forestModal %>%
  terra::extract(gedi) %>%
  select(-"ID") %>%
  setNames("forestCode") %>%
  mutate(forestType = case_when(forestCode == 41 ~ 'deciduous',
                                forestCode == 42 ~ 'evergreen',
                                forestCode == 43 ~ 'mixed',
                                forestCode == 0 ~ 'noForest'))

forestD <- cbind(forest1, forest2)


# Wrangle disturbance stack ----

distD <- disturbanceStack %>%
  terra::extract(gedi) %>%
  select(-"ID") %>%
  rev() #reverse order so that most recent (2020) is column 1, 2019 is column 2, etc

#Codes:
# 0 - None
# 1 - Fire
# 2 - Insect/Disease
# 3 - Hot Drought
# 4 - Fire + Hot Drought
# 5 - Insect/Disease + Hot Drought

fire <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                 .==1 ~ 1,
                                 .==2 ~ 0,
                                 .==3 ~ 0,
                                 .==4 ~ 1,
                                 .==5 ~ 0)))

hotDrought <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                 .==1 ~ 0,
                                 .==2 ~ 0,
                                 .==3 ~ 1,
                                 .==4 ~ 1,
                                 .==5 ~ 0)))

insect <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                 .==1 ~ 0,
                                 .==2 ~ 1,
                                 .==3 ~ 0,
                                 .==4 ~ 0,
                                 .==5 ~ 1)))

anyCombo <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                 .==1 ~ 0,
                                 .==2 ~ 0,
                                 .==3 ~ 0,
                                 .==4 ~ 1,
                                 .==5 ~ 1)))

#Function to create a column of data with number of years since the disturbance has been present
#Dats is a dataframe with years as columns, descending
#distNm is the name of the disturbance represented in the dataset. Should be CAPITALIZED
time_since_last <- function(dats, distNm) {
  colNm <- paste("yrsSince", distNm, sep = '')
  #Use 99 as entry when there are no cases of the disturbance in the stack
  #Otherwise, calculate years since
  dats <- dats %>% transmute({{colNm}} := case_when(rowSums(across(1:ncol(dats)))==0 ~ 99,
                                                     rowSums(across(1:ncol(dats)))!=0 ~ max.col(dats, ties.method = "first") - 1))
  return(dats)
}

#Function to create a column of data with number of years a disturbance has been present in last X years
#Dats is a dataframe with years as columns, descending
#distNm is the name of the disturbance represented in the dataset. Should be LOWERCASE
#xYrs is the number of years since the first column to consider
years_in_last <- function(dats, distNm, xYrs) {
  colNm <- paste(distNm, "YrsInLast", xYrs, sep = '')
  dats <- dats %>% transmute({{colNm}} := rowSums(across(1:xYrs)))
  return(dats)
}

#Function to run years_in_last for 5, 10, 15, and 20 years, then bind together and return
#DEPRECATED & UNSUSED
all_years_in_last_binned <- function(dats, distNm) {
  d <- cbind(years_in_last(dats = dats, distNm = distNm, xYrs = 5),
             years_in_last(dats = dats, distNm = distNm, xYrs = 10),
             years_in_last(dats = dats, distNm = distNm, xYrs = 15),
             years_in_last(dats = dats, distNm = distNm, xYrs = 20))
  return(d)
}

#Function to run years_in_last for all 22 years, then bind together and return
all_years_in_last_full <- function(dats, distNm) {
  d <- years_in_last(dats = dats, distNm = distNm, xYrs = 1)
  for (i in 2:ncol(dats)) {
    d <- cbind(d, years_in_last(dats = dats, distNm = distNm, xYrs = i))
  }
  return(d)
}

## Run functions on all data types ----

fireD <- time_since_last(fire, "Fire") %>%
  cbind(all_years_in_last_full(fire, "fire"))
hotD <- time_since_last(hotDrought, "HotDrought") %>%
  cbind(all_years_in_last_full(hotDrought, "hotDrought"))
insectD <- time_since_last(insect, "Insect") %>%
  cbind(all_years_in_last_full(insect, "insect"))
comboD <- time_since_last(anyCombo, "Combo") %>%
  cbind(all_years_in_last_full(anyCombo, "combo"))


#Bind together disturbance data
distD <- cbind(hotD, fireD, insectD, comboD)


# Bind together all data, remove non-forest, and export ----

df <- gedi %>%
  cbind(modisD) %>%
  cbind(climateD) %>%
  cbind(ndviD) %>%
  cbind(demD) %>%
  cbind(forestD) %>%
  cbind(distD)

df <- df %>% filter(forestMask == 1)

write.csv(df, here(outDir, "baby_data_frame.csv"))

if(parallel == TRUE) {
  stopImplicitCluster()
}
