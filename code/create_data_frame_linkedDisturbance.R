### ### ### ### ### ### ### ### ### ### ### ###

#Create modeling dataframe for linked disturbance biomass modeling
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#Last updated: 3/30/23

# This script prepares a dataframe for modeling of biomass.
# It takes in a variety of datasets, samples them, and
# creates a single joined dataframe

# The script is set up to work for GEDI collection dates from 2019 - 2021.
# Any other collection dates will break the script

# Prior script: prep_data_linkedDisturbance.R & simplify_disturbance_stack.R

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("tidyverse","beepr", "terra", "sf", "mapview", "here", "tictoc", "doParallel", "zip")
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
library(zip) #For zipping files (does not require external tools). mode = "cherry-pick" to select specific files w/o mirroring directory structure

## Clean workspace & set up environment ----
rm(list=ls()) #Ensure empty workspace if running from beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep) #activate beep on error

parallel = FALSE #Change to "True" to enable parallel computing at geographic difference choke point
dfName = "linked_disturbance_baby_data_frame" #NAME OF OUTPUT DF

#Set up parallel computing
if(parallel == TRUE) {
  corz <- parallel::detectCores()-2 #set cores for doParallel to X=2 less than system cores (1 for OS, 1 for any other operations)
  registerDoParallel(corz) #Register parallel processing
}



## Set relative directories structure ----

# Set output directory & create if doesn't already exist
outDir <- here("data", "final")
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
spei <- terra::rast(here(rawData, "spei1y_Annual_SouthernRockies_EPSG_32613_GEE.tif"))

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
ndviFileNms <- list.files(here(rawData),
                          pattern = "greenestNDVI*",
                          full.names = TRUE)
ndvi <- terra::rast(ndviFileNms)
names(ndvi) <- c("NDVI2019", "NDVI2020", "NDVI2021")

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


## Add basic gedi data ----

#Raw lat/long
#from https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column
gedi <- gedi %>%
  mutate(utm_z13n_easting = unlist(map(gedi$geometry, 1)),
         utm_z13n_northing = unlist(map(gedi$geometry, 2)))

#Also rename original lat/long
gedi <- gedi %>%
  rename(latEPSG4326 = lat_lowestmode,
         longEPSG4326 = lon_lowestmode)

#Add collection date
###########################################
#FIX THIS TO BE CORRECT -- THIS IS JUST CREATING FAKE DATA RIGHT NOW
###########################################
gedi <- gedi %>%
  mutate(gediYear = case_when(round(utm_z13n_easting, 0) %% 2 == 0 ~ 2020,
                              round(utm_z13n_easting, 0) %% 2 != 0 ~ 2019))

gediYr <- setNames(gedi$gediYear, "gediYear")

# Wrangle simple data & extract ----

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

# Wrangle Climate, Modis, & NDVI data ----


## Modis ----
modis <- c(modisNonVeg$Percent_NonVegetated_2019_03_06,
           modisNonTree$Percent_NonTree_Vegetation_2019_03_06,
           modisTree$Percent_Tree_Cover_2019_03_06,
           modisNonVeg$Percent_NonVegetated_2020_03_05,
           modisNonTree$Percent_NonTree_Vegetation_2020_03_05,
           modisTree$Percent_Tree_Cover_2020_03_05)

modisD <- modis %>%
  terra::extract(gedi)%>%
  select(-"ID") %>%
  cbind(gediYr) %>%
  mutate(modisNonVeg = case_when(gediYr == 2021 ~ NA,
                                 gediYr == 2020 ~ Percent_NonVegetated_2020_03_05,
                                 gediYr == 2019 ~ Percent_NonVegetated_2019_03_06)) %>%
  mutate(modisNonTreeVeg = case_when(gediYr == 2021 ~ NA,
                                 gediYr == 2020 ~ Percent_NonTree_Vegetation_2020_03_05,
                                 gediYr == 2019 ~ Percent_NonTree_Vegetation_2019_03_06)) %>%
  mutate(modisTreeVeg = case_when(gediYr == 2021 ~ NA,
                                 gediYr == 2020 ~ Percent_Tree_Cover_2020_03_05,
                                 gediYr == 2019 ~ Percent_Tree_Cover_2019_03_06)) %>%
  select(modisNonVeg, modisNonTreeVeg, modisTreeVeg)


## Climate ----
climNormD <- climNorm %>%
  terra::extract(gedi) %>%
  setNames(c("id", "aetNorm", "defNorm")) %>%
  select(-"id")


#Function to get spei values for years prior to GEDI collection
#Takes in a dataframe with values for SPEI from 2020 to 1999 as columns, + a gedi collection year (gediYr)
spei_in_x_yr_prior <- function(dats, x) {
  colNm <- paste("spei", x, "YrPrior", sep = '')
  dats <- dats %>% transmute({{colNm}} := case_when(gediYr == 2021 ~ .[[x]],
                                                    gediYr == 2020 ~ case_when(1+x > 22 ~ NA, #out of timeframe, return NA
                                                                               TRUE ~ .[[x+1]]),
                                                    gediYr == 2019 ~ case_when(2+x > 22 ~ NA, #out of timeframe, return NA
                                                                               TRUE <= 22 ~ .[[min(x+2, ncol(.))]])))
  return(dats)
}


#Function to run spei_in_x_yr_prior for all 22 years, then bind together and return
spei_in_x_yr_prior_full <- function(dats) {
  d <- spei_in_x_yr_prior(dats = dats, x = 1)
  for (i in 2:(ncol(dats)-1)) {
    d <- cbind(d, spei_in_x_yr_prior(dats = dats, x = i))
  }
  return(d)
}

#Run on SPEI
climSPEID <- spei %>%
  terra::extract(gedi) %>%
  select(-"ID") %>%
  rev() %>%
  cbind(gediYr) %>%
  spei_in_x_yr_prior_full()

climateD <- cbind(climNormD, climSPEID)

## NDVI ----
ndviD <- ndvi %>%
  terra::extract(gedi) %>%
  select(-"ID") %>%
  cbind(gediYr) %>%
  mutate(peakNDVI = case_when(gediYr == 2021 ~ NDVI2021,
                              gediYr == 2020 ~ NDVI2020,
                              gediYr == 2019 ~ NDVI2019)) %>%
  select(peakNDVI)


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
                                 .==5 ~ 0))) %>%
  cbind(gediYr)

hotDrought <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                 .==1 ~ 0,
                                 .==2 ~ 0,
                                 .==3 ~ 1,
                                 .==4 ~ 1,
                                 .==5 ~ 0))) %>%
  cbind(gediYr)

insect <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                 .==1 ~ 0,
                                 .==2 ~ 1,
                                 .==3 ~ 0,
                                 .==4 ~ 0,
                                 .==5 ~ 1))) %>%
  cbind(gediYr)

anyCombo <- distD %>%
  mutate(across(1:ncol(distD), ~case_when(.==0 ~ 0,
                                 .==1 ~ 0,
                                 .==2 ~ 0,
                                 .==3 ~ 0,
                                 .==4 ~ 1,
                                 .==5 ~ 1))) %>%
  cbind(gediYr)


#Function to create two columns of data:
#####number of years since the disturbance has been present (not including gedi collection year), and
#####if disturbance was present in gedi collection year (binary)
#Dats is a dataframe with years as columns, descending. gediYr should the last column
#distNm is the name of the disturbance represented in the dataset. Should be CAPITALIZED
time_since_last <- function(dats, distNm) {
  colNm1 <- paste("yrsSince", distNm, sep = '')
  colNm2 <- paste("collectionYr", distNm, sep = "")
  #Use NA as entry when there are no cases of the disturbance in the stack prior to gedi collection date
  #Otherwise, calculate years since, NOT including year of data collection
  yrsS <- dats %>% transmute({{colNm1}} := case_when(gediYr == 2021 ~ case_when(rowSums(across(`forest-disturbance-s-rockies-2020`:`forest-disturbance-s-rockies-1999`))==0 ~ NA,
                                                                               rowSums(across(`forest-disturbance-s-rockies-2020`:`forest-disturbance-s-rockies-1999`))!=0 ~ 
                                                                                 max.col(across(`forest-disturbance-s-rockies-2020`:`forest-disturbance-s-rockies-1999`), ties.method = "first")),
                                                    gediYr == 2020 ~ case_when(rowSums(across(`forest-disturbance-s-rockies-2019`:`forest-disturbance-s-rockies-1999`))==0 ~ NA,
                                                                               rowSums(across(`forest-disturbance-s-rockies-2019`:`forest-disturbance-s-rockies-1999`))!=0 ~ 
                                                                                 max.col(across(`forest-disturbance-s-rockies-2019`:`forest-disturbance-s-rockies-1999`), ties.method = "first")),
                                                    gediYr == 2019 ~ case_when(rowSums(across(`forest-disturbance-s-rockies-2018`:`forest-disturbance-s-rockies-1999`))==0 ~ NA,
                                                                               rowSums(across(`forest-disturbance-s-rockies-2018`:`forest-disturbance-s-rockies-1999`))!=0 ~ 
                                                                                 max.col(across(`forest-disturbance-s-rockies-2018`:`forest-disturbance-s-rockies-1999`), ties.method = "first"))))
  #calculate collection year disturbance binary
  cYD <- dats %>% transmute({{colNm2}} := case_when(gediYr == 2021 ~ NA,
                                                    gediYr == 2020 ~ `forest-disturbance-s-rockies-2020`,
                                                    gediYr == 2019 ~ `forest-disturbance-s-rockies-2019`))
  return(cbind(yrsS, cYD))
}

#Function to create a column of data with number of years a disturbance has been present in the prior X years
#Dats is a dataframe with years as columns, descending. gediYr should be the last column, designating gedi collection year
#distNm is the name of the disturbance represented in the dataset. Should be LOWERCASE
#xYrs is the number of years since the first column to consider
years_in_prior <- function(dats, distNm, xYrs) {
  colNm <- paste(distNm, "YrsInPrior", xYrs, sep = '')
  dats <- dats %>% transmute({{colNm}} := case_when(gediYr == 2021 ~ rowSums(across(1:(xYrs))),
                                                    gediYr == 2020 ~ case_when(1+xYrs > 22 ~ NA, #out of timeframe
                                                                               TRUE ~ rowSums(across(2:(1+xYrs)))),
                                                    gediYr == 2019 ~ case_when(2+xYrs > 22 ~ NA, #out of timeframe
                                                                               TRUE <= 22 ~ rowSums(across(3:min(2+xYrs, ncol(.)))))))
  return(dats)
}

#Function to run years_in_prior for all 22 years, then bind together and return
all_years_in_prior_full <- function(dats, distNm) {
  d <- years_in_prior(dats = dats, distNm = distNm, xYrs = 1)
  for (i in 2:(ncol(dats)-1)) {
    d <- cbind(d, years_in_prior(dats = dats, distNm = distNm, xYrs = i))
  }
  return(d)
}

## Run functions on all data types ----

fireD <- time_since_last(fire, "Fire") %>%
  cbind(all_years_in_prior_full(fire, "fire"))
hotD <- time_since_last(hotDrought, "HotDrought") %>%
  cbind(all_years_in_prior_full(hotDrought, "hotDrought"))
insectD <- time_since_last(insect, "Insect") %>%
  cbind(all_years_in_prior_full(insect, "insect"))
comboD <- time_since_last(anyCombo, "Combo") %>%
  cbind(all_years_in_prior_full(anyCombo, "combo"))

#Bind together disturbance data
distD <- cbind(hotD, fireD, insectD, comboD)


# Bind together all data, filter, and export ----

df <- gedi %>%
  cbind(modisD) %>%
  cbind(ndviD) %>%
  cbind(demD) %>%
  cbind(forestD) %>%
  cbind(climateD) %>%
  cbind(distD)

## Filter data ----

df <- df %>%
  filter(forestMask == 1) %>% #Forest-only
  filter(collectionYrFire == 0) %>% #Remove data points where a fire or insect disturbance occurred in the collection year
  filter(collectionYrInsect == 0)


## Create metadata ----
columns <- c(colnames(df)[1:19],
             "spei...",
             "yrsSince...",
             "collectionYr...",
             "...YrsInPriorX",
             "...combo...")
description <- c("Latitude in EPSG4326",
                 "Longitude in EPSG4326",
                 "Aboveground Biomass from GEDI 4A product",
                 "UTM Z 13N Northing",
                 "UTM Z 13N Easting",
                 "Year of GEDI data collection",
                 "Modis non vegetated % cover in year of GEDI data collection, NA indicates data unavailable",
                 "Modis non-tree vegetated % cover in year of GEDI data collection, NA indicates data unavailable",
                 "Modis tree % cover in year of GEDI data collection, NA indicates data unavailable",
                 "Peak NDVI in year of GEDI data collection, derived from Landsat",
                 "Topographic aspect derived from USGS SRTM 30m DEM",
                 "Topographic slope derived from USGS SRTM 30m DEM",
                 "Topographic elevation from USGS SRTM 30m DEM",
                 "McCune & Keon Heat Load Index (HLI) from USGS SRTM 30m DEM",
                 "Forest mask, binary: pixels with any type of forested cover at any point in NLCD dataset",
                 "Forest code: NLCD codes of modal forest cover for pixel",
                 "Forest type: NLCD human-readable forest cover type associated with code",
                 "30-yr average of AET from TopoFire, Holden et al.",
                 "30-yr average of CWD from TopoFire, Holden et al.",
                 "Standardized Precipitaion Evapotranspiration Index (SPEI) with climatic water balance aggregated over the year prior to a given date. Averaged for each calendar year and provided for each year X years prior to GEDI data collection. NA indicates out of data timeframe",
                 "Number of years since an event of the given disturbance type, exclusive of GEDI collection year. NA indicates disturbance was never present in available data",
                 "Presence of given disturbance in year of GEDI collection, binary",
                 "Number of years of given disturbance in the X years prior to GEDI data collection. NA indicates that X is beyond the time frame of the available data",
                 "'Combo' indicates a combined disturbance of either hotterDrought & insects or hotterDrought & fire occurring in the same year")
df_metadata <- cbind(columns, description)


## Write & zip Data & Metadata files ----

#Write data
dfFile <- paste(dfName, ".csv", sep="")
write.csv(df, here(outDir, dfFile))

stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

#Sink metadata to text file
sink(here(outDir, "metadata.md"))
cat("# Metadata for the Southern Rockies Linked Disturbance GEDI Dataset \n")
cat("This dataset includes GEDI points from 2019-2021 that are over forested areas of the Southern Rockies. \n")
cat("Data points within a buffer of Census Bureau road polylines have been removed. \n")
cat("Data points that had a fire or insect disturbance in the year of GEDI data collection have been removed. \n")
cat("Disturbance data is from the disturbance stack derived by CU Boulder's CIRES Earth Lab in 2023. \n")
cat("\n")
cat("## Information \n")
cat("Author: Tyler L. McIntosh \n")
cat("Date generated: ", stamp, "\n")
cat("[GitHub repo with code for reproduction](https://github.com/tylerhoecker/linked_disturbance)")
cat("\n")
cat("\n")
cat("## Metadata \n")
cat(paste0(colnames(df_metadata), collapse = ' :: '), sep = "\n")
cat("\n")
cat(apply(df_metadata,1,paste0, collapse=' :: '), sep = "\n")
sink()

#Zip together
zip::zip(zipfile = here(outDir, paste(dfName, ".zip", sep="")),
    files = c(here(outDir, dfFile),
              here(outDir, "metadata.md")),
    mode = "cherry-pick")

#Remove floating files
file.remove(here(outDir, dfFile))
file.remove(here(outDir, "metadata.md"))

# Clean up ----
#Stop cluster if on
if(parallel == TRUE) {
  stopImplicitCluster()
}


