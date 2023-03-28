### ### ### ### ### ### ### ### ### ### ### ###

#Create modeling dataframe for linked disturbance biomass modeling
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#3/17/23

# This script prepares a dataframe for modeling of biomass.
# It takes in a variety of datasets, samples them, and
# creates a single joined dataframe

# Prior script: prep_data_linkedDisturbance.R

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("tidyverse","beepr", "terra", "sf", "mapview", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(beepr) # beep() will make a noise. options(error = beep) will beep on any errors
library(terra) #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
library(sf) #New vector data package
library(mapview) #For quick interactive mapping
library(here)

## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep) #activate beep on error

## Load data ----

#Disturbance stack data
disturbanceStack <- terra::rast("data/disturbance_stack_southern_rockies_EPSG32613_int.tif")
datatype(disturbanceStack)

#DEM Data
demDats <- terra::rast("data/topography_southern_rockies.tif")
datatype(demDats)

#Forest data
forestMask <- terra::rast("data/forest_mask_nlcd_srockies.tif")
datatype(forestMask)
forestModal <- terra::rast("data/modal_forest_type_nlcd_srockies.tif")
datatype(forestModal)

#Climate normals data
climNorm <- terra::rast("data/climate_normals_aet_def.tif")
datatype(climNorm)

#Modis
modisNonVeg <- terra::rast("data/Modis_Percent_NonVegetated_SouthernRockies_EPSG_32613.tif")
modisNonTree <- terra::rast("data/Modis_Percent_NonTree_Vegetation_SouthernRockies_EPSG_32613.tif")
modisTree <- terra::rast("data/Modis_Percent_Tree_Cover_SouthernRockies_EPSG_32613.tif")
modisQuality <- terra::rast("data/Modis_Quality_SouthernRockies_EPSG_32613.tif")

#Roads
roads <- sf::st_read("data/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE.shp") %>%
  sf::st_transform(crs(demDats))

#GEDI

#NDVI

### ### ### ### ### ### ### ### ### ### ### ###

# Wrangle simple data ----
modis2020 <- c(modisNonVeg$Percent_NonVegetated_2020_03_05,
               modisNonTree$Percent_NonTree_Vegetation_2020_03_05,
               modisTree$Percent_Tree_Cover_2020_03_05,
               modisQuality$Quality_2020_03_05)


# Wrangle disturbance stack ----




# Generate samples ----
gediPoints <- 


#Not within road buffer
roadBuff <- roads %>% st_buffer(50) #50m buffer

# Pull data from layers to create dataframe ----


