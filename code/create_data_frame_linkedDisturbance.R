### ### ### ### ### ### ### ### ### ### ### ###

#Create modeling dataframe for linked disturbance biomass modeling
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#3/17/23

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
list.of.packages <- c("tidyverse","beepr", "terra", "sf", "mapview", "here")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(beepr) # beep() will make a noise. options(error = beep) will beep on any errors
library(terra) #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
library(sf) #New vector data package
library(mapview) #For quick interactive mapping
library(here) #Relative path best practices


## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep) #activate beep on error

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

#Modis
modisNonVeg <- terra::rast(here(rawData, "Modis_Percent_NonVegetated_SouthernRockies_EPSG_32613.tif"))
modisNonTree <- terra::rast(here(rawData, "Modis_Percent_NonTree_Vegetation_SouthernRockies_EPSG_32613.tif"))
modisTree <- terra::rast(here(rawData, "Modis_Percent_Tree_Cover_SouthernRockies_EPSG_32613.tif"))
modisQuality <- terra::rast(here(rawData, "Modis_Quality_SouthernRockies_EPSG_32613.tif"))

#Roads
roads <- sf::st_read(here(rawData, "Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE.shp")) %>%
  sf::st_transform(crs(demDats))

#GEDI
gedi <- sf::st_read(here(sharedData, "SR_GEDI_4A.geojson"))

#NDVI
ndvi <- terra::rast(here(rawData, "greenestNDVIOnly_SouthernRockies_2020-05-01_2020-09-30_EPSG_36913_GEE.tif"))

### ### ### ### ### ### ### ### ### ### ### ###

# SCRIPTED ANALYSIS ----

# Wrangle simple data ----
modis2020 <- c(modisNonVeg$Percent_NonVegetated_2020_03_05,
               modisNonTree$Percent_NonTree_Vegetation_2020_03_05,
               modisTree$Percent_Tree_Cover_2020_03_05)


# Wrangle disturbance stack ----






#Not within road buffer
roadBuff <- roads %>% st_buffer(50) #50m buffer

gedi <- 

# Pull data from layers to create dataframe ----


