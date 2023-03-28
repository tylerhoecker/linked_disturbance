### ### ### ### ### ### ### ### ### ### ### ###

#Simplify disturbance stack
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#3/28/2022

#This script takes in the disturbance stack derived for the
#Macrosystems 2023 Forest Resiliency Data Synthesis Working Group
#containing 20 bands with values from 1-16. It then simplifies the stack
#to ignore 4-5 threshold hotter droughts, as well as 'other' landfire disturbances

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse", "beepr", "future.apply", "tigris")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(here) #Relative path best practices
library(beepr) # beep() will make a noise. options(error = beep) will beep on any errors
library(terra) #New raster data package, documentation pdf here: https://cran.r-project.org/web/packages/terra/terra.pdf
  #When writing rasters, always specify data type to optimize storage & future computation: https://search.r-project.org/CRAN/refmans/raster/html/dataType.html
library(sf) #New vector data package
library(future.apply) #Apply Function to Elements in Parallel using Future
library(tigris) #Package with access to US Census Bureau shapefiles (roads, counties, etc)


## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from the beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep)

## Set output directory & create if doesn't already exist --
outDir <- here("data", "derived")

# check if sub directory exists 
if (!dir.exists(outDir)){
  dir.create(here("data", "derived"))
}

## Load data ----

disturbanceStack <- terra::rast(here("data", "disturbance_stack_southern_rockies_EPSG32613.tif"))
terra::datatype(disturbanceStack)
crsDS <- terra::crs(disturbanceStack)

### ### ### ### ### ### ### ### ### ### ### ###

# Perform stack simplification ----

## Set new codes ----
oldCodes <- seq(0, 15)
newCodes <- c(0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 3, 4, 5, 3)
# 0 - None
# 1 - Fire
# 2 - Insect/Disease
# 3 - Hot Drought
# 4 - Fire + Hot Drought
# 5 - Insect/Disease + Hot Drought

codes <- cbind(oldCodes, newCodes)


## Create test subset ----
coCounties <- tigris::counties(state = "CO") #Download counties for CO
summit <- coCounties %>% filter(NAME == "Summit") %>% sf::st_transform(crsDS) #Pull summit county
tDStack <- disturbanceStack %>% terra::crop(summit) #Crop dstack to testing subset


## Re-classify ----

#Classify to new codes
classifier <- matrix(codes, ncol=2, byrow=FALSE) #Two-column matrix can be used to classify integer values in a from -> to format

#Test set
tDStack2 <- tDStack %>% future_lapply(FUN = function(x) {terra::classify(x, classifier)}) %>% rast()

#Full stack
disturbanceStackNew <- disturbanceStack %>% future_lapply(FUN = function(x) {terra::classify(x, classifier)}) %>% rast()

#Write stack
terra::writeRaster(disturbanceStackNew,
                   here(outDir, "simple_disturbance_stack_southern_rockies_EPSG32613.tif"),
                   overwrite = TRUE,
                   datatype = "INT1U")


