### ### ### ### ### ### ### ### ### ### ### ###

#Create a buffer raster from a set of lines (e.g. roads)
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#Last updated: 4/19/23


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

parallel = TRUE #Change to "True" to enable parallel computing at geographic difference choke point

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

## SET BUFFER SIZE ----

#buffer justification:
#lanes are 3.7m each. 4 lanes = 15m, /2 = 7.5. +  13m = ~ 1/2 of GEDI 25m diameter.
# = 21m buffer on each side of road line
# Increase to 30m to be safe
buffSize <- 30

## Load data ----

workingCRS <- "PROJCRS[\"WGS 84 / UTM zone 13N\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"UTM zone 13N\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-105,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"Between 108°W and 102°W, northern hemisphere between equator and 84°N, onshore and offshore. Canada - Northwest Territories (NWT); Nunavut; Saskatchewan. Mexico. United States (USA).\"],\n        BBOX[0,-108,84,-102]],\n    ID[\"EPSG\",32613]]"

#Roads
lineDats <- sf::st_read(here(rawData, "Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE.shp")) %>%
  sf::st_transform(workingCRS)

#GEDI
gedi_og <- sf::st_read(here(sharedData, "GEDI_4A_Hayman.geojson")) %>% sf::st_transform(workingCRS)
gedi <- sf::st_read(here(sharedData, "gedi_sgye", "gedi_sgye", "gedi_sgye.shp")) %>% sf::st_transform(workingCRS)





# SCRIPTED ANALYSIS ----

# Manipulate GEDI data ----
## Get only points NOT within road buffer ----
gediBB <- sf::st_bbox(gedi) %>%
  sf::st_as_sfc()

#Add binary column to line dats
lineDats <- lineDats %>% mutate(buffer = 1)

#Perform line buffer
tic("buffer")
lineBuff <- lineDats %>%
  sf::st_filter(gediBB) %>%
  sf::st_buffer(buffSize)
toc()


#Rasterize
res <- 30
rawBB <- sf::st_bbox(gedi)
ext <- ext(rawBB[1]-res, rawBB[3]+res, rawBB[2]-res, rawBB[4]+res)
r <- rast(ext, res)


lineRast <- rasterize(lineBuff, r, field = "buffer", fun = "min")

t <- lineRast %>%
  terra::extract(gedi) %>%
  filter(buffer == 1)
