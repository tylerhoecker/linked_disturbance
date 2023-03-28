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
list.of.packages <- c("terra","sf","here","tidyverse", "beepr", "future.apply")
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


## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from the beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep)

## Load data ----

disturbanceStack <- terra::rast("data/disturbance_stack_southern_rockies_EPSG32613_int.tif")
datatype(disturbanceStack)

### ### ### ### ### ### ### ### ### ### ### ###



