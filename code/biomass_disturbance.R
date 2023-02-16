library(tidyverse)
library(terra)
library(sf)

# Data directory relative to the location of the script
data_dir = file.path('..','data_forest_resiliency','data_sandbox')

# Load the EarthLab disturbance stack for S. Rockies ecoregion (SR)
# Multi-band raster - each band is a year - categories are disturbance history
dstack = rast(file.path(data_dir,'disturbance_stack_southern_rockies_EPSG32613.tif'))
plot(dstack[[10]])

# Load GEDI biomass points for SR
gedi_4a = read_sf()