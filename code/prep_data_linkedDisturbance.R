#Prep data for Linked Disturbance Project
#Tyler L. McIntosh, 2/24/2023

#This script takes in data from GEE and elsewhere and preps it
#for the linked disturbance project

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

#####Load libraries
library(terra) #New raster package
library(sf) #New vector package
library(here) #Best practice for relative paths
library(spatialEco) #spatial ecology package for calculating Heat Load Index (HLI)

#####Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 10) #Set standard decimal print output

#AOI shapefile
aoiShp <- st_read("data/aoi/EPA_lvl3_SRockies_epsg32613.shp")


#########################################################################
#######Load topography data, add heat load index, and write as single tif
##########################################################################

topoFileNames <- list.files(here("data"), pattern = "USGS_SRTM*", full.names=TRUE)
topo_SRockies <- rast(topoFileNames)

#Calculate HLI, mask, rename
sRockiesHLI <- hli(topo_SRockies$elevation) #Calculate HLI
sRockiesHLImasked <- mask(sRockiesHLI, aoiShp) #Mask to aoi
names(sRockiesHLImasked) <- "heatLoadIndex"

#Add to data & export
topo_SRockies <- c(topo_SRockies,sRockiesHLImasked) #combine
writeRaster(topo_SRockies, here("data", "topography_southern_rockies.tif"))


#####################################################
#########Load nlcd and create forest masks
######################################################

nlcdFileNames <- list.files(here("data"),
                            pattern = "NLCD*", full.names = TRUE)
nlcdCollection <- sprc(nlcdFileNames) #Create as SpatRasterCollection since they aren't of the same area
nlcd <- mosaic(nlcdCollection) #Mosaic the SpatRasterCollection
writeRaster(nlcd, here("data", "nlcd_southern_rockies.tif"))


###Create simple modal forest type dataset & forest mask

#Create classifier - 41, 42, and 43 are the classes of interest (forest)
m <- c(0, 40, NA,
       44, 100, NA) #change non-forested classes to NA
forestClassifier <- matrix(m, ncol=3, byrow=TRUE)

#Run classifier over all years of NLCD
forestType <- nlcd %>% lapply(function(yr) {
  classed <- classify(yr,
                      forestClassifier,
                      right = NA)
  print("layer done")
  return(classed)
})

#Rename & turn back to spatrast
names(forestType) <- names(nlcd)
forestType <- terra::rast(forestType)

#Compute modal forest type raster
#na.rm=TRUE to not include NAs. (i.e. include anything that was a forest in any layer)
forestType <- terra::modal(forestType, na.rm=TRUE)
plot(forestType)

#Classify to forest/non-forest mask
m <- c(41, 43, 1) #forest -> binary forest/non-forest
forestClassifier2 <- matrix(m, ncol=3, byrow=TRUE)
forestNonForest <- forestType %>% classify(forestClassifier2, right = NA)

plot(forestNonForest)

#Write modal forest type and forest mask
writeRaster(forestType, here("data", "modal_forest_type.tif"))
writeRaster(forestNonForest, here("data", "forest_mask.tif"))
