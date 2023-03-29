#Prep data for Linked Disturbance Project
#Tyler L. McIntosh, 2/24/2023

#This script takes in data from GEE and elsewhere and preps it
#for the linked disturbance project

#Prior script: wrangle_data_GEE.js

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

#####Load libraries
library(terra) #New raster package
library(sf) #New vector package
library(here) #Best practice for relative paths
library(spatialEco) #spatial ecology package for calculating Heat Load Index (HLI)
library(future.apply) #parallelize apply functions

#####Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 10) #Set standard decimal print output

#AOI shapefile
aoiShp <- st_read("data/aoi/EPA_lvl3_SRockies_epsg32613.shp")

#Set output directory & create if doesn't already exist
outDir <- here("data", "derived")

# check if sub directory exists 
if (!dir.exists(outDir)){
  dir.create(here("data", "derived"))
}

#Also set data directories
sharedData <- here("data", "shared")
rawData <- here("data", "raw_GEE")

#########################################################################
#######Load topography data, add heat load index, and write as single tif
##########################################################################

topoFileNames <- list.files(rawData, pattern = "USGS_SRTM*", full.names=TRUE)
topo_SRockies <- terra::rast(topoFileNames)

#Calculate HLI, mask, rename
sRockiesHLI <- spatialEco::hli(topo_SRockies$elevation) #Calculate HLI
sRockiesHLImasked <- terra::mask(sRockiesHLI, aoiShp) #Mask to aoi
names(sRockiesHLImasked) <- "heatLoadIndex"

#Add to data & export
topo_SRockies <- terra::c(topo_SRockies,sRockiesHLImasked) #combine
terra::writeRaster(topo_SRockies,
            here(outDir, "topography_southern_rockies.tif"),
            overwrite = TRUE,
            datatype = 'FLT4S') #ALWAYS specify datatype when writing a raster to file. Default is FLT4S

#########################################################################
#######Load climate normals data, write as single tif
##########################################################################

aet <- terra::rast(here(sharedData, "Climate_Normals-20230317T210403Z-001/Climate_Normals/aet_1981_2010_mean_clip_SRockies.tif"))
def <- terra::rast(here(sharedData, "Climate_Normals-20230317T210403Z-001/Climate_Normals/def_1981_2010_mean_clip_SRockies.tif"))
climateNormals <- c(aet,def) %>%
  terra::project(crs(topo_SRockies))

terra::writeRaster(climateNormals,
            here(outDir, "climate_normals_aet_def.tif"),
            overwrite = TRUE,
            datatype = 'FLT4S')


#####################################################
#########Load nlcd and create forest masks
######################################################

nlcdFileNames <- list.files(rawData,
                            pattern = "NLCD*", full.names = TRUE)
nlcdCollection <- sprc(nlcdFileNames) #Create as SpatRasterCollection since they aren't of the same area
nlcd <- terra::mosaic(nlcdCollection) #Mosaic the SpatRasterCollection
terra::writeRaster(nlcd,
            here(outDir, "nlcd_southern_rockies.tif"),
            overwrite = TRUE,
            datatype = 'INT1U')

###Create simple modal forest type dataset & forest mask

#Create classifier - 41, 42, and 43 are the classes of interest (forest)
m <- c(0, 40, NA,
       44, 100, NA) #change non-forested classes to 0
forestClassifier <- matrix(m, ncol=3, byrow=TRUE)

#Run classifier over all years of NLCD
forestType <- nlcd %>% future_lapply(function(yr) {
  classed <- terra::classify(yr,
                      forestClassifier,
                      right = NA)
  return(classed)
})

#Rename & turn back to spatrast
names(forestType) <- names(nlcd)
forestType <- terra::rast(forestType)

#Compute modal forest type raster
#na.rm=TRUE to not include NAs. (i.e. include anything that was a forest in any layer)
forestType <- terra::modal(forestType, na.rm=TRUE)

#now re-classify to turn remaining NAs into 0s
m <- matrix(c(NA, 0),
            ncol = 2, byrow=TRUE)
forestType <- forestType %>% terra::classify(m)

plot(forestType)

#Classify to forest/non-forest mask
m <- c(41, 43, 1) #forest -> binary forest/non-forest
forestClassifier2 <- matrix(m, ncol=3, byrow=TRUE)
forestNonForest <- forestType %>% terra::classify(forestClassifier2, right = NA)

plot(forestNonForest)

#Write modal forest type and forest mask
terra::writeRaster(forestType,
            here(outDir, "modal_forest_type_nlcd_srockies.tif"),
            overwrite = TRUE,
            datatype = "INT1U")
terra::writeRaster(forestNonForest,
            here(outDir, "forest_mask_nlcd_srockies.tif"),
            overwrite = TRUE,
            datatype = "INT1U")

