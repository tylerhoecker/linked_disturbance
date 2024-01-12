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
library(spatialEco) #spatial ecology package for calculating McCune & Keon Heat Load Index (HLI)
library(future.apply) #parallelize apply functions
library(tidyverse)
library(ecoregions)


#####Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 10) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)

#AOI shapefile
aoiShp <- st_read("data/polygons/aoi/EPA_lvl3_SRockies_epsg32613.shp")

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


################################################
########## Load NED 10m CHILI & mosaic
###############################################

chiliFileNames <- list.files(rawData,
                            pattern = "US_NED_10m_chili*", full.names = TRUE)
chiliCollection <- terra::sprc(chiliFileNames) #Create as SpatRasterCollection since they aren't of the same area
chili <- terra::mosaic(chiliCollection) #Mosaic the SpatRasterCollection
datatype(chili)
terra::writeRaster(chili,
                   here(outDir, "US_NED_10m_chili_southern_rockies.tif"),
                   overwrite = TRUE,
                   datatype = 'FLT4S')

#####################################################
#########Load nlcd and create forest masks
######################################################

nlcdFileNames <- list.files(rawData,
                            pattern = "NLCD*", full.names = TRUE)
nlcdCollection <- terra::sprc(nlcdFileNames) #Create as SpatRasterCollection since they aren't of the same area
nlcd <- terra::mosaic(nlcdCollection) #Mosaic the SpatRasterCollection
terra::writeRaster(nlcd,
            here(outDir, "nlcd_southern_rockies.tif"),
            overwrite = TRUE,
            datatype = 'INT1U')
#nlcd <- rast(here(outDir, "nlcd_southern_rockies.tif"))

###Create simple modal forest type dataset & forest mask

#Create classifier - 41, 42, and 43 are the classes of interest (forest)
#41: deciduous
#42: evergreen
#43: mixed forest
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

######################################################
######### Create development buffer raster from NLCD
######################################################

# bufferDist <- 300
# 
# #developed NLCD classes: c(21, 22, 23, 24)
# m <- c(0, 20, NA,
#        21, 24, 1,
#        25, 100, NA) #change non-developed to zero
# devClassifier <- matrix(m, ncol=3, byrow=TRUE)
# 
# developed <- nlcd$`2019_landcover` %>%
#   terra::classify(devClassifier,
#                   right = NA)
# 
# devPoly <- developed %>% terra::as.polygons(trunc = TRUE,
#                                      dissolve = TRUE,
#                                      values = TRUE)
# 
# devBuff <- devPoly %>%
#   sf::st_as_sf() %>%
#   sf::st_buffer(bufferDist)
# 
# st_write(devBuff, here::here(outDir, "development_buffer.geojson"))



# # A function to extract a class (or set of classes) from a vector, polygonize, buffer,
# # and return a binary raster of buffer presence
# buffer.cat.raster.class <- function(raster, classVect, bufferDist) {
#   
# }
# 
# bufferDist <- 300
# 
# #developed NLCD classes: c(21, 22, 23, 24) 
# m <- c(0, 20, NA,
#        21, 24, 1,
#        25, 100, NA) #change non-developed to zero
# devClassifier <- matrix(m, ncol=3, byrow=TRUE)
# 
# developed <- nlcd$`2019_landcover` %>%
#   terra::classify(devClassifier,
#                   right = NA)
# 
# devPoly <- developed %>% terra::as.polygons(trunc = TRUE,
#                                      dissolve = TRUE,
#                                      values = TRUE)
# 
# devBuff <- devPoly %>%
#   sf::st_as_sf() %>%
#   sf::st_buffer(bufferDist)
# 
# #rasterize
# #create empty raster
# res <- 30
# ext <- ext(developed)
# r <- rast(ext, res)
# 
# #transfer poly values to raster
# devRast <- rasterize(devBuff, r, field = "roadBuffer", fun = "min")
# 
# #write
# terra::writeRaster(lineRast,
#                    here(outDir, "dev_buffer_raster.tif"),
#                    overwrite = TRUE,
#                    datatype = "INT1U")


######################################################
######### Create road buffer raster
######################################################

#buffer justification:
#lanes are 3.7m each. 4 lanes = 15m, /2 = 7.5. +  13m = ~ 1/2 of GEDI 25m diameter.
# = 21m buffer on each side of road line
# Increase to 30m to be safe
buffSize <- 30

workingCRS <- "PROJCRS[\"WGS 84 / UTM zone 13N\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"UTM zone 13N\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-105,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"Between 108°W and 102°W, northern hemisphere between equator and 84°N, onshore and offshore. Canada - Northwest Territories (NWT); Nunavut; Saskatchewan. Mexico. United States (USA).\"],\n        BBOX[0,-108,84,-102]],\n    ID[\"EPSG\",32613]]"

#Roads
lineDats <- sf::st_read(here(rawData, "Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE.shp")) %>%
  sf::st_transform(workingCRS)

lineDats <- lineDats %>% mutate(roadBuffer = 1)

#Buffer
lineBuff <- lineDats %>%
  sf::st_filter(gediBB) %>%
  sf::st_buffer(buffSize)

#rasterize

#create empty raster
res <- 30
rawBB <- sf::st_bbox(lineDats)
ext <- ext(rawBB[1]-res, rawBB[3]+res, rawBB[2]-res, rawBB[4]+res)
r <- rast(ext, res)

#transfer poly values to raster
lineRast <- rasterize(lineBuff, r, field = "roadBuffer", fun = "min")

#write
terra::writeRaster(lineRast,
                   here(outDir, "road_buffer_raster.tif"),
                   overwrite = TRUE,
                   datatype = "INT1U")


#####################################
###### Generate grids
####################################

#The below "make_grid" function is from: 
#https://strimas.com/post/hexagonal-grids/

#Creates a hexagonal grid over a polygon of a given cell size
make.hex.grid <- function(aoi, cellsize) {
  increasedAoi <- aoi %>% sf::st_buffer(cellsize)
  varnm <- paste("hex_id_", cellsize, sep="")
  hexGrid <- increasedAoi %>%
    sf::st_make_grid(cellsize = cellsize, square = FALSE) %>%
    sf::st_as_sf() %>%
    mutate(!!varnm := seq(1:nrow(.)))
  return(hexGrid)
}

hex100000 <- aoiShp %>% make.hex.grid(100000)
hex50000 <- aoiShp %>% make.hex.grid(50000)
hex10000 <- aoiShp %>% make.hex.grid(10000)
hex5000 <- aoiShp %>% make.hex.grid(5000)

st_write(hex100000, here::here(outDir, "hex100000_sr.geojson"))
st_write(hex50000, here::here(outDir, "hex50000_sr.geojson"))
st_write(hex10000, here::here(outDir, "hex10000_sr.geojson"))
st_write(hex5000, here::here(outDir, "hex5000_sr.geojson"))




