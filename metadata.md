# Metadata for the Southern Rockies Linked Disturbance GEDI Dataset 
This dataset includes GEDI points from 2019-2021 that are over forested areas of the Southern Rockies. 
Data points within a buffer of Census Bureau road polylines have been removed. 
Data points that had a fire or insect disturbance in the year of GEDI data collection have been removed. 
Disturbance data is from the disturbance stack derived by CU Boulder's CIRES Earth Lab in 2023. 

## Information 
Author: Tyler L. McIntosh 
Date generated:  2023-04-25 21:26:27 MDT 
[GitHub repo with code for reproduction](https://github.com/tylerhoecker/linked_disturbance)

## Metadata 
columns :: description

date :: GEDI collection date
beam :: GEDI beam
shot_numbe :: GEDI shot number
degrade_fl :: unknown
l4_quality :: unknwon
l2_quality :: unknown
algorithm_ :: unknown
sensitivit :: sensitivity?
lat_lowest :: Latitude in EPSG4326
lon_lowest :: Longitude in EPSG4326
elev_lowes :: Elevation
tree_cover :: tree cover?
pft_class :: Plant functional type class
agbd_se :: Aboveground Biomass from GEDI 4A product standard error?
agbd :: Aboveground Biomass from GEDI 4A product
roadBuffer :: Within road buffer? 1 = yes, NA = no
utm_z13n_easting :: UTM Z 13N Northing
utm_z13n_northing :: UTM Z 13N Easting
gediYear :: Year of GEDI data collection
modisNonVeg :: Modis non vegetated % cover in year of GEDI data collection, NA indicates data unavailable
modisNonTreeVeg :: Modis non-tree vegetated % cover in year of GEDI data collection, NA indicates data unavailable
modisTreeVeg :: Modis tree % cover in year of GEDI data collection, NA indicates data unavailable
modisNonVeg2000 :: Modis non vegetated % cover in 2000, NA indicates data unavailable
modisNonTreeVeg2000 :: Modis non-tree vegetated % cover in 2000, NA indicates data unavailable
modisTreeVeg2000 :: Modis tree % cover in 2000, NA indicates data unavailable
peakNDVI :: Peak NDVI in year of GEDI data collection, derived from Landsat
aspect :: Topographic aspect derived from USGS SRTM 30m DEM
slope :: Topographic slope derived from USGS SRTM 30m DEM
elevation :: Topographic elevation from USGS SRTM 30m DEM
heatLoadIndex :: McCune & Keon Heat Load Index (HLI) from USGS SRTM 30m DEM
chili :: CHILI heat load index
mtpi :: MTPI
forestMask :: Forest mask, binary: pixels with any type of forested cover at any point in NLCD dataset
forestCode :: Forest code: NLCD codes of modal forest cover for pixel
forestType :: Forest type: NLCD human-readable forest cover type associated with code
hex_id_100000 :: hex id 100000m size
hex_id_50000 :: hex id 50000m size
hex_id_10000 :: hex id 10000m size
hex_id_5000 :: hex id 5000m size
US_L4CODE :: EPA level 4 code
US_L4NAME :: EPA level 4 name
adm_code :: Surface Management Agency code
aetNorm :: 30-yr average of AET from TopoFire, Holden et al.
defNorm :: 30-yr average of CWD from TopoFire, Holden et al.
spei... :: Standardized Precipitaion Evapotranspiration Index (SPEI) with climatic water balance aggregated over the year prior to a given date. Averaged for each calendar year and provided for each year X years prior to GEDI data collection. NA indicates out of data timeframe
speiCum... :: Cumulative SPEI over the time period specified
yrsSince... :: Number of years since an event of the given disturbance type, exclusive of GEDI collection year. NA indicates disturbance was never present in available data
collectionYr... :: Presence of given disturbance in year of GEDI collection, binary
...YrsInPriorX :: Number of years of given disturbance in the X years prior to GEDI data collection. NA indicates that X is beyond the time frame of the available data
...combo... :: 'Combo' indicates a combined disturbance of either hotterDrought & insects or hotterDrought & fire occurring in the same year
