////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////// SETUP //////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

//Download landsat data
//Tyler L. McIntosh, CU Boulder Earth Lab, 2/24/2022

///////Explanation of analysis
//When script is run, prepared downloads of landsat data will be populated in the 'tasks' panel.
//Clouds and cloud shadows are masked prior to composites & mosaics
//Snow/ice and water are masked prior to composites & mosaics

//Select the variables of interest by clicking 'Run'
//Before running, change user-set parameters below


///////Naming conventions wherever possible:
//lowerCamelCase for variables
//underscore_separated for files & functions

///////Data inputs & outputs
// INPUTS:
// an aoi chosen from epa_lvl3 (EPA Ecoregions at the level 3 category)
// OUTPUTS:
// .tif files of standard data exported for further analysis
var epa_lvl3 = ee.FeatureCollection("users/tymc5571/EPA_EcoLvl3"),
    landsatSurfaceRef = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2");
    
////////////////////////////////// USER-SET PARAMETERS /////////////////////////////////////

//Google Drive folder for outputs
var folderName = 'GEE_Exports';

//Set output projection
var projection = 'EPSG:26913';

//'EPSG:26912' = NAD83 UTM Zone 12N (Middle Rockies)
//'EPSG:26913' = NAD83 UTM Zone 13N (Southern Rockies)
//'EPSG:32613' = WSG84 UTM Zone 13N (Southern Rockies)

//Select timeframe of interest
var startDate = '2020-05-01';
var endDate = '2020-09-30';

//Select EPA region of interest
//print('EPA Level 3 Ecoregions:', epa_lvl3);
var aoiRegion = '6.2.14'; //CHOOSE EPA NA_L3CODE OF INTEREST
var regionReadable = 'SouthernRockies'; //Human-readable region name for output files
// '6.2.14' = 'SouthernRockies'
// '6.2.10' = 'MiddleRockies'



////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////// Analysis /////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////

///////////// PREP ///////////////

//Filter to selected AOI and create a raster mask
var aoi = epa_lvl3.filter(ee.Filter.eq('NA_L3CODE', aoiRegion));
print('AOI:', aoi);
var aoiRast = aoi.reduceToImage({ //Reduce to raster for masking
  properties: ['Shape_Area'], //Reducing by shape area since we don't care what the number is, just need any number
  reducer: ee.Reducer.first() //There shouldn't be any overlap between features, so this is sufficient
}).remap({ //Then remap any value to 1 via default value (have to have something in 'from' and 'to' to avoid error)
  from: [1],
  to: [1],
  defaultValue: 1
});
//Map.addLayer(aoiRast);
var mask = aoiRast.eq(1);


/////////////////////GET LANDSAT///////////////////

////// GREENEST PIXEL COMPOSITE //////////

//Filter collection to time and spatial frame of interest
var landsatFilt = landsatSurfaceRef.filterDate(startDate, endDate)
  .filterBounds(aoi);

//Function to mask clouds & cloud shadows on a per-image basis
//Good intro to bitmasks (for python, but useful: https://medium.com/analytics-vidhya/python-for-geosciences-raster-bit-masks-explained-step-by-step-8620ed27141e)
//function addapted from GEE workflow example: https://calekochenour.github.io/remote-sensing-textbook/03-beginner/chapter12-cloud-masking.html
var cloudAndShadowMask = function(im) {
  //Bit 4 = Cloud Shadow
  //Bit 6 = Clear (0: Cloud or Dilated Cloud bits are set. 1: Cloud and Dilated Cloud bits are not set)
  var shadowBit = (1 << 4); //base 2
  var clearBit = (1 << 6); //base 2
  
  var qa = im.select('QA_PIXEL');
  
  //Flag in shadow mask should be 0 (false), flag in clear mask should be 1 (true, i.e. not 0)
  var shadowMask = qa.bitwiseAnd(shadowBit).eq(0);
  var clearMask = qa.bitwiseAnd(clearBit).eq(0).not();
  
  //Mask & return
  var masked = im.updateMask(shadowMask).updateMask(clearMask);
  return (masked);
};

//Function to mask snow/ice & water (cause problems with using NDVI for greenest)
var snowWaterMask = function(im) {
  var snowBit = (1 << 5); //base 2
  var waterBit = (1 << 7); //base 2
  
  var qa = im.select('QA_PIXEL');
  
  //Flag in shadow mask should be 0 (false), flag in clear mask should be 1 (true, i.e. not 0)
  var snowMask = qa.bitwiseAnd(snowBit).eq(0);
  var waterMask = qa.bitwiseAnd(waterBit).eq(0);
  
  //Mask & return
  var masked = im.updateMask(snowMask).updateMask(waterMask);
  return (masked);
};

//Run function
var landsatFiltClean = landsatFilt.map(cloudAndShadowMask).map(snowWaterMask);
  
//Function to add NDVI to collection images
var addNDVI = function(im) {
  var ndvi = im.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDVI');
  return im.addBands(ndvi);
};

//Collection w/ ndvi band added
var landsatFiltNDVI = landsatFiltClean.map(addNDVI);

//compute max greenness mosaic based on NDVI band
//Note that this will put clouds over all water bodies! Cloud NDVI > water NDVI
var greenest = landsatFiltNDVI.qualityMosaic('NDVI').clip(aoi); 

//View
print('greenest', greenest);
Map.addLayer(greenest,{bands:['SR_B4','SR_B3','SR_B2'],min:300,max:30000},
  'greenest');
  
var greenestBands = greenest.select('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7');

////// Median composite //////
var medianComposite = landsatFiltNDVI.median().clip(aoi);

//View
print('median composite', medianComposite);
Map.addLayer(medianComposite,{bands:['SR_B4','SR_B3','SR_B2'],min:300,max:30000},
  'median composite');

var medianBands = medianComposite.select('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7');

///////////////////ADD HUMAN READABLE TO FILE NAMES ////////////////////

///////////////////// EXPORT DATA ////////////////////
var projectionReadable = ee.String(projection).slice(0,4).cat('_').cat(ee.String(projection).slice(5));
var timeframeReadable = ee.String(startDate).cat('_').cat(endDate);
var appendixName = ee.String('_').cat(regionReadable).cat('_').cat(timeframeReadable).cat('_').cat(projectionReadable).cat('_GEE');

//Export the data to google drive with human readable names
Export.image.toDrive({
  image: greenestBands,
  description: ee.String('greenestLandsatBands').cat(appendixName).getInfo(),
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: medianBands,
  description: ee.String('medianCompositeLandsatBands').cat(appendixName).getInfo(),
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});
Export.image.toDrive({
  image: greenest.select('NDVI'),
  description: ee.String('greenestNDVI_LandsatDerived').cat(appendixName).getInfo(),
  crs: projection,
  scale: 30,
  region: aoi,
  folder: folderName,
  maxPixels: 1e10
});