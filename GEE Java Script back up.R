// Area of Interest
Map.addLayer(roi); // view TMNR on the map

// ----- Landsat 7 collection -----
  // Cloud masking function
// var cloudMaskL457 = function(image) {
  // var qa = image.select('QA_PIXEL');
  // var cloud = qa.bitwiseAnd(1 << 5)
  //             .and(qa.bitwiseAnd(1 << 7))
  //             .or(qa.bitwiseAnd(1 << 3));
  // var mask2 = image.mask().reduce(ee.Reducer.min());
  // return image.updateMask(cloud.not()).updateMask(mask2);
  // };

// Exporting landsat 7 imagery
var L7 =ee.ImageCollection("LANDSAT/LE07/C02/T1_L2") // USGS Landsat 7 Level 2, Collection 2, Tier 1 
.filterDate('2002-01-01', '2013-12-31')
// .map(cloudMaskL457)
.filterBounds(roi)
.filterMetadata('WRS_ROW', 'equals', 84)
.filterMetadata('WRS_PATH', 'equals', 175)
.filterMetadata('CLOUD_COVER_LAND', 'less_than', 25);

//Applies scaling factors.
function L7applyScaleFactors(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBand = image.select('ST_B6').multiply(0.00341802).add(149.0);
  return image.addBands(opticalBands, null, true)
  .addBands(thermalBand, null, true);
}

L7 = L7.map(L7applyScaleFactors);
print('L7 Image Collection', L7)

// Convert the image collection to a list
var L7collectionList = L7.toList(L7.size());

// Select a specific image from the list by its index
var L7index = 1
var L7selectedImage = ee.Image(L7collectionList.get(L7index)); // Replace 'index' with the desired index
print('L7 Selected Imagery', L7selectedImage)

//Inspect some imagery
var L7visualisation = {
  bands: ['SR_B3', 'SR_B2', 'SR_B1'], // Natural colour composite
  min: 0.0,
  max: 0.3,
};
Map.addLayer(L7selectedImage, L7visualisation, 'Landsat 7: whole designated area',false)
Map.addLayer(L7selectedImage.clip(roi), L7visualisation, 'Landsat 7: TMNR',false)

///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ----- Landsat 8 collection -----
  
  // Exporting landsat 8 imagery
var L8= ee.ImageCollection("LANDSAT/LC08/C02/T1_L2") //USGS Landsat 8 Level 2, Collection 2, Tier 1 
.filterDate('2013-01-01', '2021-12-31')
.filterBounds(roi)
.filterMetadata('WRS_ROW', 'equals', 84)
.filterMetadata('WRS_PATH', 'equals', 175)
.filterMetadata('CLOUD_COVER_LAND', 'less_than', 25);

// Applies scaling factors.
function L8applyScaleFactors(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0);
  return image.addBands(opticalBands, null, true)
  .addBands(thermalBands, null, true);
}

L8 = L8.map(L8applyScaleFactors);
print('L8 Image Collection', L8)

// Convert the image collection to a list
var L8collectionList = L8.toList(L8.size());

// Select a specific image from the list by its index
var L8index = 60

var L8selectedImage = ee.Image(L8collectionList.get(L8index)); // Replace 'index' with the desired index
print('L8 Selected Imagery', L8selectedImage)

//Inspect some imagery
var L8visualisation = {
  bands: ['SR_B4', 'SR_B3', 'SR_B2'], // Natural colour composite
  min: 0.0,
  max: 0.3,
};
Map.addLayer(L8selectedImage, L8visualisation, 'Landsat 8: whole designated area',false)
Map.addLayer(L8selectedImage.clip(roi), L8visualisation, 'Landsat 8: TMNR',false)

///////////////////////////////////////////////////////////////////////////////////////////////////////////
  // ----- Landsat 9 collection -----
  
  // Exporting landsat 9 imagery
var L9= ee.ImageCollection("LANDSAT/LC09/C02/T1_L2") //USGS Landsat 9 Level 2, Collection 2, Tier 1 
.filterDate('2021-01-01', '2023-12-31')
.filterBounds(roi)
.filterMetadata('WRS_ROW', 'equals', 84)
.filterMetadata('WRS_PATH', 'equals', 175)
.filterMetadata('CLOUD_COVER_LAND', 'less_than', 25);

// Applies scaling factors.
function L9applyScaleFactors(image) {
  var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
  var thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0);
  return image.addBands(opticalBands, null, true)
  .addBands(thermalBands, null, true);
}

L9 = L9.map(L9applyScaleFactors);
print('L9 Image Collection', L9)

// Convert the image collection to a list
var L9collectionList = L9.toList(L9.size());

// Select a specific image from the list by its index
var L9index = 12
var L9selectedImage = ee.Image(L9collectionList.get(L9index)); // Replace 'index' with the desired index
print('L9 Selected Imagery', L9selectedImage)

//Inspect some imagery
var L9visualisation = {
  bands: ['SR_B4', 'SR_B3', 'SR_B2'], // Natural colour composite
  min: 0.0,
  max: 0.3,
};
Map.addLayer(L9selectedImage, L9visualisation, 'Landsat 9: whole designated area',false)
Map.addLayer(L9selectedImage.clip(roi), L9visualisation, 'Landsat 9: TMNR',false)

// IMAGERY EXTRACTION
var L7id = L7.aggregate_array('system:index');
var L8id = L8.aggregate_array('system:index');
var L9id = L9.aggregate_array('system:index');

// Landsat 7 collection extraction

// L7id.evaluate(function(list){
  //   list.map(function(id){
    //     var image = L7.filter(ee.Filter.eq('system:index', id)).first();
    //     Export.image.toDrive({
      //       image: image.toFloat(), //export as float
      //       scale: 30, // spatial resolution
      //       region: roi,
      //       crs: 'EPSG:32734', // WGS84 UTM 34S-EPSG:32734
      //       //maxPixels: 1e13,
      //       folder: 'GEE Landsat Collection',
      //       description: id,
      //       formatOptions: {
        //         cloudOptimized: true
        //       }
      //     });
    //   });
  // });

// // Landsat 8 collection extraction

// L8id.evaluate(function(list){
  //   list.map(function(id){
    //     var image = L8.filter(ee.Filter.eq('system:index', id)).first();
    //     Export.image.toDrive({
      //       image: image.toFloat(), //export as float
      //       scale: 30, // spatial resolution
      //       region: roi,
      //       crs: 'EPSG:32734', // WGS84 UTM 34S-EPSG:32734
      //       //maxPixels: 1e13,
      //       folder: 'GEE Landsat Collection',
      //       description: id,
      //       formatOptions: {
        //         cloudOptimized: true
        //       }
      //     });
    //   });
  // });

// Landsat 9 collection extraction

// L9id.evaluate(function(list){
  //   list.map(function(id){
    //     var image = L9.filter(ee.Filter.eq('system:index', id)).first();
    //     Export.image.toDrive({
      //       image: image.toFloat(), //export as float
      //       scale: 30, // spatial resolution
      //       region: roi,
      //       crs: 'EPSG:32734', // WGS84 UTM 34S-EPSG:32734
      //       //maxPixels: 1e13,
      //       folder: 'GEE Landsat Collection',
      //       description: id,
      //       formatOptions: {
        //         cloudOptimized: true
        //       }
      //     });
    //   });
  // });


// Plotting cloud score for each Landsat collection separately
// var getCloudScores = function(img){
  //   //Get cloud cover
  //   var value = ee.Image(img).get('CLOUD_COVER_LAND');
  //   return ee.Feature(null,{'score': value})
  // };

// var L7_LCC_values = L7.map(getCloudScores);
// print(ui.Chart.feature.byFeature(L7_LCC_values).setChartType('ScatterChart'));

// var L8_LCC_values = L8.map(getCloudScores);
// print(ui.Chart.feature.byFeature(L8_LCC_values).setChartType('ScatterChart'));

// var L9_LCC_values = L9.map(getCloudScores);
// print(ui.Chart.feature.byFeature(L9_LCC_values).setChartType('ScatterChart'));




/**
  * Function to mask clouds using the Sentinel-2 QA band
* @param {ee.Image} image Sentinel-2 image
* @return {ee.Image} cloud masked Sentinel-2 image
*/
  
  // function maskS2clouds(image) {
    //   var qa = image.select('QA60');
    
    //   // Bits 10 and 11 are clouds and cirrus, respectively.
    //   var cloudBitMask = 1 << 10;
    //   var cirrusBitMask = 1 << 11;
    
    //   // Both flags should be set to zero, indicating clear conditions.
    //   var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
    //       .and(qa.bitwiseAnd(cirrusBitMask).eq(0));
    
    //   return image.updateMask(mask).divide(10000);
    // }

var S2_TOA =ee.ImageCollection("COPERNICUS/S2_HARMONIZED") 
.filterDate('2015-01-01', '2017-03-28')
.filterBounds(roi)
.filterMetadata('MGRS_TILE', 'equals', '34HBH')
.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 5))
//.map(maskS2clouds);

print('S2_TOA', S2_TOA)

// Convert the image collection to a list
var SR_TOA_collectionList = S2_TOA.toList(S2_TOA.size());

// Select a specific image from the list by its index
var S2_TOA_index = 15
var S2_TOA_selectedImage = ee.Image(SR_TOA_collectionList.get(S2_TOA_index)); // Replace 'index' with the desired index

print('S2_TOA Selected Imagery', S2_TOA_selectedImage)

//Inspect some imagery
var SR_TOA_visualisation = {
  bands: ['B4', 'B3', 'B2'], // Natural colour composite
  min: 0.0,
  max: 0.3,
};

Map.addLayer(S2_TOA_selectedImage.divide(10000), SR_TOA_visualisation, 'SR TOA: whole designated area',false)
Map.addLayer(S2_TOA_selectedImage.clip(roi).divide(10000), SR_TOA_visualisation, 'SR TOA: TMNR',true)


var S2TOAid = S2_TOA.aggregate_array('system:index');

// Sentinel 2 TOA collection extraction

S2TOAid.evaluate(function(list){
  list.map(function(id){
    var image = S2_TOA.filter(ee.Filter.eq('system:index', id)).first();
    Export.image.toDrive({
      image: image.divide(10000), //export as float
      scale: 30, // spatial resolution
      region: roi,
      crs: 'EPSG:32734', // WGS84 UTM 34S-EPSG:32734
      maxPixels: 1e13,
      folder: 'GEE Landsat Collection',
      description: id,
      formatOptions: {
        cloudOptimized: true
      }
    });
  });
});










