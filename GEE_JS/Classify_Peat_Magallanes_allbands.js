var image = ee.Image("users/italomoletto/NEXER/NASADEM_merged_UTM19S_resampled"),
    geometry = 
    /* color: #98ff00 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-79.44613305497292, -48.20626409900153],
          [-79.44613305497292, -57.66001015723314],
          [-63.15340844559792, -57.66001015723314],
          [-63.15340844559792, -48.20626409900153]]], null, false),
    imageVisParam = {"opacity":1,"bands":["classification"],"min":0,"max":5,"palette":["146200","040cff","ff710c","faffd0","94ff43","ffffff","00ff00"]},
    table = ee.FeatureCollection("users/italomoletto/NEXER/training_landcover_spectra");

var training_polygons = table
function maskS2clouds(image) {
  var qa = image.select('QA60');

  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = 1 << 10;
  var cirrusBitMask = 1 << 11;

  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
      .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

  return image.updateMask(mask).divide(10000);
}

//LOAD COLLECTIONS
var S2_mosaic = ee.ImageCollection('COPERNICUS/S2_SR').filterBounds(geometry).filter(ee.Filter.calendarRange(1,3,'month')).filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 15)).map(maskS2clouds).median().select(['B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12']).clip(geometry)
//Load Sentinel-1 SAR collection and filter according to data collection type
var S1 = ee.ImageCollection('COPERNICUS/S1_GRD')
  .filter(ee.Filter.eq('orbitProperties_pass','DESCENDING'))
  .filterBounds(geometry)
  .filterDate('2019-01-01','2019-12-31')
  .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
  //.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
// Filter speckle noise
var filterSpeckles = function(img) {
  //var vv = img.select('VV') //select the VV polarization band
  var vh = img.select('VH')
  var meters = ee.Number(100)
  //var vv_smoothed = vv.focal_median(meters,'circle','meters').rename('VV_Filtered') //Apply a focal median filter
  var vh_smoothed = vh.focal_median(meters, 'circle', 'meters').rename('VH_Filtered')
  return img.addBands(vh_smoothed) // Add filtered VV band to original image
}

// Map speckle noise filter across collection. Result is same collection, with smoothed VV band added to each image
S1 = S1.map(filterSpeckles)
//Get one image per month in Magallanes
var S1_Jan = S1.select('VH_Filtered').filterDate('2019-01-01','2019-01-30').mosaic().rename('VH_Jan')
var S1_Feb = S1.select('VH_Filtered').filterDate('2019-02-01','2019-02-28').mosaic().rename('VH_Feb')
var S1_Mar = S1.select('VH_Filtered').filterDate('2019-03-01','2019-03-30').mosaic().rename('VH_Mar')
var S1_Apr = S1.select('VH_Filtered').filterDate('2019-04-01','2019-04-30').mosaic().rename('VH_Apr')
var S1_May = S1.select('VH_Filtered').filterDate('2019-05-01','2019-05-30').mosaic().rename('VH_May')
var S1_Jun = S1.select('VH_Filtered').filterDate('2019-06-01','2019-06-30').mosaic().rename('VH_Jun')
var S1_Jul = S1.select('VH_Filtered').filterDate('2019-07-01','2019-07-30').mosaic().rename('VH_Jul')
var S1_Aug = S1.select('VH_Filtered').filterDate('2019-08-01','2019-08-30').mosaic().rename('VH_Aug')
var S1_Sep = S1.select('VH_Filtered').filterDate('2019-09-01','2019-09-30').mosaic().rename('VH_Sep')
var S1_Oct = S1.select('VH_Filtered').filterDate('2019-10-01','2019-10-30').mosaic().rename('VH_Oct')
var S1_Nov = S1.select('VH_Filtered').filterDate('2019-11-01','2019-11-30').mosaic().rename('VH_Nov')
var S1_Dec = S1.select('VH_Filtered').filterDate('2019-12-01','2019-12-30').mosaic().rename('VH_Dec')
var S1_12months = S1_Jan.addBands(S1_Feb).addBands(S1_Mar).addBands(S1_Apr).addBands(S1_May).addBands(S1_Jun).addBands(S1_Jul).addBands(S1_Aug).addBands(S1_Sep).addBands(S1_Oct).addBands(S1_Nov).addBands(S1_Dec)

var dem = ee.Image("users/italomoletto/NEXER/NASADEM_merged_UTM19S_resampled").rename('dem');
var slope = ee.Terrain.slope(image)
var aspect = ee.Terrain.aspect(image)
var tpi = ee.Image("users/italomoletto/NEXER/TPI").rename('tpi');
var ndvi = S2_mosaic.normalizedDifference(['B8','B4']).rename('ndvi')
var ndwi = S2_mosaic.normalizedDifference(['B8','B11']).rename('ndwi')
var seli = S2_mosaic.normalizedDifference(['B8A','B5']).rename('seli')
var ndsi = S2_mosaic.normalizedDifference(['B3','B11']).rename('ndsi')

var facc = ee.ImageCollection('users/imerg/flow_acc_3s').mosaic()
var tan_slope = slope.multiply(3.14).divide(180).tan().reproject('EPSG:4326',null,500)
var res = ee.Number(30).pow(2)
var uca = facc.expression('(facc+1)*res',{'facc':facc,'res':res}).rename('uca')
var twi = uca.expression('(log(uca))/tan_slope',{'uca':uca.select('uca'),'tan_slope':tan_slope}).rename('twi')


var image = S1_12months.addBands(S2_mosaic).addBands(dem).addBands(slope).addBands(aspect).addBands(tpi).addBands(twi).addBands(ndvi).addBands(ndwi).addBands(seli).addBands(ndsi)


Map.addLayer(S2_mosaic.select(['B4','B3','B2']),{min:0,max:0.2})

var classifier = ee.Classifier.smileRandomForest(500)
var classifier_svm = ee.Classifier.svm('voting', "C_SVC","RBF",true,null,0.000001,null,64)
var classifier_cart = ee.Classifier.smileCart(null, 1)

print(S2_mosaic)
//print(training)
// Train the classifier.
//var bands_mixed = ['B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12']
var bands_mixed = ['VH_Jan','VH_Feb','VH_Mar','VH_Apr','VH_May','VH_Jun','VH_Jul','VH_Aug','VH_Sep','VH_Oct','VH_Nov','VH_Dec','B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','dem','slope','aspect','ndvi','ndwi','seli','ndsi']

// Overlay the points on the imagery to get training.
var training = image.select(bands_mixed).sampleRegions({
  collection: training_polygons,
  properties: ['landcover'],
  scale: 10
});
// Filter out the null property values and try again.
var trainingNoNulls = training.filter(
  ee.Filter.notNull(training.first().propertyNames())
);

var trained2 = classifier.train(trainingNoNulls, 'landcover', bands_mixed);
var trained_svm = classifier_svm.train(trainingNoNulls, 'landcover', bands_mixed);
var trained_cart = classifier_cart.train(trainingNoNulls, 'landcover', bands_mixed);

//print(trained2)
// Classify the image.
var classified = image.classify(trained2);
var classified_svm = image.classify(trained_svm);
var classified_cart = image.classify(trained_cart);
print(classified)
Map.addLayer(classified,imageVisParam);
Map.addLayer(classified_svm,imageVisParam);
Map.addLayer(classified_cart,imageVisParam);

//Map.addLayer(image)
Map.addLayer(table)


Export.image.toDrive({
  image:classified,
  description:'RF_PeatBog_S2S1DEM',
  scale: 10,
  region: geometry,
  folder:'PeatBog_Magallanes',
  maxPixels:1e12
});

Export.image.toDrive({
  image:classified_cart,
  description:'CART_PeatBog_S2S1DEM',
  scale: 10,
  region: geometry,
  folder:'PeatBog_Magallanes',
  maxPixels:1e12
});

var extract_values = image.reproject('EPSG:4326',null,10).reduceRegions({collection:training_polygons,reducer:ee.Reducer.first()})
//print(extract_values)

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: extract_values,
  description:'lc_data',
  folder:'PeatBog_Magallanes',
  fileFormat: 'SHP'
});
