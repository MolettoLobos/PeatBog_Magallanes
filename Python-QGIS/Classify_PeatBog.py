import ee
from ee_plugin import Map

image = ee.Image("users/italomoletto/NEXER/NASADEM_merged_UTM19S_resampled"),

geometry = ee.Geometry.Polygon(
        [[[-79.44613305497292, -48.20626409900153],
          [-79.44613305497292, -57.66001015723314],
          [-63.15340844559792, -57.66001015723314],
          [-63.15340844559792, -48.20626409900153]]])
imageVisParam = {"opacity":1,"bands":["classification"],"min":0,"max":5,"palette":["146200","040cff","ff710c","faffd0","94ff43","ffffff","00ff00"]}



#training_polygons = ee.FeatureCollection("users/italomoletto/NEXER/training_landcover_spectra_23_3000pointsmax");

training_polygons = ee.FeatureCollection("users/italomoletto/NEXER/training_landcover_spectra_24covers_withoutbushd_3000pointsmax");

#//training_polygons = ee.FeatureCollection("users/italomoletto/NEXER/training_landcover_spectra_25covers_withbushd_3000pointsmax");
#//training_polygons = ee.FeatureCollection("users/italomoletto/NEXER/training_data_20_covers_spectra");
#//training_polygons = ee.FeatureCollection("users/italomoletto/NEXER/training_data_23_covers_spectra");
#//training_polygons =  ee.FeatureCollection("users/italomoletto/NEXER/Coberturas_AM_split");


def maskS2clouds(image):
    qa = image.select('QA60');
    MSK_CLDPRB = image.select('MSK_CLDPRB')
    #  // Bits 10 and 11 are clouds and cirrus, respectively.
    cloudBitMask = 1 << 10;
    cirrusBitMask = 1 << 11;

    #  // Both flags should be set to zero, indicating clear conditions.
    mask = qa.bitwiseAnd(cloudBitMask).eq(0).And(qa.bitwiseAnd(cirrusBitMask).eq(0)).And(MSK_CLDPRB.lt(1));

    return image.updateMask(mask).divide(10000);

#//S2 COLLECTION
S2_2020_summer = ee.ImageCollection('COPERNICUS/S2_SR').filterDate('2019-12-21','2020-03-21').map(maskS2clouds).sort('system:time_start',False)
S2_2019_summer = ee.ImageCollection('COPERNICUS/S2_SR').filterDate('2018-12-21','2019-03-21').map(maskS2clouds).sort('system:time_start',False)
S2_summer = S2_2019_summer.merge(S2_2020_summer).median().select(['B1','B2','B3','B4','B5','B6','B7','B8','B9','B11','B12','B8A'])
S2_2020_autumn = ee.ImageCollection('COPERNICUS/S2_SR').filterDate('2020-03-21','2020-06-21').map(maskS2clouds).sort('system:time_start',False)
S2_2019_autumn = ee.ImageCollection('COPERNICUS/S2_SR').filterDate('2019-03-21','2019-06-21').map(maskS2clouds).sort('system:time_start',False)
S2_autumn = S2_2019_autumn.merge(S2_2020_autumn).median().select(['B1','B2','B3','B4','B5','B6','B7','B8','B9','B11','B12','B8A']).rename(['B1_a','B2_a','B3_a','B4_a','B5_a','B6_a','B7_a','B8_a','B9_a','B11_a','B12_a','B8A_a'])
#//S2_2020_winter = ee.ImageCollection('COPERNICUS/S2_SR').filterDate('2020-06-21','2020-06-21').map(maskS2clouds).sort('system:time_start',False)
S2_2019_winter = ee.ImageCollection('COPERNICUS/S2_SR').filterDate('2019-06-21','2019-09-21').map(maskS2clouds).sort('system:time_start',False)
S2_winter = S2_2019_winter.median().select(['B1','B2','B3','B4','B5','B6','B7','B8','B9','B11','B12','B8A']).rename(['B1_w','B2_w','B3_w','B4_w','B5_w','B6_w','B7_w','B8_w','B9_w','B11_w','B12_w','B8A_w'])
S2_2019_spring = ee.ImageCollection('COPERNICUS/S2_SR').filterDate('2019-09-21','2019-12-21').map(maskS2clouds).sort('system:time_start',False)
S2_spring = S2_2019_spring.median().select(['B1','B2','B3','B4','B5','B6','B7','B8','B9','B11','B12','B8A']).rename(['B1_s','B2_s','B3_s','B4_s','B5_s','B6_s','B7_s','B8_s','B9_s','B11_s','B12_s','B8A_s'])
#//LOAD COLLECTIONS
S2_mosaic = S2_summer.addBands(S2_spring).addBands(S2_autumn).addBands(S2_winter).clip(geometry)
#//S2_mosaic = S2_summer
#//Load Sentinel-1 SAR collection and filter according to data collection type
S1 = ee.ImageCollection('COPERNICUS/S1_GRD').filter(ee.Filter.eq('orbitProperties_pass','DESCENDING')).filterBounds(geometry).filterDate('2016-01-01','2100-12-31').filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
#  //.filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
#// Filter speckle noise
def filterSpeckles(img):
    ##  //vv = img.select('VV') //select the VV polarization band
    vh = img.select('VH')
    meters = ee.Number(100)
    ##  //var vv_smoothed = vv.focal_median(meters,'circle','meters').rename('VV_Filtered') //Apply a focal median filter
    vh_smoothed = vh.focal_median(meters, 'circle', 'meters').rename('VH_Filtered')
    return img.addBands(vh_smoothed) #// Add filtered VV band to original image

#// Map speckle noise filter across collection. Result is same collection, with smoothed VV band added to each image
S1 = S1.map(filterSpeckles)
#//Get one image per month in Magallanes
S1_Jan = S1.select('VH_Filtered').filterDate('2019-01-01','2019-01-30').mosaic().rename('VH_Jan')
S1_Feb = S1.select('VH_Filtered').filterDate('2019-02-01','2019-02-28').mosaic().rename('VH_Feb')
S1_Mar_2019 = S1.select('VH_Filtered').filterDate('2019-03-01','2019-03-30').mosaic().rename('VH_Mar')
S1_Mar_2017 = S1.select('VH_Filtered').filterDate('2017-03-01','2017-03-30').mosaic().rename('VH_Mar')
S1_Mar = ee.ImageCollection([S1_Mar_2019,S1_Mar_2017]).mosaic()
S1_Apr = S1.select('VH_Filtered').filterDate('2019-04-01','2019-04-30').mosaic().rename('VH_Apr')
S1_May_2019 = S1.select('VH_Filtered').filterDate('2019-05-01','2019-05-31').mosaic().rename('VH_May')
S1_May_2020 = S1.select('VH_Filtered').filterDate('2020-05-01','2020-05-31').mosaic().rename('VH_May')
S1_May = ee.ImageCollection([S1_May_2019,S1_May_2020]).mosaic()
S1_Jun = S1.select('VH_Filtered').filterDate('2019-06-01','2019-06-30').mosaic().rename('VH_Jun')
S1_Jul = S1.select('VH_Filtered').filterDate('2019-07-01','2019-07-30').mosaic().rename('VH_Jul')
S1_Aug = S1.select('VH_Filtered').filterDate('2019-08-01','2019-08-30').mosaic().rename('VH_Aug')
S1_Sep = S1.select('VH_Filtered').filterDate('2019-09-01','2019-09-30').mosaic().rename('VH_Sep')
S1_Oct_2019 = S1.select('VH_Filtered').filterDate('2019-10-01','2019-10-31').mosaic().rename('VH_Oct')
S1_Oct_2018 = S1.select('VH_Filtered').filterDate('2018-10-31','2018-11-01').mosaic().rename('VH_Oct')
S1_Oct = ee.ImageCollection([S1_Oct_2019,S1_Oct_2018]).mosaic()
S1_Nov = S1.select('VH_Filtered').filterDate('2019-11-01','2019-11-30').mosaic().rename('VH_Nov')
S1_Dec = S1.select('VH_Filtered').filterDate('2019-12-01','2019-12-30').mosaic().rename('VH_Dec')
S1_12months = S1_Jan.addBands(S1_Feb).addBands(S1_Mar).addBands(S1_Apr).addBands(S1_May).addBands(S1_Jun).addBands(S1_Jul).addBands(S1_Aug).addBands(S1_Sep).addBands(S1_Oct).addBands(S1_Nov).addBands(S1_Dec)

dem = ee.Image("users/italomoletto/NEXER/NASADEM_merged_UTM19S_resampled").rename('dem');
slope = ee.Terrain.slope(dem)
aspect = ee.Terrain.aspect(dem)
tpi = ee.Image("users/italomoletto/NEXER/TPI").rename('tpi');
ndvi = S2_mosaic.normalizedDifference(['B8','B4']).rename('ndvi')
ndwi = S2_mosaic.normalizedDifference(['B8','B11']).rename('ndwi')
seli = S2_mosaic.normalizedDifference(['B8A','B5']).rename('seli')
ndsi = S2_mosaic.normalizedDifference(['B3','B11']).rename('ndsi')

ndvi_a = S2_mosaic.normalizedDifference(['B8_a','B4_a']).rename('ndvi_a')
ndwi_a = S2_mosaic.normalizedDifference(['B8_a','B11_a']).rename('ndwi_a')
seli_a = S2_mosaic.normalizedDifference(['B8A_a','B5_a']).rename('seli_a')
ndsi_a = S2_mosaic.normalizedDifference(['B3_a','B11_a']).rename('ndsi_a')

ndvi_w = S2_mosaic.normalizedDifference(['B8_w','B4_w']).rename('ndvi_w')
ndwi_w = S2_mosaic.normalizedDifference(['B8_w','B11_w']).rename('ndwi_w')
seli_w = S2_mosaic.normalizedDifference(['B8A_w','B5_w']).rename('seli_w')
ndsi_w = S2_mosaic.normalizedDifference(['B3_w','B11_w']).rename('ndsi_w')

ndvi_s = S2_mosaic.normalizedDifference(['B8_s','B4_s']).rename('ndvi_s')
ndwi_s = S2_mosaic.normalizedDifference(['B8_s','B11_s']).rename('ndwi_s')
seli_s = S2_mosaic.normalizedDifference(['B8A_s','B5_s']).rename('seli_s')
ndsi_s = S2_mosaic.normalizedDifference(['B3_s','B11_s']).rename('ndsi_s')

facc = ee.ImageCollection('users/imerg/flow_acc_3s').mosaic()
tan_slope = slope.multiply(3.14).divide(180).tan()
res = ee.Number(30).pow(2)
uca = facc.expression('(facc+1)*res',{'facc':facc,'res':res}).rename('uca')
twi = uca.expression('(log(uca))/tan_slope',{'uca':uca.select('uca'),'tan_slope':tan_slope}).rename('twi')


image = S1_12months.addBands(S2_mosaic).addBands(dem).addBands(slope).addBands(aspect).addBands(tpi).addBands(twi).addBands(ndvi).addBands(ndwi).addBands(seli).addBands(ndsi).addBands(ndvi_a).addBands(ndwi_a).addBands(seli_a).addBands(ndsi_a).addBands(ndvi_w).addBands(ndwi_w).addBands(seli_w).addBands(ndsi_w).addBands(ndvi_s).addBands(ndwi_s).addBands(seli_s).addBands(ndsi_s)


Map.addLayer(S2_mosaic.select(['B4','B3','B2']),{"min":0,"max":0.2})
Map.addLayer(S1_12months,{"min":-35,"max":5})
classifier = ee.Classifier.smileRandomForest(500)
classifier_svm = ee.Classifier.svm('voting', "C_SVC","RBF",True,None,0.000001,None,64)
classifier_cart = ee.Classifier.smileCart(None, 1)

print(S2_mosaic)
#print(training)
#// Train the classifier.
#//var bands_mixed = ['B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12']
#//var bands_mixed = ['VH_Jan','VH_Feb','VH_Mar','VH_Apr','VH_May','VH_Jun','VH_Jul','VH_Aug','VH_Sep','VH_Oct','VH_Nov','VH_Dec','B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','dem','slope','aspect','ndvi','ndwi','seli','ndsi']
bands_mixed = ['VH_Jan','VH_Feb','VH_Mar','VH_Apr','VH_May','VH_Jun','VH_Jul','VH_Aug','VH_Sep','VH_Oct','VH_Nov','VH_Dec','B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','dem','slope','aspect','ndvi','ndwi','seli','ndsi','B1_a','B2_a','B3_a','B4_a','B5_a','B6_a','B7_a','B8_a','B9_a','B11_a','B12_a','B8A_a','ndvi_a','ndwi_a','seli_a','ndsi_a','B1_w','B2_w','B3_w','B4_w','B5_w','B6_w','B7_w','B8_w','B9_w','B11_w','B12_w','B8A_w','ndvi_w','ndwi_w','seli_w','ndsi_w','B1_s','B2_s','B3_s','B4_s','B5_s','B6_s','B7_s','B8_s','B9_s','B11_s','B12_s','B8A_s','ndvi_s','ndwi_s','seli_s']
bands_mixed = ['VH_Jan','VH_Feb','VH_Mar','VH_Apr','VH_May','VH_Jun','VH_Jul','VH_Aug','VH_Sep','VH_Oct','VH_Nov','VH_Dec','B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12','dem','slope','aspect','ndvi','ndwi','seli','ndsi','B1_a','B2_a','B3_a','B4_a','B5_a','B6_a','B7_a','B8_a','B9_a','B11_a','B12_a','B8A_a','ndvi_a','ndwi_a','seli_a','ndsi_a','B1_s','B2_s','B3_s','B4_s','B5_s','B6_s','B7_s','B8_s','B9_s','B11_s','B12_s','B8A_s','ndvi_s','ndwi_s','seli_s']

#//var bands_mixed = ['B1_a','B2_a','B3_a','B4_a','B5_a','B6_a','B7_a','B8_a','B9_a','B11_a','B12_a','B8A_a','ndvi_a','ndwi_a','seli_a','ndsi_a','B1_w','B2_w','B3_w','B4_w','B5_w','B6_w','B7_w','B8_w','B9_w','B11_w','B12_w','B8A_w','ndvi_w','ndwi_w','seli_w','ndsi_w','B1_s','B2_s','B3_s','B4_s','B5_s','B6_s','B7_s','B8_s','B9_s','B11_s','B12_s','B8A_s','ndvi_s','ndwi_s','seli_s']

#// Overlay the points on the imagery to get training.
#/*
#var training = image.select(bands_mixed).sampleRegions({
#  collection: training_polygons,
#  properties: ['landcover'],
#  scale: 10
#});
#*/
training = ee.FeatureCollection('users/italomoletto/NEXER/lc_data_allseasons')
#// Filter out the null property values and try again.
trainingNoNulls = training.filter(
  ee.Filter.notNull(training.first().propertyNames())
);
print(training)
trained2 = classifier.train(trainingNoNulls, 'landcover', bands_mixed);
trained_svm = classifier_svm.train(trainingNoNulls, 'landcover', bands_mixed);
trained_cart = classifier_cart.train(trainingNoNulls, 'landcover', bands_mixed);
print(trained2)
#// Classify the image.

classified = image.classify(trained2);
classified_svm = image.classify(trained_svm);
classified_cart = image.classify(trained_cart);
print(classified)
imageVisParam = {"opacity":1,"bands":["classification"],"min":0,"max":11,"palette":["000000","040cff","2700ff","008f40","2a4913","ffb100","ff9006","ffffff","fff81f","878787","ff0000","ff1414"]};
Map.addLayer(classified,imageVisParam);
Map.addLayer(classified_svm,imageVisParam);
Map.addLayer(classified_cart,imageVisParam);

#//Map.addLayer(image)
Map.addLayer(training_polygons)

"""
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

var extract_values = image.select(bands_mixed).reproject('EPSG:4326',null,10).reduceRegions({collection:training_polygons,reducer:ee.Reducer.first()})
//print(extract_values)

// Export the FeatureCollection to a KML file.
Export.table.toDrive({
  collection: extract_values,
  description:'lc_data',
  folder:'PeatBog_Magallanes',
  fileFormat: 'SHP'
});


"""