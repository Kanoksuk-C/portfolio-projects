// Create a region of interest
var roi = ee.Geometry.Rectangle(100.3500544, 13.5016874, 100.7999208, 14.0497482);
Map.centerObject(roi, 10);
Map.addLayer(roi, {}, 'ROI', false, 0.7);

// Import image collection: Harmonized Sentinel-2 MSI, Level-2A
var s2 = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')

// Compute the Normalized Difference Vegetation Index (NDVI)
var dataset = ee.Image(s2);
var addNDVI = function(dataset)
  {
    var ndvi = dataset.normalizedDifference(['B8', 'B4']).rename('NDVI');
    return dataset.addBands(ndvi);
  };

// Add NDVI band to s2 image collection
// var withNDVI = s2.map(addNDVI);
// and
// Get the least cloudy image between Oct2021 and Dec2021
var image = ee.Image(s2.map(addNDVI)
            .filterDate('2021-12-07', '2021-12-12')
            .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE',10))
            .filterBounds(roi)
            .median()
);

// Visualization parameters for True composite image
var ParamsTrue = {min: 0, max: 2550, bands: ['B4', 'B3', 'B2'],};
Map.addLayer(image.clip(roi), ParamsTrue, 'True Color', false);

// Visualization parameters for NDVI single band image
var ParamsNDVI = {min: -1, max: 1, palette: ['blue', 'white', 'green']};
Map.addLayer(image.select('NDVI').clip(roi), ParamsNDVI, 'NDVI', false);

// Load Ground Truth Data.
var gts = ee.FeatureCollection("users/sminggt/GroundTruth_shp");

// Paint the Ground Truth edges with different colors.
var empty = ee.Image().byte();
var gts_outlines = empty.paint({
  featureCollection: gts,
  color: 'class',
  width: 4
});

//Define a palette for land cover
var lulcPalette = [
  'Tan',        //farmland  (0)
  'Green',      //grass     (1)
  'Red',        //road      (2)
  'Gray',       //urban     (3)
  'LightCyan',  //water     (4)
];
Map.addLayer(gts_outlines, {palette: lulcPalette, min:0, max:4}, 'Ground Truth', false);

// Overlay the ground truth on the imagery to get training.
var label = 'class';
var bands = ['B2', 'B3', 'B4', 'B8', 'NDVI'];
var input = image.select(bands);
//
var trainImage = input.sampleRegions({
  collection: gts,
  properties: [label],
  scale: 30
});
// Split training data/testing data = 80/20
var trainingData = trainImage.randomColumn();
var trainSet = trainingData.filter(ee.Filter.lessThan('random',0.8));
var testSet = trainingData.filter(ee.Filter.greaterThanOrEquals('random',0.8));

//Classification Model
var classifier = ee.Classifier.smileCart().train(trainSet, label, bands);

// Classify the trainImage
var classified = input.classify(classifier);

Map.addLayer(classified.clip(roi), 
  {palette: lulcPalette, min:0, max:4}, 'Classification CART (Dec 2021)');

//Accuracy Assessment
var confusionMatrix = ee.ConfusionMatrix(testSet.classify(classifier)
  .errorMatrix({actual: 'class', predicted: 'classification'}));

print('Confusion Matrix (Dec 2021): ', confusionMatrix);
print('Overall Accuracy (Dec 2021): ', confusionMatrix.accuracy());
