// Import input_features (above)
var input_features = image;

var bands = input_features.bandNames();

// // // // // Create training data
// var training = input_features.select(bands).sampleRegions({
//   collection: newfc,
//   properties: ['landcover'],  
//   scale: 30,
//   tileScale:15
// });
// print(training);

// Create the training data (using statified random sampling)
var training = input_features.stratifiedSample({
  numPoints: 5000, // This number defines max number of input px per class
  classBand:'Value',
  tileScale: 1,
  scale: 30
});

// Add a random column (named random, specify seed value for repeatability)
training = training.randomColumn('random', 42);

// Split into training and validation sets
var filter_training = ee.Filter.gt('random', 0.3); 
var filter_validation = ee.Filter.lte('random', 0.3);
var OZWT_training = training.filter(filter_training);

var OZWT_validation = training.filter(filter_validation);

// Print the training data, created above
print("Number of training pixels:", OZWT_training.size());
print("Number of validation pixels:", OZWT_validation.size());
print("Min class value:", OZWT_training.reduceColumns(ee.Reducer.min(), ['Value']).get('min'));
print("Max class value:", OZWT_training.reduceColumns(ee.Reducer.max(), ['Value']).get('max'));

// Create Classifier
var classifier = ee.Classifier.smileRandomForest(
  {numberOfTrees: 300,
  variablesPerSplit: 20,
  minLeafPopulation: 3,
  // maxNodes: 130 // This has a big effect when <100
  })
 .setOutputMode('MULTIPROBABILITY')
 .train({
    features: OZWT_training,
    classProperty: 'Value',
    inputProperties: bands
});

// Run classification
var classified = input_features.select(bands).classify(classifier);

//Turn the results into a multi-band image.
var classifiedImage = classified
  // Get rid of the extra dimensions.
  .arrayProject([0])
  .arrayFlatten([
    ['0', '1', '2', '3','4','5','6','7', '8']
]);

// Display classification
Map.centerObject(classified, 8);
Map.addLayer(classifiedImage, {}, 'classified_PROB');
// Map.addLayer(classified,
// {min: 0, max: 7, palette: ['#29951C', '#19AD29', '#B08615','#E7E00F', '#70FA0A', '#E800FF', '#FF0000', '#0800FF', '#808080']},
// 'classification'); 

// // Export to GDrive
Export.image.toDrive({
  image: classifiedImage,
  description: 'MP_1990',
  scale: 30,
  region: geometry,
  maxPixels: 4e9,
});

// Get details of classifier
var classifier_details = classifier.explain();

// Explain the classifier with importance values
var variable_importance = ee.Feature(null, ee.Dictionary(classifier_details).get('importance'));

var chart =
  ui.Chart.feature.byProperty(variable_importance)
  .setChartType('ColumnChart')
  .setOptions({
  title: 'Random Forest Variable Importance',
  legend: {position: 'none'},
  hAxis: {title: 'Bands'},
  vAxis: {title: 'Importance'}
});

// Plot a chart
print("Variable importance:", chart);

                                        ///////////////VALIDATION//////////////////

// Independent validation (more useful)
var validation_data = OZWT_validation.classify(classifier);
var errorMatrix_val = validation_data.errorMatrix('Value','classification');

print('Validation error matrix: ', errorMatrix_val);
print('Validation user accuracy: ', errorMatrix_val.consumersAccuracy());
print('Validation producer accuracy: ', errorMatrix_val.producersAccuracy());
print('Validation overall accuracy: ', errorMatrix_val.accuracy());
