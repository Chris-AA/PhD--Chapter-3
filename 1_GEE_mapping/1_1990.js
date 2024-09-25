// Load input images (Landsat 5, Landsat 7, Landsat 8)
 
var l8_bands = ['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B10', 'pixel_qa'];
var l7_bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7','B6','pixel_qa'];
var l5_bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'B6',  'pixel_qa'];
var l4_bands = ['B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'B6',  'pixel_qa'];
var std_names = ['blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'temp', 'pixel_qa'];
 
// Load landsat data
var l8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
    .select(l8_bands, std_names)
    .filterMetadata('CLOUD_COVER_LAND', 'less_than', 50);
var l7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')
    .select(l7_bands, std_names)
    .filterMetadata('CLOUD_COVER_LAND', 'less_than', 50);
var l5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')
    .select(l5_bands, std_names)
    .filterMetadata('CLOUD_COVER_LAND', 'less_than', 50);
var l4 = ee.ImageCollection('LANDSAT/LT04/C01/T1_SR')
    .select(l4_bands, std_names)
    .filterMetadata('CLOUD_COVER_LAND', 'less_than', 50);
    
// Apply cloud and cloud shadow mask to each image
var maskLandsat= (function(im){

  var cloudShadowBitMask = (1 << 3);  // cloud shadow
  var cloudsBitMask = (1 << 5);    // cloud

// Get the pixel QA band.
var qa = im.select('pixel_qa');

// Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudShadowBitMask).eq(0)
    .bitwise_and(qa.bitwiseAnd(cloudsBitMask).eq(0));
 
return im.updateMask(mask);
});

// Merge all Landsat data
var landsat = l8.merge(l7).merge(l5).merge(l4).map(maskLandsat);

// Select time period
var collection = landsat.filterDate('1990-05-01', '1990-10-31')
                        .filterBounds(geometry);
print(collection);                        
//////////////////////////////TERRAIN CORRECTION FUNCTION ///////////////////////
var scale = 300;

// get terrain layers
var dem = ee.Image("USGS/SRTMGL1_003");
var degree2radian = 0.01745;


var terrainCorrection = function(collection) {

  
  collection = collection.map(illuminationCondition);
  collection = collection.map(illuminationCorrection);

  return(collection);

  ////////////////////////////////////////////////////////////////////////////////
  // Function to calculate illumination condition (IC). Function by Patrick Burns and Matt Macander 
  function illuminationCondition(img){

  // Extract image metadata about solar position
  var SZ_rad = ee.Image.constant(ee.Number(img.get('SOLAR_ZENITH_ANGLE'))).multiply(3.14159265359).divide(180).clip(img.geometry().buffer(10000)); 
  var SA_rad = ee.Image.constant(ee.Number(img.get('SOLAR_AZIMUTH_ANGLE')).multiply(3.14159265359).divide(180)).clip(img.geometry().buffer(10000)); 
  // Creat terrain layers
  var slp = ee.Terrain.slope(dem).clip(img.geometry().buffer(10000));
  var slp_rad = ee.Terrain.slope(dem).multiply(3.14159265359).divide(180).clip(img.geometry().buffer(10000));
  var asp_rad = ee.Terrain.aspect(dem).multiply(3.14159265359).divide(180).clip(img.geometry().buffer(10000));
  
  // Calculate the Illumination Condition (IC)
  // slope part of the illumination condition
  var cosZ = SZ_rad.cos();
  var cosS = slp_rad.cos();
  var slope_illumination = cosS.expression("cosZ * cosS", 
                                          {'cosZ': cosZ,
                                           'cosS': cosS.select('slope')});
  // aspect part of the illumination condition
  var sinZ = SZ_rad.sin(); 
  var sinS = slp_rad.sin();
  var cosAziDiff = (SA_rad.subtract(asp_rad)).cos();
  var aspect_illumination = sinZ.expression("sinZ * sinS * cosAziDiff", 
                                           {'sinZ': sinZ,
                                            'sinS': sinS,
                                            'cosAziDiff': cosAziDiff});
  // full illumination condition (IC)
  var ic = slope_illumination.add(aspect_illumination);

  // Add IC to original image
  var img_plus_ic = ee.Image(img.addBands(ic.rename('IC')).addBands(cosZ.rename('cosZ')).addBands(cosS.rename('cosS')).addBands(slp.rename('slope')));
  return img_plus_ic;
  }
   ////////////////////////////////////////////////////////////////////////////////
  // Function to apply the Sun-Canopy-Sensor + C (SCSc) correction method to each 
  // image. Function by Patrick Burns and Matt Macander 
  function illuminationCorrection(img){
    var props = img.toDictionary();
    var st = img.get('system:time_start');
    
    var img_plus_ic = img;
    var mask1 = img_plus_ic.select('nir').gt(-0.1);
    var mask2 = img_plus_ic.select('slope').gte(5)
                            .and(img_plus_ic.select('IC').gte(0))
                            .and(img_plus_ic.select('nir').gt(-0.1));
    var img_plus_ic_mask2 = ee.Image(img_plus_ic.updateMask(mask2));
    
    // Specify Bands to topographically correct  
    var bandList = ['blue','green','red','nir','swir1','swir2']; 
    var compositeBands = img.bandNames();
    var nonCorrectBands = img.select(compositeBands.removeAll(bandList));
    
    var geom = ee.Geometry(img.get('system:footprint')).bounds().buffer(10000);
    
    function apply_SCSccorr(band){
      var method = 'SCSc';
      var out = img_plus_ic_mask2.select('IC', band).reduceRegion({
      reducer: ee.Reducer.linearFit(), // Compute coefficients: a(slope), b(offset), c(b/a)
      geometry: ee.Geometry(img.geometry().buffer(-5000)), // trim off the outer edges of the image for linear relationship 
      scale: 300,
      maxPixels: 1000000000
      });  

   if (out === null || out === undefined ){
       return img_plus_ic_mask2.select(band);
       }
  
  else{
      var out_a = ee.Number(out.get('scale'));
      var out_b = ee.Number(out.get('offset'));
      var out_c = out_b.divide(out_a);
      // Apply the SCSc correction
      var SCSc_output = img_plus_ic_mask2.expression(
        "((image * (cosB * cosZ + cvalue)) / (ic + cvalue))", {
        'image': img_plus_ic_mask2.select(band),
        'ic': img_plus_ic_mask2.select('IC'),
        'cosB': img_plus_ic_mask2.select('cosS'),
        'cosZ': img_plus_ic_mask2.select('cosZ'),
        'cvalue': out_c
      });
      
      return SCSc_output;
    }
      
    }
    
    var img_SCSccorr = ee.Image(bandList.map(apply_SCSccorr)).addBands(img_plus_ic.select('IC'));
    var bandList_IC = ee.List([bandList, 'IC']).flatten();
    img_SCSccorr = img_SCSccorr.unmask(img_plus_ic.select(bandList_IC)).select(bandList);
    
    return img_SCSccorr.addBands(nonCorrectBands)
      .setMulti(props)
      .set('system:time_start',st);
  }
  
};

collection = terrainCorrection(collection);
// print(collection);
// var newimg = ee.Image(collection.median()).clip(geometry);
// var img = ee.Image(landsat.median()).clip(geometry);

//////////// Add spectral indices, borrowed from mapbiomas////////////////
var addSpectralIndices = function(im){
  im = im.addBands(im.expression(                                     //EVI
    '2.5 * (N - R) / (N + 6 * R - 7.5 * B + 1)',{
      'R': im.select('red').multiply(0.0001),
      'B': im.select('blue').multiply(0.0001),
      'N': im.select('nir').multiply(0.0001),
    }).rename('EVI'));
  im = im.addBands(im.normalizedDifference(['nir','swir1']).rename('NDWI'));  // NDWI
  im = im.addBands(im.normalizedDifference(['nir','swir2']).rename('NBR'));   // NBR
    
  return im;
};

var calculateTasseledCap = function (image){
  var b = image.select("blue", "green", "red", "nir", "swir1", "swir2");
  var brightness_= ee.Image([0.3029, 0.2786, 0.4733, 0.5599, 0.508, 0.1872]);
  var greenness_= ee.Image([-0.2941, -0.243, -0.5424, 0.7276, 0.0713, -0.1608]);
  var wetness_= ee.Image([ 0.1511, 0.1973, 0.3283, 0.3407, -0.7117, -0.4559]);
  var sum = ee.call("Reducer.sum");
  var brightness = b.multiply(brightness_).reduce(sum);
  var greenness = b.multiply(greenness_).reduce(sum);
  var wetness = b.multiply(wetness_).reduce(sum);
  return ee.Image(brightness).addBands(greenness).addBands(wetness)};
  var landsat_tc = collection.mosaic();
var TasseledCap = calculateTasseledCap(landsat_tc)
                           .select([0,1,2],["Brightness","Greenness","Wetness"]);


// Apply functions to image
collection = collection.map(addSpectralIndices);

print(collection);
// Produce an annual median composite 
var L_composite = collection.select(['blue', 'green', 'red', 'nir', 'swir1', 'swir2'])
                                .reduce(ee.Reducer.median());

//Spectral indices
var indices = ['EVI','NDWI','NBR'];
var spectral_percentiles = collection.select(indices)
                      .reduce(ee.Reducer.percentile([10,25,50,75,90]));
                       
// Create single image with all bands
var input_features = L_composite
          .addBands(spectral_percentiles)
          .addBands(TasseledCap)
          .clip(proj_area);

// Select bands
var bands = input_features.bandNames();

//input_features = input_features.select(['']);

print(input_features);

// Rasterise training data
var Sample_rasterised = Sample.reduceToImage({
    properties:['landcover'],
    reducer:ee.Reducer.first()}).toInt();

// // Add a class band to features
input_features = input_features.addBands((Sample_rasterised).toInt().rename('Value'));

// //////////////////////// Display the result////////////////////////////
var visParams = {bands: ['red_median', 'green_median', 'blue_median'],min: 0, max: 1000};
Map.addLayer(input_features, visParams, 'input_features');


// Output to asset
Export.image.toAsset({
  image: input_features,
  description: 'C3_1990',
  assetId: 'C3_1990',
  scale: 30,
  region: geometry,
  maxPixels: 1e9,
  pyramidingPolicy: {
    'Value': 'mode'
  }
});
