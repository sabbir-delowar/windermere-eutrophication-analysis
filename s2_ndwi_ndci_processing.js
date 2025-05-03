Map.centerObject(polygon, 10)
//collect image and cloud probablity data
var s2Sr = ee.ImageCollection('COPERNICUS/S2_SR');
      .select('B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8','B8A','B9','B11','B12');
var s2Clouds = ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY');

var START_DATE = ee.Date('2021-07-01');
var END_DATE = ee.Date('2021-08-31');
var MAX_CLOUD_PROBABILITY = 65;
var region = polygon;

//cloud mask
function maskClouds(img) {
  var clouds = ee.Image(img.get('cloud_mask')).select('probability');
  var isNotCloud = clouds.lt(MAX_CLOUD_PROBABILITY);
  return img.updateMask(isNotCloud);
}

// The masks for the 10m bands sometimes do not exclude bad data at
// scene edges, so we apply masks from the 20m and 60m bands as well.
// Example asset that needs this operation:
// COPERNICUS/S2_CLOUD_PROBABILITY/20190301T000239_20190301T000238_T55GDP
function maskEdges(s2_img) {
  return s2_img.updateMask(
      s2_img.select('B8A').mask().updateMask(s2_img.select('B9').mask()));
}

// Filter input collections by desired data range and region.
var criteria = ee.Filter.and(
    ee.Filter.bounds(region), ee.Filter.date(START_DATE, END_DATE));
s2Sr = s2Sr.filter(criteria).map(maskEdges);
s2Clouds = s2Clouds.filter(criteria);
print(s2Sr.size(), 'Number of images used')

// Join S2 SR with cloud probability dataset to add cloud mask.
var s2SrWithCloudMask = ee.Join.saveFirst('cloud_mask').apply({
  primary: s2Sr,
  secondary: s2Clouds,
  condition:
      ee.Filter.equals({leftField: 'system:index', rightField: 'system:index'})
});
var s2CloudMasked =
    ee.ImageCollection(s2SrWithCloudMask).map(maskClouds).median().reproject({
    crs: 'EPSG:4326',
    scale: 30
  });

//Pansharpening
var image = s2CloudMasked


/**
 * Pansharpens a Sentinel 2 image.
 *
 * Arguments:
 *
 * params - a client-side object containing:
 *
 *    image (Image, required)
 *        The image to pansharpen
 *
 *    geometry (Geometry, default: image.geometry())
 *        The region to pansharpen
 *
 *    crs (Projection, default: projection of image's first band)
 *        The projection to work in.
 *
 *    maxPixels (Long, default: 10000000)
 *        The maximum number of pixels to reduce.
 *
 *    bestEffort (Boolean, default: false)
 *        If the geometry would contain more pixels than maxPixels,
 *        compute and use a larger scale which would allow the operation to succeed.
 *
 *    tileScale (Float, default: 1)
 *        A scaling factor used to reduce aggregation tile size;
 *        using a larger tileScale (e.g. 2 or 4) may enable computations
 *        that run out of memory with the default.
 */
function panSharpen(params) {
  if (params && !(params.image instanceof ee.Image))
    throw Error('panSharpen(params): You must provide an params object with an image key.')

  var image = params.image
  var geometry = params.geometry || image.geometry()
  var crs = params.crs || image.select(0).projection()
  var maxPixels = params.maxPixels
  var bestEffort = params.bestEffort || false
  var tileScale = params.tileScale || 1

  var bands10m = ['B2', 'B3', 'B4', 'B8']
  var bands20m = ['B5', 'B6', 'B7', 'B8A', 'B11', 'B12']
  var panchromatic = image
    .select(bands10m)
    .reduce(ee.Reducer.mean())
  var image20m = image.select(bands20m)
  var image20mResampled = image20m.resample('bilinear')

  var stats20m = image20m
    .reduceRegion({
      reducer: ee.Reducer.stdDev().combine(
        ee.Reducer.mean(), null, true
      ),
      geometry: polygon,
      scale: 20,
      crs: crs,
      bestEffort: bestEffort,
      maxPixels: maxPixels,
      tileScale: tileScale
    })
    .toImage()

  var mean20m = stats20m
    .select('.*_mean')
    .regexpRename('(.*)_mean', '$1')

  var stdDev20m = stats20m
    .select('.*_stdDev')
    .regexpRename('(.*)_stdDev', '$1')

  var kernel = ee.Kernel.fixed({
    width: 5,
    height: 5,
    weights: [
      [-1, -1, -1, -1, -1],
      [-1, -1, -1, -1, -1],
      [-1, -1, 24, -1, -1],
      [-1, -1, -1, -1, -1],
      [-1, -1, -1, -1, -1]
    ],
    x: -3,
    y: -3,
    normalize: false
  })

  var highPassFilter = panchromatic
    .convolve(kernel)
    .rename('highPassFilter')

  var stdDevHighPassFilter = highPassFilter
    .reduceRegion({
      reducer: ee.Reducer.stdDev(),
      geometry: polygon,
      scale: 10,
      crs: crs,
      bestEffort: bestEffort,
      maxPixels: maxPixels,
      tileScale: tileScale
    })
    .getNumber('highPassFilter')

  function calculateOutput(bandName) {
    bandName = ee.String(bandName)
    var W = ee.Image().expression(
      'stdDev20m / stdDevHighPassFilter * modulatingFactor', {
        stdDev20m: stdDev20m.select(bandName),
        stdDevHighPassFilter: stdDevHighPassFilter,
        modulatingFactor: 0.25
      }
    )
    return ee.Image()
      .expression(
        'image20mResampled + (HPF * W)', {
          image20mResampled: image20mResampled.select(bandName),
          HPF: highPassFilter,
          W: W
      }
    )
    .uint16()
  }

  var output = ee.ImageCollection(
      bands20m.map(calculateOutput)
    )
    .toBands()
    .regexpRename('.*_(.*)', '$1')

  var statsOutput = output
    .reduceRegion({
      reducer: ee.Reducer.stdDev().combine(
        ee.Reducer.mean(), null, true
      ),
      geometry: polygon,
      scale: 10,
      crs: crs,
      bestEffort: bestEffort,
      maxPixels: maxPixels,
      tileScale: tileScale
    })
    .toImage()

  var meanOutput = statsOutput
    .select('.*_mean')
    .regexpRename('(.*)_mean', '$1')

  var stdDevOutput = statsOutput
    .select('.*_stdDev')
    .regexpRename('(.*)_stdDev', '$1')

  var sharpened = ee.Image()
    .expression(
      '(output - meanOutput) / stdDevOutput * stdDev20m + mean20m', {
        output: output,
        meanOutput: meanOutput,
        stdDevOutput: stdDevOutput,
        stdDev20m: stdDev20m,
        mean20m: mean20m
      }
    )
    .uint16()

  return image
    .addBands(sharpened, null, true)
    .select(image.bandNames())
}
var sharpened = panSharpen({
  image: image,
  bestEffort: true
})

//print('sharpened', sharpened)
var visParams = {bands: 'B5,B8A,B12', min: 600, max: 4000}
var image1 = image.clip(polygon)
var sharpened = sharpened.clip(polygon)
//Map.addLayer(image1, visParams, 'Before sharpening')
//Map.addLayer(sharpened, visParams, 'sharpened')
//visualize true color
var rgbVis = {min: 0, max: 3000, bands: ['B4', 'B3', 'B2']};
//Map.addLayer( sharpened, rgbVis, 'S2 SR masked at ' + MAX_CLOUD_PROBABILITY + '%',true);

//land mask
var ndwi =
    sharpened.normalizedDifference(['B3', 'B8']).rename('NDWI');
//Map.addLayer(ndwi,{palette: ['red', 'yellow', 'green', 'cyan', 'blue']},'NDWI');
//add ndwi band to image
var sharpened1 = sharpened.addBands(ndwi)
// Create NDWI mask
var ndwiThreshold = sharpened1.select(['NDWI']).gte(0.0);
var AOI = sharpened1.updateMask(ndwiThreshold);
//Map.addLayer( ndwiThreshold, {palette:['black', 'white']},'NDWI Binary Mask');
var NDWI = ndwiThreshold.updateMask(ndwiThreshold);
//Map.addLayer(NDWI, {palette:['blue']}, 'NDWI');
//Map.addLayer(AOI, rgbVis, 'AOI');

//chl calculation
var ndci = AOI.expression(
      '((1/B4)-(1/B5))*B6', {
      'B4': AOI.select('B5'),
      'B5': AOI.select('B4'),
      'B6': AOI.select('B2')
});
var AOI = AOI.addBands(ndci);


//select only necessary bands
var AOI= AOI.select(['B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8','NDWI', 'NDCI'])

//get min max value and add layer
var MinMax = AOI.reduceRegion({
    reducer: ee.Reducer.minMax(),
    geometry: polygon,
    scale: 30,
    maxPixels: 1e10
})
print(MinMax)
MinMax.evaluate(function (MinMaxDict) {
  var BandCompViz = {
    min: MinMaxDict['NDCI_min'],
    max: MinMaxDict['NDCI_max'],
    palette:  ['00FFFF', '0000FF']
  };
  Map.addLayer(AOI.select('NDCI'), BandCompViz, 'NDCI');
});
//export ndci to drive
var projection = ndci.projection().getInfo();
Export.image.toDrive({
  image: ndci,
  description: 'Windermere_21_7-8',
  crs: projection.crs,
  crsTransform: projection.transform,
  region: polygon
});
