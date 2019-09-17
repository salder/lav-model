


#using 2019 Date will be no improvment as the year was not so good according to cloud cover, tested 9/2019


//var geometry=table
var geometry=geometry


var s2 = ee.ImageCollection('COPERNICUS/S2')
.filterBounds(geometry);

// Function to mask clouds using the Sentinel-2 QA band.
function maskS2clouds(image) {
  var qa = image.select('QA60');
  // Bits 10 and 11 are clouds and cirrus, respectively.
  var cloudBitMask = ee.Number(2).pow(10).int();
  var cirrusBitMask = ee.Number(5).pow(20).int();//2,11
  // Both flags should be set to zero, indicating clear conditions.
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
    qa.bitwiseAnd(cirrusBitMask).eq(0));
  // Return the masked and scaled data.
  return image.updateMask(mask).divide(10000);
}

//tested but not bettre
// Map the function over one year of data and take the median.
//var composite =  s2.filter(ee.Filter.calendarRange(6,9,'month'))
//                 // s2.filterDate('2018-06-01', '2018-08-30')
//                  // Pre-filter to get less cloudy granules.
//                  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 0.1))
//                  .map(maskS2clouds)
//                  .filterBounds(geometry)
//                  .median();

//var collection = ee.ImageCollection('COPERNICUS/S2')
// .filterDate('2018-06-01', '2018-08-01')
//.filterBounds(geometry)
// .filterMetadata('CLOUDY_PIXEL_PERCENTAGE', 'less_than', 1);                  

var composite2 = // s2.filter(ee.Filter.calendarRange(6,9,'month'))
s2.filterDate('2018-06-01', '2018-08-30')
// Pre-filter to get less cloudy granules.
.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 0.1))
.map(maskS2clouds)
.filterBounds(geometry)
.median();  




//var composite_vinter=composite.clip(table) 
var composite_vinter2=composite2.clip(geometry)                  
//Map.addLayer(composite_vinter, {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3},'vinter.bete');






var nir = composite_vinter2.select('B8');
var red = composite_vinter2.select('B4');
var b2 = composite_vinter2.select('B2');
var b3 = composite_vinter2.select('B3');
var b8 = composite_vinter2.select('B8');
var b11 = composite_vinter2.select('B11');
var b12 = composite_vinter2.select('B12');
var b7 = composite_vinter2.select('B7');
var b6 = composite_vinter2.select('B6');
var b5 = composite_vinter2.select('B5');
var b4 = composite_vinter2.select('B4');
var ndvi = nir.subtract(red).divide(nir.add(red)).rename('NDVI');
var ndwi = b3.subtract(b8).divide(b3.add(b8)).rename('NDWI');
var soil= b8.subtract(red).divide(b8.add(red).add(0.5)).multiply(1.5);
var ndwi_land_clip = (ndwi.lt(-0.1));//0.2




var ndviParams = {min: -1, max: 1, palette: ['blue', 'white', 'green']};
var wetParams = {min: -0.2, max: 0.2, palette: ['red', 'green', 'blue']};
var bParams = {min: 0, max: 1, palette: ['blue', 'white', 'green']};


var ndci = composite_vinter2.expression(
  '(B5-B4)/(B5+B4)', {
    'B5': composite_vinter2.select('B5'),
    'B4': composite_vinter2.select('B4'),
    'B6': composite_vinter2.select('B6'),
    'B7': composite_vinter2.select('B7')
  });
var ndciParams = {min: -1, max: 1, palette: ['blue', 'white', 'green']};


var sipi = composite_vinter2.expression(
  '(B08 - B01) / (B08 - B04)', {
    'B08': composite_vinter2.select('B8'),
    'B01': composite_vinter2.select('B1'),
    'B04': composite_vinter2.select('B4'),
    'B7': composite_vinter2.select('B7')
  });
var sipiParams = {min: 0, max: 2, palette: ['blue', 'white', 'green']};


var L = 0.428;

var savi = composite_vinter2.expression(
  '(B08 - B04) / (B08 + B04 + L) * (1.0 + L)', {
    'B08': composite_vinter2.select('B8'),
    'B01': composite_vinter2.select('B1'),
    'B04': composite_vinter2.select('B4'),
    'B7': composite_vinter2.select('B7'),
    'L':L
  });




var gndvi = composite_vinter2.expression(
  '(B08 - B03) / (B08 + B03)', {
    'B08': composite_vinter2.select('B8'),
    'B01': composite_vinter2.select('B1'),
    'B04': composite_vinter2.select('B4'),
    'B03': composite_vinter2.select('B3'),
    'L':L
  });












// plots

Map.addLayer(composite_vinter2, {bands: ['B4', 'B3', 'B2'], min: 0, max: 0.3},'vinter.bete2');
Map.addLayer(ndvi, ndviParams, 'NDVI image');
Map.addLayer(ndwi, ndviParams, 'NDWI image');
Map.addLayer(soil, ndviParams, 'soil image');
Map.addLayer(ndwi_land_clip, ndviParams, 'land image');
Map.addLayer(ndci, ndviParams, 'NDCI image');
Map.addLayer(sipi, sipiParams, 'SIPI image');
Map.addLayer(savi, ndviParams, 'SAVI image');
Map.addLayer(gndvi, ndviParams, 'SAVI image');









///////////////////////////////////////////////////
  ///
  //////////////////////////////////////////////////
  
  //Export
var polygon=geometry

var result = composite_vinter2.select('B2');

Export.image.toDrive({
  image: result,
  description:'b2_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});










var result = composite_vinter2.select('B3');

Export.image.toDrive({
  image: result,
  description: 'b3_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result = composite_vinter2.select('B4');

Export.image.toDrive({
  image: result,
  description: 'b4_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result = composite_vinter2.select('B5');

Export.image.toDrive({
  image: result,
  description: 'b5_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result =composite_vinter2.select('B6');

Export.image.toDrive({
  image: result,
  description: 'b6_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result = composite_vinter2.select('B7');

Export.image.toDrive({
  image: result,
  description: 'b7_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result = composite_vinter2.select('B8');

Export.image.toDrive({
  image: result,
  description: 'b8_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result = composite_vinter2.select('B9');

Export.image.toDrive({
  image: result,
  description: 'b9_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result =composite_vinter2.select('B10');

Export.image.toDrive({
  image: result,
  description: 'b10_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result = composite_vinter2.select('B11');

Export.image.toDrive({
  image: result,
  description: 'b11_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result = composite_vinter2.select('B12');

Export.image.toDrive({
  image: result,
  description: 'b12_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result =ndvi;

Export.image.toDrive({
  image: result,
  description: 'ndvi_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result =ndci;

Export.image.toDrive({
  image: result,
  description: 'ndci_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});


var result =ndwi;

Export.image.toDrive({
  image: result,
  description: 'ndwi_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});



var result =soil;

Export.image.toDrive({
  image: result,
  description: 'soil_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});
result =ndwi_land_clip;


result =ndwi_land_clip;

Export.image.toDrive({
  image: result,
  description: 'land_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});




result =sipi;
Export.image.toDrive({
  image: result,
  description: 'sipi_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});



result =savi;
Export.image.toDrive({
  image: result,
  description: 'savi_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});




result =gndvi;
Export.image.toDrive({
  image: result,
  description: 'gndvi_lav_vinterbete',
  scale: 10,
  region: polygon,
  maxPixels:10000000000000, 
  crs:'EPSG:3006'
});

