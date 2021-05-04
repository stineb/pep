# map chunks generated with:

Google Earth Engine export

```
// years to process (from start year t0 to end year t1)
var t0 = "2010";
var t1 = "2019";

var lc = ee.ImageCollection("MODIS/006/MCD12Q1")
.select('LC_Type1')
.filterDate(t0.concat("-01-01"),t1.concat("-12-31"))
.median()
.reproject('EPSG:4326', null, 500);

var forest = lc.updateMask(lc.eq(4));

var NH = 
    ee.Geometry.Polygon(
        [[[-180, 80],
          [-180, -80],
          [180, -80],
          [180, 80]]], null, false);

// Export the image, specifying scale and region.
// Divide it up into regions (see above) not to choke GEE on export.
Export.image.toDrive({
  image: forest,
  description: 'DB_forest_NH',
  scale: 500,
  maxPixels: 27216515550,
  region: NH
});
```

# merged with:

Some GDAL foo

```
gdalbuildvrt tmp.vrt *.tif

gdalwarp -overwrite -multi -wo "NUM_THREADS=ALL_CPUS" -of GTiff -co "BIGTIFF=YES" -co "COMPRESS=DEFLATE" tmp.vrt  IGBP_DB_2010_2019.tif
```
# R pre-processing

Focal smoother in R


```
r <- raster("IGBP_DB_2010_2019.tif")
r <- focal(r, w = matrix(1, nc = 3, nr = 3),
 		mean,
 		na.rm = FALSE)
writeRaster(r, "IGBP_DB_2010_2019_eroded.tif",
	options="COMPRESS=DEFLATE", 
	overwrite=TRUE)
```

# R sampling

Simple randomized sampling

```
locations <- sampleRandom(r, 20000, cells = TRUE)
lon_lat <- xyFromCell(r, cell = locations[,1])

```
