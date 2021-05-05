## GEE code

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
  description: 'DN_forest',
  scale: 500,
  maxPixels: 27216515550,
  region: NH
});
```
## gdal merge

```
gdalbuildvrt tmp.vrt *.tif
gdalwarp -overwrite -multi -wo "NUM_THREADS=ALL_CPUS" -of GTiff -co "BIGTIFF=YES" -co "COMPRESS=DEFLATE" tmp.vrt  IGBP_DB_2010_2019.tif
```

## erode pixels

```
r <- raster("data/landcover/IGBP_DB_2010_2019.tif")
r <- focal(r, w = matrix(1, nc = 3, nr = 3), mean, na.rm = FALSE)
writeRaster(r, "data/land_cover/IGBP_DB_2010_2019_eroded.tif",
            options="COMPRESS=DEFLATE", 
            overwrite=TRUE)
```

## Land Cover

IGBP land cover (at 500m) for all Deciduous Broadleaf pixels

## Koeppen-Geiger classes

Downloaded from the World Bank, but provided by U. Wien.

