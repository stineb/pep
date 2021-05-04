# Random pixel selection
library(raster)

# load tif file
r <- raster("data/land_cover/IGBP_DB_2010_2019_eroded.tif")

# select locations
locations <- sampleRandom(
  r,
  size = 10000, # retain 8K values roughly
  xy = TRUE,
  cell = TRUE)

# grab altitudes


# grab Koeppen-geigen values




