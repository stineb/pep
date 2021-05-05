# erode pixels
library(raster)

# set land cover classes
lc <- c("DN")

# moving window analysis
lapply(lc, function(cl){
  
  # read in data
  r <- raster(sprintf("data/land_cover/IGBP_%s_2010_2019.tif",cl))
  
  # focal / moving window operation
  r <- focal(
    r,
    w = matrix(1,
               ncol = 3,
               nrow = 3),
    mean,
    na.rm = FALSE)
  
  # write data to file
  writeRaster(
    r,
    sprintf("data/land_cover/IGBP_%s_2010_2019_eroded.tif",cl),
    options="COMPRESS=DEFLATE", 
    overwrite=TRUE)
})
