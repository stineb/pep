# Random pixel selection
library(raster)

# set land cover classes
lc <- c("DB","DN")

# process land cover classes
lapply(lc, function(cl){
  # load tif file
  r <- raster(sprintf("data/land_cover/IGBP_%s_2010_2019_eroded.tif",cl))
  
  # export all DB pixel locations and save as RDS
  rr <- rasterToPoints(r, fun=function(x){x==4})
  rr <- rr[,1:2]
  saveRDS(rr, sprintf("data/%s_locations.rds",cl))
  
  # pseudo random locations
  loc <- sample(1:nrow(rr), size = 8000)
  ss <- rr[loc,]
  
  # write subset to disk
  saveRDS(ss, sprintf("data/%s_location_selection.rds",cl))
})

