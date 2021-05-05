# Random pixel selection
library(raster)

# set land cover classes
lc <- c("DB","DN")

# process land cover classes
lapply(lc, function(cl){
  # load tif file
  r <- raster(sprintf("data/land_cover/IGBP_%s_2010_2019_eroded.tif",cl))
  
  # export all DB pixel locations and save as RDS
  if(cl == "DB"){
    rr <- rasterToPoints(r, fun=function(x){x==4})  
  } else {
    rr <- rasterToPoints(r, fun=function(x){x==3})
  }
  
  rr <- rr[,1:2]
  saveRDS(rr, sprintf("data/%s_locations.rds",cl))
  
  # pseudo random locations
  loc <- sample(1:nrow(rr), size = 10000)
  ss <- rr[loc,]
  
  # write subset to disk
  saveRDS(ss, sprintf("data/%s_location_selection.rds",cl))
})

