# Random pixel selection
library(raster)

# read koeppen-geiger data
kg <- raster::raster("data/koeppen_geiger/Beck_KG_V1_present_0p0083.tif")

# Define lat / lon projection.
lat_lon <- CRS("+init=epsg:4326")

# set land cover classes
lc <- c("DN","DB")

# process land cover classes
pixel_locations <- lapply(lc, function(cl){
  
  # load tif file
  r <- raster::raster(sprintf("data/land_cover/IGBP_%s_2010_2019_eroded.tif",cl))
  
  # export all DB pixel locations and save as RDS
  if(cl == "DB"){
    rr <- raster::rasterToPoints(r, fun=function(x){x==4})
  } else {
    rr <- raster::rasterToPoints(r, fun=function(x){x==3})
  }
  
  # only select x and y locations
  rr <- as.data.frame(rr[,1:2])
  colnames(rr) <- c("lon","lat")
  
  # Read in the coordinates and assign them a projection,
  # otherwise they remain just 'numbers'
  location <- sp::SpatialPoints(rr, lat_lon)
  
  # extract koeppen-geiger data
  rr$kg_class <- unlist(raster::extract(kg, location))
  rr$igbp <- cl
  
  return(rr)
})

# bind data frames
pixel_locations <- do.call("rbind", pixel_locations)

# save to disk
saveRDS(pixel_locations, "data/pixel_locations.rds")