# Random pixel selection
library(raster)
library(spatstat)

# load tif file
r <- raster("data/land_cover/IGBP_DB_2010_2019_eroded.tif")

# export all DB pixel locations and save as RDS
rr <- rasterToPoints(r, fun=function(x){x==4})
rr <- rr[,1:2]
saveRDS(rr, "data/locations.rds")

# pseudo random locations
loc <- sample(1:nrow(rr), size = 8000)
ss <- rr[loc,]

# write subset to disk
saveRDS(ss, "data/location_selection.rds")
