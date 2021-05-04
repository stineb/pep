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
test <- rr[loc,]

# grab Koeppen-geiger values
plot(test[,1],test[,2], xlim = c(10, 20),ylim=c(40,50))



