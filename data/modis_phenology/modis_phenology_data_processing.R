# modis phenology data processing

# read in library
library(MODISTools)
library(dplyr)

# list all days (as DOY)
# between Jan 1 1970 and now
doy <- as.numeric(format(seq(as.Date("1970-01-01"),Sys.Date(), "days"),"%j"))
date <- seq(as.Date("1970-01-01"),Sys.Date(), "days")

# download data for
# Harvard Forest
subset <- mt_subset(
  product = "MCD12Q2",
  lat = 42.5378,
  lon = -72.1715,
  band = c("Greenup.Num_Modes_01",
           "Greenup.Num_Modes_02"),
  start = "2001-01-01",
  end = "2018-01-01",
  km_lr = 0,
  km_ab = 0,
  site_name = "testsite",
  internal = TRUE,
  progress = FALSE)

subset <- subset %>%
  filter(value <= 32766) %>% # drop fill values (not relevant for Northern Hemisphere)
  mutate(
    doy = doy[value], # substitute locations for doy values
    date = date[value] # if you want an absolute date
  )

# print 
print(subset)  
