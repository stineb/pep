
library(tidyverse)
library(dplyr)

#setwd("/cluster/work/climate/bestocke/data/pep_pmodel_output/pep725")
setwd("/cluster/work/climate/bestocke/data/pep_pmodel_output/modis")

mod <- list.files(pattern = "*.rds") %>%
  map(readRDS) %>% 
  bind_rows()

#doy_cutoff <- lubridate::yday("2001-09-23") # daylight hours fall below 12 (not 11 hours,"2001-06-21").

df_out <- mod %>%
  mutate(
    gpp = ifelse(daylength < 12, 0, gpp),
    apar = ifelse(daylength < 12, 0, apar),
    alpha = ifelse(daylength < 12, NA, alpha),
    rd = ifelse(daylength < 12, 0, rd)
  )

df_out <- df_out %>% 
  group_by(sitename, lat, lon, year) %>% 
  summarise(
    gpp = sum(gpp),
    rd = sum(rd),
    apar = sum(apar),
    alpha = mean(alpha, na.rm = TRUE)
  )

saveRDS(df_out, "/cluster/work/climate/bestocke/data/pep_pmodel_output/modis/modis_pmodel_output.rds")

