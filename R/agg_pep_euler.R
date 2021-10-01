
library(tidyverse)
library(dplyr)
library(lubridate)

#setwd("/cluster/work/climate/bestocke/data/pep_pmodel_output/pep725")
setwd("/cluster/work/climate/bestocke/data/pep_pmodel_output/modis")

mod <- list.files(pattern = "*.rds") %>%
  map(readRDS) %>% 
  bind_rows()

doy_cutoff <- lubridate::yday("2001-06-21") 

df_out <- mod %>%
  mutate(gpp   = ifelse(doy >= doy_cutoff, 0, gpp),
         apar  = ifelse(doy >= doy_cutoff, 0, apar),
         alpha = ifelse(doy >= doy_cutoff, NA, alpha),
         rd    = ifelse(doy >= doy_cutoff, 0, rd))

#df_out <- mod %>%
#  mutate(
#    gpp = ifelse(daylength < 12, 0, gpp),
#    apar = ifelse(daylength < 12, 0, apar),
#    alpha = ifelse(daylength < 12, NA, alpha),
#    rd = ifelse(daylength < 12, 0, rd)
#  )

df_out <- df_out %>% 
  group_by(sitename, lat, lon, year) %>% 
  summarise(
    gpp = sum(gpp),
    rd = sum(rd),
    apar = sum(apar),
    alpha = mean(alpha, na.rm = TRUE)
  )

#saveRDS(df_out, "/cluster/work/climate/bestocke/data/pep_pmodel_output/pep725/pep725_pmodel_output_21JunCompleted.rds")
saveRDS(df_out, "/cluster/work/climate/bestocke/data/pep_pmodel_output/modis/modis_pmodel_output_21JunCompleted.rds")

