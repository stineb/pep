library(MODISTools)
library(tidyverse)
library(lubridate)
library(ingestr)

# load sample
sampled_pixels <- readRDS("~/pep/data/modis_phenology/sampled_pixels.rds")
sampled_pixels$sitename <- paste0("testf",rownames(sampled_pixels)) 
sampled_pixels <- sampled_pixels %>%
  relocate(sitename) %>%
  mutate(
    date_start = "2001-01-01",
    date_end = "2018-01-01"
  ) %>%
  as_tibble()

if(file.exists("~/pep/data/modis_phenology/data_modis_phenol.rds")){
  
  # load existing modis phenology data
  data_modis_phenol <- readRDS("~/pep/data/modis_phenology/data_modis_phenol.rds")

} else {
    
  # download modis phenology data
  
  products <- mt_products()
  
  # List available bands for a product
  bands <- mt_bands(product = "MCD12Q2")
  
  data_modis_phenol <- data.frame()
  
  # NOTE: this should be a lapply() or a do() call
  # in a dplyr routine
  for(i in 1:nrow(sampled_pixels)) {
    
    df_modis_pheno_sub <- ingest_bysite(
      sitename  = sampled_pixels$sitename[i],
      source    = "modis",
      year_start= 2001,
      year_end  = 2018,
      lon       = sampled_pixels$lon[i],
      lat       = sampled_pixels$lat[i],
      settings  = settings_modis,
      verbose   = FALSE
    )
    
    df_modis_pheno_sub <- df_modis_pheno_sub %>%
      tidyr::drop_na()
    
    data_modis_phenol <- rbind(data_modis_phenol, df_modis_pheno_sub)
  }
  
  # save data to file
  saveRDS(data_modis_phenol, "~/pep/data/modis_phenology/data_modis_phenol.rds")
}

# Convert value output to DOY
modis_phenology <- data_modis_phenol %>%
  mutate(year=year(date)) %>%
  # drop fill values (not relevant for Northern Hemisphere)
  dplyr::filter(Greenup.Num_Modes_01 <= 32766) %>% 
  dplyr::filter(Dormancy.Num_Modes_01 <= 32766) %>% 
  dplyr::filter(MidGreenup.Num_Modes_01 <= 32766) %>% 
  dplyr::filter(MidGreendown.Num_Modes_01 <= 32766) %>% 
  mutate(
    SOS_1_date = as_date(Greenup.Num_Modes_01), 
    SOS_1_doy = yday(SOS_1_date)
    ) %>%
  mutate(
    SOS_2_date = as_date(MidGreenup.Num_Modes_01), 
    SOS_2_doy = yday(SOS_2_date)
    ) %>% 
  mutate(
    EOS_1_date = as_date(Dormancy.Num_Modes_01), 
    EOS_1_doy = yday(EOS_1_date)
  ) %>%
  mutate(
    EOS_2_date = as_date(MidGreendown.Num_Modes_01), 
    EOS_2_doy = yday(EOS_2_date)
  ) 

modis_pheno_sites <- sampled_pixels[,1:3] %>%
  left_join(modis_phenology) %>%
  relocate(year,.after=date) 

# Filter points in the Northern Hemisphere, i.e., latitude > 0
modis_pheno_sites <- modis_pheno_sites %>% filter(lat > 0)

# save data
saveRDS(modis_pheno_sites, "~/pep/data/modis_phenology/modis_pheno_sites.rds")
