library(MODISTools)
library(tidyverse)
library(lubridate)
library(ingestr)

# load sample
sampled_pixels <- readRDS("data/sampled_pixels.rds")
sampled_pixels$sitename <- paste0("testf",rownames(sampled_pixels)) 
sampled_pixels <- sampled_pixels %>%
  relocate(sitename)

sampled_pixels <- sampled_pixels %>%
  mutate(
    date_start = "2001-01-01",
    date_end = "2018-01-01"
  ) %>%
  as_tibble()

if(file.exists("data/modis_phenology/data_modisEVI_pheno.rds")){
  
  # load existing modis phenology data
  data_modisEVI_pheno <- readRDS("data/modis_phenology/data_modisEVI_pheno.rds")

} else {
    
  # download modis phenology data
  
  products <- mt_products()
  
  # List available bands for a product
  bands <- mt_bands(product = "MCD12Q2")
  
  data_modisEVI_pheno <- data.frame()
  
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
    
    data_modisEVI_pheno <- rbind(data_modisEVI_pheno, df_modis_pheno_sub)
  }
  
  # save data to file
  saveRDS(data_modisEVI_pheno, "data/modis_phenology/data_modisEVI_pheno.rds")
}

# list all days (as DOY) between Jan 1 1970 and now
dates <- seq(as.Date("1970-01-01"),Sys.Date(), "days")
doy <- as.numeric(format(seq(as.Date("1970-01-01"),Sys.Date(), "days"),"%j"))

# Convert value output to DOY
modis_phenology <- data_modisEVI_pheno %>%
  mutate(year=year(date)) %>%
  dplyr::filter(Greenup.Num_Modes_01 <= 32766) %>% 
  # drop fill values (not relevant for Northern Hemisphere)
  dplyr::filter(MidGreendown.Num_Modes_01 <= 32766) %>% 
  mutate(
    SOS_date = dates[Greenup.Num_Modes_01],
    SOS_doy = doy[Greenup.Num_Modes_01],
    SOS_date2 = as_date(Greenup.Num_Modes_01),
    SOS_doy2 = yday(SOS_date2)
    ) %>%
  mutate(
    EOS_date = dates[MidGreendown.Num_Modes_01],
    EOS_doy = doy[MidGreendown.Num_Modes_01],
    EOS_date2 = as_date(MidGreendown.Num_Modes_01),
    EOS_doy2 = yday(EOS_date2)
    ) 

modis_pheno_sites <- sampled_pixels[,1:3] %>%
  left_join(modis_phenology) %>%
  relocate(year,.after=date) 

# save data
saveRDS(modis_pheno_sites, "data/modis_phenology/modis_pheno_sites.rds")
