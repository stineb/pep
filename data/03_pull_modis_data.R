library(MODISTools)
library(tidyverse)
library(lubridate)
library(ingestr)

## List available MODIS products
products <- mt_products()

## List available bands for a product
bands <- mt_bands(product = "MCD12Q2")

# load sample
sampled_pixels <- readRDS("~/pep/data/sampled_pixels.rds")
sampled_pixels$sitename <- paste0("testf",rownames(sampled_pixels)) 
sampled_pixels <- sampled_pixels %>% relocate(sitename) 
sampled_pixels <- sampled_pixels %>% mutate(date_start = "2001-01-01", date_end = "2018-01-01")
sampled_pixels <- as_tibble(sampled_pixels)

data_modisEVI_pheno <- data.frame() 

for(i in 1:nrow(sampled_pixels)) { # nrow(sampled_pixels)
  
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
  df_modis_pheno_sub <- df_modis_pheno_sub %>% tidyr::drop_na()
  
  data_modisEVI_pheno <- rbind(data_modisEVI_pheno, df_modis_pheno_sub)
}

saveRDS(data_modisEVI_pheno, "~/pep/data/data_modisEVI_pheno.rds")
data_modisEVI_pheno <- readRDS("~/pep/data/data_modisEVI_pheno.rds")

# list all days (as DOY) between Jan 1 1970 and now
dates <- seq(as.Date("1970-01-01"),Sys.Date(), "days")
doy <- as.numeric(format(seq(as.Date("1970-01-01"),Sys.Date(), "days"),"%j"))

# Convert value output to DOY
data_modisEVI_pheno <- data_modisEVI_pheno %>%
  mutate(year=year(date)) %>%
  dplyr::filter(Greenup.Num_Modes_01 <= 32766) %>% 
  dplyr::filter(MidGreendown.Num_Modes_01 <= 32766) %>% # drop fill values (not relevant for Northern Hemisphere)
  mutate(SOS_date = dates[Greenup.Num_Modes_01], SOS_doy = doy[Greenup.Num_Modes_01], SOS_date2 = as_date(Greenup.Num_Modes_01), SOS_doy2 = yday(SOS_date2)) %>%
  mutate(EOS_date = dates[MidGreendown.Num_Modes_01], EOS_doy = doy[MidGreendown.Num_Modes_01], EOS_date2 = as_date(MidGreendown.Num_Modes_01), EOS_doy2 = yday(EOS_date2)) 

modis_pheno_sites <- sampled_pixels[,1:3] %>% left_join(data_modisEVI_pheno) %>% relocate(year,.after=date) 
saveRDS(modis_pheno_sites, "~/pep/data/modis_pheno_sites.rds")


