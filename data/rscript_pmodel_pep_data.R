#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)

# set output path
path <- "~/data/pep_pmodel_output"

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
library(rsofun)
library(rbeni)
library(raster)
library(sf)
library(data.table)

source("R/format_drivers.R")
source("R/process_pmodel_modis.R")

# read sites data frame, in this case
# the list of all MODIS
df <- data.table::fread("~/data/pep/processed/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(
    lon = LON,
    lat = LAT,
    sitename = timeseries) %>%
  select(
    lon, lat, sitename 
  ) %>%
  unique() %>%
  mutate(
    year_start = 1979,
    year_end = 2014
  )

# read etopo data
if(!grepl('eu-', Sys.info()['nodename'])){
  r <- raster::raster("~/Desktop/ETOPO1_Bed_g_geotiff.tif")
} else {
  r <- raster::raster("~/data/etopo/ETOPO1_Bed_g_geotiff.tif")
}

# grab elev data
s <- st_as_sf(
  df,
  coords = c("lon","lat"),
  crs = 4326)
df$elv <- raster::extract(r, s)
df <- df %>%
  mutate(
    elv = ifelse(elv < 0, 0, elv)
  )

# chunk data for processing
df_sites <- df %>%
  dplyr::select(sitename, lat, lon, year_start, year_end, elv) %>%
  mutate(idx = 1:n()) %>%
  mutate(chunk = rep(1:as.integer(args[2]),
                     each = (nrow(.)/as.integer(args[2])), len = nrow(.)))

# split sites data frame into (almost) equal chunks
list_df_split <- df_sites %>%
  group_by(chunk) %>%
  group_split()

df_sites_sub <- list_df_split[[as.integer(args[1])]] %>%
  dplyr::select(-c(chunk, idx))

message("Processing pixels:")
message(nrow(df_sites_sub))

# process data
df_pmodel <- format_drivers(
    df_sites_sub,
    bias_correction = TRUE,
    verbose = TRUE,
    run_model = args[3]
    )

# neither rowwise or apply()
# retain the tibble class which
# fucks up model evaluation in rsofun
# so a simple for loop it is

if(args[3]){
  filename <- file.path(path, paste0("pmodel_output_",args[1],".rds"))
  saveRDS(df_pmodel, filename)
} else {
  filename <- file.path(path, paste0("pmodel_drivers_",args[1],".rds"))
  saveRDS(df_pmodel, filename)
}
