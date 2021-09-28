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
source("R/process_pmodel.R")

# read sites data frame, in this case
# the list of all MODIS
df <- data.table::fread("~/data/pep/processed/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(
    lon = LON,
    lat = LAT,
    sitename = timeseries) %>%
  dplyr::select(
    lon, lat, sitename 
  ) %>%
  unique() %>%
  mutate(
    year_start = 1948,
    year_end = 2018
  )

# read etopo data
if(grepl('eu-', Sys.info()['nodename'])){
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

df_sites_sub <- df[1:2,]

message("Processing pixels:")
message(nrow(df_sites_sub))

# process data
df_pmodel <- format_drivers(
    df_sites_sub,
    bias_correction = TRUE,
    verbose = TRUE,
    run_model = args[3]
    )

print(head(df_model))

