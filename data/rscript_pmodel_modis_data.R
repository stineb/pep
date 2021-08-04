#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly=TRUE)
path <- "data/pmodel/sites"

# load libraries and
# scripts
library(tidyverse)
library(ingestr)
library(rsofun)
library(rbeni)

source("R/format_drivers.R")
source("R/process_pmodel_data.R")
source("R/calc_climate_index.R")

# read sites data frame, in this case
# the list of all global HWSD locations
# at .5 degree
file <- list.files("data-raw/np_dbase/","*.csv", full.names = TRUE)

# check
if(length(file) > 1){
  stop("Leaf NP site directory contains multiple site files, only one
       file may be present!")
}

df_sites <- read.table(
  file,
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE) %>%
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

message("Processing cells:")
message(nrow(df_sites_sub))

# process data
df_pmodel <- format_drivers(
    df_sites_sub,
    bias_correction = TRUE,
    verbose = TRUE,
    run_model = args[3])

# neither rowwise or apply()
# retain the tibble class which
# fucks up model evaluation in rsofun
# so a simple for loop it is

if(args[3]){
  filename <- file.path(path, paste0("pmodel_output_",args[1],".rds"))
  saveRDS(df_pmodel, filename)
} else {
  for (i in 1:nrow(df_pmodel)){
    # write cell / file to disk
    filename <- file.path(path, paste0(df_pmodel$sitename[i],".rds"))
    saveRDS(df_pmodel[i,], filename)
  }  
}
