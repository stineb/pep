#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rlang)
library(lubridate)

source("R/run_pmodel_pep.R")

load("data/ddf_meteo.RData")
load("data/df_pheno.RData")
load("data/df_co2.RData")
load("data/df_sites.RData")

## define model parameters
params_siml <- list(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = TRUE,
  tempstress         = TRUE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
  )

## calibrated parameters for v3.0 (see https://rpubs.com/stineb/rsofun_benchmark_v30)
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  vpdstress_par_a = 9999,
  vpdstress_par_b = 9999,
  vpdstress_par_m = 9999
  )

df_soiltexture <- bind_rows(
  top    = tibble(layer = "top",    fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1),
  bottom = tibble(layer = "bottom", fsand = 0.4, fclay = 0.3, forg = 0.1, fgravel = 0.1)
)

## get all available cores
ncores <- parallel::detectCores()


# ##------------------------------------------------------------------------
# ## 1. extract return levels only and write to (small) files
# ##------------------------------------------------------------------------
# nchunk <- as.integer(args[2]) # 1000  # make sure this is consistent with the number of parallel jobs (job array!) in the submission script
# nsims <- nrow(df)
# nsims_chunk <- ceiling(nsims/nchunk)
# isim <- seq(1:nsims)
# irow_chunk <- split(isim, ceiling(seq_along(isim)/nsims_chunk))

# print("running pmodel for row indices:")
# print(irow_chunk[[as.integer(args[1])]]) 

if (ncores > 1){

  ## set up cluster
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rlang", "rsofun")) %>%
    multidplyr::cluster_assign(run_pmodel_pep = run_pmodel_pep) %>%
    multidplyr::cluster_assign(ddf_meteo = ddf_meteo) %>%
    multidplyr::cluster_assign(df_co2 = df_co2) %>%
    multidplyr::cluster_assign(df_sites = df_sites) %>%
    multidplyr::cluster_assign(params_siml = params_siml) %>%
    multidplyr::cluster_assign(params_modl = params_modl) %>%
    multidplyr::cluster_assign(df_soiltexture = df_soiltexture)

  df_pmodel <- df %>% 
  
    ## nest by site-species (timeseries)
    group_by(id_species_site, lon_mid, lat_mid) %>% 
    nest() %>% 
    
    ## add id_cell column
    left_join(df_sites %>% 
                rename(id_cell = sitename),
              by = c("lon_mid", "lat_mid")) %>% 
    ungroup() %>% 

    ## partition to cores    
    multidplyr::partition(cl) %>%

    ## run p-model
    mutate(out_pmodel = purrr::map2(data, id_cell, 
                                    ~run_pmodel_pep(
                                      .x,
                                      ddf_meteo %>% 
                                        dplyr::filter(sitename == .y) %>% 
                                        unnest(data),
                                      df_co2,
                                      df_sites %>% 
                                        dplyr::filter(sitename == .y),
                                      params_siml, 
                                      params_modl, 
                                      df_soiltexture 
                                    ))) %>% 
    collect() %>% 
    dplyr::select(-data)

} else {

  df_pmodel <- df %>% 
  
    ## nest by site-species (timeseries)
    group_by(id_species_site, lon_mid, lat_mid) %>% 
    nest() %>% 
    
    ## add id_cell column
    left_join(df_sites %>% 
                rename(id_cell = sitename),
              by = c("lon_mid", "lat_mid")) %>% 
    ungroup() %>% 

    ## run p-model
    mutate(out_pmodel = purrr::map2(data, id_cell, 
                                    ~run_pmodel_pep(
                                      .x,
                                      ddf_meteo %>% 
                                        dplyr::filter(sitename == .y) %>% 
                                        unnest(data),
                                      df_co2,
                                      df_sites %>% 
                                        dplyr::filter(sitename == .y),
                                      params_siml, 
                                      params_modl, 
                                      df_soiltexture 
                                    ))) %>% 
    dplyr::select(-data)

}

save(df_pmodel, file = "data/df_pmodel.RData")
