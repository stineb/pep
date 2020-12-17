#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(purrr)
library(tidyr)
library(magrittr)
library(multidplyr)
library(rlang)
library(lubridate)
library(ingestr)
library(readr)
library(rsofun)

source("R/run_pmodel_pep.R")

load("data/df_zani_corrected.RData")
load("data/df_mine.RData")

# df_mine <- df_mine %>% 
#   mutate(id_species_site = paste0(s_id, "_", species))
# 
# ## test - ok!
# tmp <- df_mine %>% 
#   dplyr::select(s_id, species, id_species_site, year, on, off) %>% 
#   left_join(
#     df_zani %>% 
#       dplyr::select(s_id, species, id_species_site, year, on, off),
#     by = c("id_species_site", "year")
#   )

load("data/ddf_watch_zani.RData")
load("data/ddf_cru_zani.RData")
load("data/df_zani_cells.RData")

load("data/df_mine_cells.RData")
load("data/ddf_watch_mine.RData")
load("data/ddf_cru_mine.RData")

## get forcing data into format
ddf_meteo_zani <- ddf_watch_zani %>% 
  tidyr::unnest(data) %>% 
  left_join(
    ddf_cru_zani %>% 
      tidyr::unnest(data),
    by = c("sitename", "date")
  ) %>% 
  group_by(sitename) %>% 
  tidyr::nest()

ddf_meteo_mine <- ddf_watch_mine %>% 
  tidyr::unnest(data) %>% 
  left_join(
    ddf_cru_mine %>% 
      tidyr::unnest(data),
    by = c("sitename", "date")
  ) %>% 
  group_by(sitename) %>% 
  tidyr::nest()

df_co2 <- read_csv("~/data/co2/cCO2_rcp85_const850-1765.csv")


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

##----------------------------------------
## MINE
##----------------------------------------
if (ncores > 1){

  ## set up cluster
  cl <- multidplyr::new_cluster(ncores) %>%
    multidplyr::cluster_library(c("dplyr", "purrr", "tidyr", "dplyr", "magrittr", "rlang", "rsofun", "ingestr", "geosphere")) %>%
    multidplyr::cluster_assign(run_pmodel_pep = run_pmodel_pep) %>%
    multidplyr::cluster_assign(gen_fapar_tseries = gen_fapar_tseries) %>%
    multidplyr::cluster_assign(ddf_meteo_mine = ddf_meteo_mine) %>%
    multidplyr::cluster_assign(df_co2 = df_co2) %>%
    multidplyr::cluster_assign(df_mine_cells = df_mine_cells) %>%
    multidplyr::cluster_assign(params_siml = params_siml) %>%
    multidplyr::cluster_assign(params_modl = params_modl) %>%
    multidplyr::cluster_assign(df_soiltexture = df_soiltexture)

  df_pmodel_mine <- df_mine %>% 
  
    ## nest by site-species (timeseries)
    group_by(id_species_site, lon_mid, lat_mid) %>% 
    nest() %>% 
    
    ## add id_cell column
    left_join(df_mine_cells %>% 
                dplyr::select(-lon, -lat),
              by = c("lon_mid", "lat_mid")) %>% 
    ungroup() %>% 

    ## partition to cores    
    multidplyr::partition(cl) %>%

    ## run p-model
    mutate(out_pmodel = purrr::map2(data, sitename, 
                                    ~run_pmodel_pep(
                                      .x,
                                      ddf_meteo_mine %>% 
                                        dplyr::filter(sitename == .y) %>% 
                                        unnest(data),
                                      df_co2,
                                      df_mine_cells %>% 
                                        dplyr::filter(sitename == .y),
                                      params_siml, 
                                      params_modl, 
                                      df_soiltexture 
                                    ))) %>% 
    collect() %>% 
    dplyr::select(-data)

} else {

  df_pmodel_mine <- df_mine %>% 
    
    ## nest by site-species (timeseries)
    group_by(id_species_site, lon_mid, lat_mid) %>% 
    nest() %>% 
    
    ## add id_cell column
    left_join(df_mine_cells %>% 
                dplyr::select(-lon, -lat),
              by = c("lon_mid", "lat_mid")) %>% 
    ungroup() %>% 
    
    slice(1) %>% 
    
    ## run p-model
    mutate(out_pmodel = purrr::map2(data, sitename, 
                                    ~run_pmodel_pep(
                                      .x,
                                      ddf_meteo_mine %>% 
                                        dplyr::filter(sitename == .y) %>% 
                                        unnest(data),
                                      df_co2,
                                      df_mine_cells %>% 
                                        dplyr::filter(sitename == .y),
                                      params_siml, 
                                      params_modl, 
                                      df_soiltexture 
                                    ))) %>% 
    dplyr::select(-data)
  
}

save(df_pmodel_mine, file = "data/df_pmodel_mine.RData")

