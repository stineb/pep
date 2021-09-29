#' Run pmodel and combine results
#' 
#' Runs pmodel with PEP specific data formatting
#' and returns aggregated results to limit output
#' data volumes
#'
#' @param df_pheno phenology of a site
#' @param df_forcing forcing data
#' @param df_co2 co2 data
#' @param df_siteinfo siteinfo
#' @param params_siml simulation parameters
#' @param params_modl model settings
#' @param df_soiltexture soil texture data
#' @param agg 

run_pmodel_pep <- function(
  df_pheno,
  df_forcing,
  df_co2,
  df_siteinfo,
  params_siml,
  params_modl,
  df_soiltexture,
  agg = TRUE){
  
  ## read from file containing full tseries model output of this site
  filnam <- paste0("data/df_pmodel_", df_siteinfo$sitename[1], ".csv")
  
  if (file.exists(filnam)){
    
    mod <- read_csv(filnam)
    
  } else {
   
    ## generate fapar time series for this site-species and each year
    useyears <- min(df_pheno$year):max(df_pheno$year)
    df_fapar <- map_dfr(as.list(useyears), ~gen_fapar_tseries(df_pheno, .))
    
    ## get mean seasonality in forcing to be used for all years before 1979
    df_forcing_meandoy <- df_forcing %>% 
      ungroup() %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      group_by(doy) %>% 
      summarise_if(is.numeric, mean, na.rm = TRUE)
    
    ## construct forcing data frame
    df_forcing <- df_fapar %>% 
      
      ## combine with fapar df to get the full time series (now includes NA values)
      left_join(df_forcing, by = "date") %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      left_join(df_forcing_meandoy %>% 
                  rename(temp_doy = temp, patm_doy = patm, qair_doy = qair, vpd_doy = vpd,
                         ppfd_doy = ppfd, rain_doy = rain, snow_doy = snow, prec_doy = prec, 
                         ccov_doy = ccov),
                by = "doy") %>% 
      
      ## fill missing values with mean seasonality
      rowwise() %>% 
      mutate(temp = ifelse(is.na(temp), temp_doy, temp),
             vpd  = ifelse(is.na(vpd),  vpd_doy,  vpd),
             patm = ifelse(is.na(patm), patm_doy, patm),
             ppfd = ifelse(is.na(ppfd), ppfd_doy, ppfd),
             snow = ifelse(is.na(snow), snow_doy, snow),
             rain = ifelse(is.na(rain), rain_doy, rain),
             prec = ifelse(is.na(prec), prec_doy, prec),
             ccov = ifelse(is.na(ccov), ccov_doy, ccov)
      ) %>% 
      
      ## fill previously not used - necessary after rsofun update
      mutate(tmin = temp,
             tmax = temp) %>% 
      
      ## merge co2 data in it
      mutate(year = lubridate::year(date)) %>% 
      left_join(df_co2, by = "year") %>% 
      
      ## remove days in leap years
      dplyr::filter(!(month(date)==2 & mday(date) == 29))
    
    ## complement
    df_siteinfo$year_start <- min(useyears)
    df_siteinfo$year_end <- max(useyears)
    params_siml$nyeartrend <- df_siteinfo$year_end - df_siteinfo$year_start + 1
    params_siml$firstyeartrend <- df_siteinfo$year_start
    df_siteinfo$elv <- unique(df_pheno$elv)[1]
    df_siteinfo$whc <- 200
    
    ## run pmodel
    mod <- run_pmodel_f_bysite( 
      df_siteinfo$sitename[1], 
      params_siml, 
      df_siteinfo,
      df_forcing, 
      df_soiltexture, 
      params_modl = params_modl, 
      makecheck = TRUE 
    ) 
    
    write_csv(mod, file = filnam)
  }
  
  # # determine DOY when daylength falls below 11 h. 
  # # THIS WORKS ONLY FOR THE NORTHERN HEMISPHERE!
  # vec_dayl <- geosphere::daylength(df_siteinfo$lat, 1:365)
  # vec_dayl[1:lubridate::yday("2001-06-21")] <- 9999 # first half of the year
  # doy_cutoff <- min(which(vec_dayl < 11))
  
  ## try alternative: only up to summer solstice 21 june
  doy_cutoff <- lubridate::yday("2001-06-21")
  
  df_out <- mod %>% 
    dplyr::select(date, gpp, vcmax, aet = transp, pet) %>% 
    left_join(df_forcing %>% dplyr::select(date, ppfd, fapar),
              by = "date") %>% 
    rowwise() %>% 
    mutate(ppfd = ppfd * 60 * 60 * 24,
           year = lubridate::year(date),
           alpha = aet/pet,
           apar = fapar * ppfd,
           rd = 0.015 * vcmax * 60 * 60 * 24 * 12.0107) %>% 
    dplyr::select(-aet, -pet, -ppfd) %>% 
    mutate(doy = lubridate::yday(date)) %>% 
    rowwise() %>% 
    
    ## don't accumulate after daylength falls below 11 h
    mutate(gpp = ifelse(doy >= doy_cutoff, 0, gpp),
           apar = ifelse(doy >= doy_cutoff, 0, apar),
           alpha = ifelse(doy >= doy_cutoff, NA, alpha),
           rd = ifelse(doy >= doy_cutoff, 0, rd))
  
  if (agg){
    ## take sum/mean  
    df_out <- df_out %>% 
      group_by(year) %>% 
      summarise(gpp = sum(gpp),
                rd = sum(rd),
                apar = sum(apar),
                alpha = mean(alpha, na.rm = TRUE))
  }
  
  return(df_out)
  
}

gen_fapar_tseries <- function(df, useyear){
  ddf <- df %>% dplyr::filter(year == useyear)
  if (nrow(ddf) == 0){
    out <- init_dates_dataframe(yrstart = useyear, yrend = useyear) %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      rowwise() %>% 
      mutate(fapar = ifelse(doy >= mean(df$on) & doy < mean(df$off), 1.0, 0.0)) %>% 
      dplyr::select(-doy) %>% 
      ungroup()
  } else {
    out <- init_dates_dataframe(yrstart = useyear, yrend = useyear) %>% 
      mutate(doy = lubridate::yday(date)) %>% 
      rowwise() %>% 
      mutate(fapar = ifelse(doy >= ddf$on & doy < ddf$off, 1.0, 0.0)) %>% 
      dplyr::select(-doy) %>% 
      ungroup()
  }
  return(out)
}
