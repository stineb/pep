## This code is copied directly from DataMeta_1_PhenoData_Cleaning.R

clean_zani <- function(pheno_out.df, pheno_off.df, ts_common){
  
  # Create data.frame to store output
  pheno_clean.df <- data.frame()
  
  for(ts in ts_common){
    
    # Subset per timeseries
    pheno_out.sub <- pheno_out.df[pheno_out.df$timeseries==ts, ]
    pheno_off.sub <- pheno_off.df[pheno_off.df$timeseries==ts, ]
    
    # Eliminate duplicate phenological observations per year
    pheno_out.sub <- pheno_out.sub %>% 
      distinct(year, .keep_all = TRUE)
    pheno_off.sub <- pheno_off.sub %>% 
      distinct(year, .keep_all = TRUE)
    
    # Remove observations with MAD_ratio >=3
    pheno_out.sub <- pheno_out.sub %>% 
      mutate(MAD_ratio = madratio_fx(.$day)) %>% 
      filter(MAD_ratio<3)
    pheno_off.sub <- pheno_off.sub %>% 
      mutate(MAD_ratio = madratio_fx(.$day)) %>% 
      filter(MAD_ratio<3)
    
    # Keep only common observations (i.e. for the same years)
    yr_common <- intersect(pheno_out.sub$year, pheno_off.sub$year)
    pheno_out.sub <- pheno_out.sub[pheno_out.sub$year %in% yr_common,]
    pheno_off.sub <- pheno_off.sub[pheno_off.sub$year %in% yr_common,]
    
    # Error check
    if(nrow(pheno_out.sub)!=nrow(pheno_off.sub)) {
      print("ERROR! Unequal timeseries length")
      break
    }
    
    # Keep timeseries with more than 15 years of common observations
    if(nrow(pheno_out.sub)>=15) {
      
      # Calculate mean and standard deviation
      pheno_out.sub$mean_DoY <- mean(pheno_out.sub$day)
      pheno_out.sub$SD_DoY <- sd(pheno_out.sub$day)
      pheno_off.sub$mean_DoY <- mean(pheno_off.sub$day)
      pheno_off.sub$SD_DoY <- sd(pheno_off.sub$day)
      
      # Remove timeseries with standard deviation >15 for leaf flushing 
      # and >20 for leaf senescence
      if(sd(pheno_out.sub$day)<15 && sd(pheno_off.sub$day)<20) {
        
        # Timeseries length
        pheno_clean.sub <- rbind(pheno_out.sub,pheno_off.sub)
        pheno_clean.sub$timeseries_length <- nrow(pheno_clean.sub)
        
        # Bind to output dataset
        pheno_clean.df <- rbind(pheno_clean.df,pheno_clean.sub)
        print(paste0("Timeseries ",ts," included"))
      } else {
        print(paste0("Timeseries ",ts," excluded"))
      }
    } else {
      print(paste0("Timeseries ",ts," excluded"))
    }
  }
  
  ##----------------------------------------
  # Data wrangling 
  pheno_clean.df <- pheno_clean.df %>% 
    rename(DoY = day) %>% 
    select(timeseries, everything()) %>% 
    arrange(timeseries,year)
  
  
  ##----------------------------------------
  # Summary of phenological observations
  
  # Observations count (per species) 
  (nrow(pheno_clean.df)/2)
  pheno_clean.df %>% 
    group_by(species) %>% 
    tally(name="Observations") %>% 
    mutate(Observations=Observations/2)
  
  # Timeseries count
  (length(unique(pheno_clean.df$timeseries)))
  
  # Sites count (per species)
  (length(unique(pheno_clean.df$s_id)))
  pheno_clean.df %>% 
    count(timeseries,species) %>% 
    count(species,name="Sites")
  
  
  ##----------------------------------------
  # Export dataset
  # write.table(pheno_clean.df, "DataMeta_2_PhenologyObs_PEP725_CleanData.csv", sep=";", row.names = FALSE)
  
  return(pheno_clean.df)
}

madratio_fx <- function(x) {
  abs(median(x)-x) / mad(x)
}