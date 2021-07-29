###################################
## Model Optimization & Predictions

## Arguments from batch file
args <- c("allSpc", # c("FaSy", "QuRo", "AeHi", "BePe", "LaDe", "SoAu", "allSpc")
          "allMod") # c("CDD", "DM1", "DM2", "TPM", "SIAM", "TDM", "TPDM", "PIAgsi", "PIAmns", "PIApls", "allMod")

calibSpec <- data.frame(matrix(c("Fagus sylvatica", "Quercus robur", "Aesculus hippocastanum",
                                 "Betula pendula", "Larix decidua", "Sorbus aucuparia", "allSpc"),
                               nrow = 1))

colnames(calibSpec) <- c("FaSy", "QuRo", "AeHi", "BePe", "LaDe", "SoAu", "allSpc")

if("allSpc" %in% calibSpec) {
  calibSpec <- c("Fagus sylvatica", "Quercus robur", "Aesculus hippocastanum",
                 "Betula pendula", "Larix decidua", "Sorbus aucuparia")
} else {
  calibSpec <- unlist(calibSpec[, unlist(strsplit(args[1], "_"))])
}

#
calibModl <- data.frame(matrix(c("CDD.model", "DM1.model", "DM2.model", "TPM.model",
                                 "SIAM.model", "TDM.model", "TPDM.model", "PIAgsi.model",
                                 "PIAmns.model", "PIApls.model", "allMod"),
                               nrow = 1))

colnames(calibModl) <- c("CDD", "DM1", "DM2", "TPM", "SIAM", "TDM", "TPDM", "PIAgsi", "PIAmns", "PIApls", "allMod")

if("allMod" %in% calibModl) {
  calibModl <- c("CDD.model", "DM1.model", "DM2.model", "TPM.model", "SIAM.model",
                 "TDM.model", "TPDM.model", "PIAgsi.model", "PIAmns.model", "PIApls.model")
} else {
  calibModl <- unlist(calibModl[, unlist(strsplit(args[2], "_"))])
}


##----------------------------------------
## Import & Format data
library(data.table)

# Define data paths
Input  <- "../10_Input/"
Output <- "../20_Output/"

# Phenological observations
all.df <- fread(paste0(Input, "DataMeta_3_Drivers_selectedBBCH_CZ_20210316.csv"))

pheno.df <- copy(all.df[, .(timeseries, ts_yr, PEP_ID, LON, LAT, Species,
                            YEAR, DoY_off, BBCH_off, DoY_out, BBCH_out)])

# Predictors
preds.df <- copy(all.df[, .(timeseries, ts_yr, PEP_ID, LON, LAT, Species,
                            YEAR, DoY_out, BBCH_out, BBCH_off,
                            temp_GS, RD_summer, cGSI, cA_tot, cA_totw)])

# Minimum temperature and photoperiod
tmin.df  <- fread(paste0(Input, "Minimum Temperature.csv"))
photo.df <- fread(paste0(Input, "Photoperiod.csv"))

photo.df <- photo.df[, ts_yr := paste0(PEP_ID, "_", YEAR)]

#---
# Create input data lists per phenology stages (i.e. leaf colouring & leaf fall)
myDataList_perSpc <- NULL
myPredList_perSpc <- NULL

for (myBBCH in unique(pheno.df$BBCH_off)) {
  calibSpec_pBBCH <- calibSpec[calibSpec %in% unique(pheno.df[BBCH_off == myBBCH]$Species)]
  
  for(mySpc in calibSpec_pBBCH) {
    myPhe.DT <- unique(pheno.df[BBCH_off == myBBCH & Species == mySpc])
    myPre.DT <- unique(preds.df[BBCH_off == myBBCH & Species == mySpc])
    
    myTsYr <- Reduce(intersect, list(myPhe.DT$ts_yr,
                                     myPre.DT$ts_yr,
                                     tmin.df$ts_yr,
                                     photo.df$ts_yr))
    
    myPhe.DT <- myPhe.DT[ts_yr %in% myTsYr]
    myPre.DT <- myPre.DT[ts_yr %in% myTsYr]
    myTn.DT  <- tmin.df[ts_yr %in% myTsYr]
    myPho.DT <- photo.df[ts_yr %in% myTsYr]
    
    setorder(myPhe.DT, ts_yr)
    setorder(myPre.DT, ts_yr)
    setorder(myTn.DT, ts_yr)
    setorder(myPho.DT, ts_yr)
    
    myPhe.DT <- myPhe.DT[,
                         rID := .I
    ][,
      nSta := .N,
      by = .(PEP_ID)
    ]
    myPre.DT <- myPre.DT[,
                         rID := .I
    ][,
      nSta := .N,
      by = .(PEP_ID)
    ]
    
    myTmini <- t(as.matrix(myTn.DT[, -c("ts_yr", "YEAR", "PEP_ID", "LAT", "LON")]))
    myDOY <- try(!is.na(myTmini)) * c(1:366)
    myDOY[which(myDOY == 0)] <- NA
    
    #
    data = list("doy"              = myDOY,#as.vector(myPhe.DT$DoY_out),
                "site"             = as.vector(myPhe.DT$timeseries),
                "location"         = t(as.matrix(myPhe.DT[, .(LON, LAT)])),
                "year"             = as.vector(myPhe.DT$ts_yr),
                "Ti"               = NULL,
                "Tmini"            = myTmini, #t(as.matrix(myTn.DT[, -c("ts_yr", "YEAR", "PEP_ID", "LAT", "LON")])),
                "Tmaxi"            = NULL,
                "Li"               = t(as.matrix(myPho.DT[, -c("ts_yr", "lat_yr", "YEAR", "PEP_ID", "LAT")])),
                "SPEI"             = NULL,
                "VPDi"             = NULL,
                "transition_dates" = as.vector(myPhe.DT$DoY_off),
                "georeferencing"   = NULL)
    
    myDim <- as.character(c(1 : length(as.vector(myPhe.DT$DoY_out))))
    
    dimnames(data$location)[[2]] <- myDim
    dimnames(data$Tmini)[[2]]    <- myDim
    dimnames(data$Li)[[2]]       <- myDim
    
    myDataList_perSpc[[as.character(myBBCH)]][[mySpc]] <- data
    myPredList_perSpc[[as.character(myBBCH)]][[mySpc]] <- myPre.DT
    
    rm(myTmini, myDOY, data, myDim, myPhe.DT, myTsYr, myTn.DT, myPho.DT)
  }
  rm(mySpc)
}
rm(myBBCH, calibSpec_pBBCH)

cat("\n=================================\nInput data prepared.\nStarting calibration\n=================================\n=================================\n")


##----------------------------------------
## MODELS OF LEAF SENESCENCE (and drivers):

# Functions of autumn phenology models according Zani et al. (2020)

CDD.model = function(par, data){
  # exit the routine as some parameters are missing
  if (length(par) != 2){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the
  # par argument for readability
  T_base = par[1]
  F_crit = par[2]

  # create forcing/chilling rate vector
  Rf = data$Tmini - T_base
  Rf[Rf > 0] = 0

  # photoperiod-dependent start-date for chilling accumulation (t0)
  # t0 is defined as the first day when daily minimum temperature is lower than a temperature threshold (T_base)
  # after the date of the peak multiyear average daily minimum temperature, namely the 200th day of year
  t0 <- vector()
  for(c in 1:ncol(data$Tmini)) {
    interval = 1:366
    t0A = interval[which(data$Tmini[,c] < T_base)]
    ind1 = min(which(t0A > 200))
    t0A = t0A[ind1]
    t0 = c(t0,t0A)
  }
  # nullify values before the t0
  for(c in 1:ncol(data$Tmini)){
    Rf[1:t0[c],c] = 0
  }

  # predict date of leaf.off according to optimized F_crit
  doy = apply(Rf,2, function(xt){
    doy = which(cumsum(xt) <= F_crit)[1]
  })

  return(doy)
}

DM1.model = function(par, data){
  # exit the routine as some parameters are missing
  if (length(par) != 3){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the
  # par argument for readability
  T_base = par[1]
  P_base = par[2]
  F_crit = par[3]

  # create forcing/chilling rate vector at the day level
  Rf = (data$Tmini - T_base)*(data$Li/P_base) # lengthening photoperiod promoting leaf senescence
  Rf[Rf > 0] = 0

  # photoperiod-dependent start-date for chilling accumulation (t0)
  # t0 is defined as the first day when photoperiod is shorter than the photoperiod threshold (P_base)
  # after the date of the longest photoperiod (summer solstice), namely, the 173rd day of year
  t0 <- vector()
  for(c in 1:ncol(data$Tmini)) {
    interval = 1:366
    t0A = interval[which(data$Li[,c] < P_base)]
    ind1 = min(which(t0A > 173))
    t0A = t0A[ind1]
    t0 = c(t0,t0A)
  }

  # nullify values before the t0
  for(c in 1:ncol(data$Tmini)){
    Rf[1:t0[c],c] = 0 #nullify values before the date of leaf.out
  }

  # calculate the summation along the year (interval = 1:366) and derive the date of leaf.off
  # DOY of budburst criterium
  doy = apply(Rf,2, function(xt){
    doy = which(cumsum(xt) <= F_crit)[1]
  })

  return(doy)
}

DM2.model = function(par, data){
  # exit the routine as some parameters are missing
  if (length(par) != 3){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the
  # par argument for readability
  T_base = par[1]
  P_base = par[2]
  F_crit = par[3]

  # create forcing/chilling rate vector at the day level
  Rf = (data$Tmini - T_base)*(1-(data$Li/P_base)) # shortening photoperiod promoting leaf senescence
  Rf[Rf > 0] = 0

  # photoperiod-dependent start-date for chilling accumulation (t0)
  # t0 is defined as the first day when photoperiod is shorter than the photoperiod threshold (P_base)
  # after the date of the longest photoperiod (summer solstice), namely, the 173rd day of year
  t0 <- vector()
  for(c in 1:ncol(data$Tmini)) {
    interval = 1:366
    t0A = interval[which(data$Li[,c] < P_base)]
    ind1 = min(which(t0A > 173))
    t0A = t0A[ind1]
    t0 = c(t0,t0A)
  }

  # nullify values before the t0
  for(c in 1:ncol(data$Tmini)){
    Rf[1:t0[c],c] = 0 #nullify values before the date of leaf.out
  }

  # calculate the summation along the year (interval = 1:366) and derive the date of leaf.off
  # DOY of budburst criterium
  doy = apply(Rf,2, function(xt){
    doy = which(cumsum(xt) <= F_crit)[1]
  })

  return(doy)
}

TPM.model = function(par, data){
  # exit the routine as some parameters are missing
  if (length(par) != 4){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the par argument for readability
  P_base = par[1]
  a = par[2]
  b = par[3]
  F_crit = par[4]

  # create forcing/chilling rate vector at the day level
  Rf = 1/(1+exp(a*(data$Tmini*data$Li-b)))

  # photoperiod-dependent start-date for chilling accumulation (t0)
  # t0 is defined as the first day when photoperiod is shorter than the photoperiod threshold (P_base)
  # after the date of the longest photoperiod (summer solstice), namely, the 173rd day of year
  t0 <- vector()
  for(c in 1:ncol(data$Tmini)) {
    interval = 1:366
    t0A = interval[which(data$Li[,c] < P_base)]
    ind1 = min(which(t0A > 173))
    t0A = t0A[ind1]
    t0 = c(t0,t0A)
  }

  # nullify values before the t0
  for(c in 1:ncol(data$Tmini)){
    Rf[1:t0[c],c] = 0 #nullify values before the date of leaf.out
  }

  # calculate the summation along the year and derive the date of leaf.off
  # DOY of budburst criterium
  doy = apply(Rf,2, function(xt){
    doy = which(cumsum(xt) >= F_crit)[1]
  })

  return(doy)
}

SecondGen_PIA.models = function(par, predictor, data) {
  # exit the routine as some parameters are missing
  if (length(par) != 5 & length(par) != 6){
    stop("model parameter(s) out of range (too many, too few)")
  }

  # extract the parameter values from the
  # par argument for readability
  P_base = as.numeric(par[1])
  a = as.numeric(par[2])
  b = as.numeric(par[3])
  c = as.numeric(par[4])
  if(length(par)==5) {
    d = as.numeric(par[5])
    pred = predictor
  }
  if(length(par)==6) {
    d = as.numeric(par[5])
    e = as.numeric(par[6])
    pred1 = predictor[[1]]
    pred2 = predictor[[2]]
  }

  # create forcing/chilling rate vector at the day level
  Rf = 1/(1+exp(a*(data$Tmini*data$Li-b)))

  # photoperiod-dependent start-date for chilling accumulation (t0)
  # t0 is defined as the first day when photoperiod is shorter than the photoperiod threshold (P_base)
  # after the date of the longest photoperiod (summer solstice), namely, the 173rd day of year
  t0 <- vector()
  for(col in 1:ncol(data$Tmini)) {
    interval = 1:366
    t0A = interval[which(data$Li[,col] < P_base)]
    ind1 = min(which(t0A > 173))
    t0A = t0A[ind1]
    t0 = c(t0,t0A)
  }

  # nullify values before the t0
  for(col in 1:ncol(data$Tmini)){
    Rf[1:t0[col],col] = 0
  }

  if(length(par)==5) {
    # add predictor at the end of the matrix-columns
    Rf = rbind(Rf,predictor)

    # predict date of leaf.off
    doy = apply(Rf,2, function(xt){
      doy = which(cumsum(xt[1:366]) >= c+d*xt[367])[1]
    })
  }
  if(length(par)==6) {
    # add predictors at the end of the matrix-columns
    Rf = rbind(Rf,pred1)
    Rf = rbind(Rf,pred2)

    # predict date of leaf.off
    doy = apply(Rf,2, function(xt){
      doy = which(cumsum(xt[1:366]) >= c+d*xt[367]+e*xt[368])[1]
    })
  }

  return(doy)
}


##========================================
##----------------------------------------
## Calibration per species
detach(package:data.table)
library(phenor)

cat("\n\nSTARTING SPECIES-SPECIFIC CALIBRATION\n")

mySmry_perSpc    <- NULL
myLcDoy_perSpc   <- NULL
myLcDoy_perSpcDT <- NULL

for (myBBCH in unique(pheno.df$BBCH_off)) {
  cat(paste0("\nStarting calibration for ", ifelse(myBBCH == 94, "leaf colouring",
                                                   ifelse(myBBCH == 95, "leaf fall",
                                                          paste0("BBCH ", myBBCH))),
             ".\n(", Sys.time(), ")\n"))
  
  calibSpec_pBBCH <- calibSpec[calibSpec %in% unique(pheno.df[BBCH_off == myBBCH]$Species)]
  
  for(mySpc in calibSpec_pBBCH) {
    cat(paste0("\nStarting calibration for species ", mySpc, ".\n(", Sys.time(),")\n"))
    
    myDat <- myDataList_perSpc[[as.character(myBBCH)]][[mySpc]]
    myPrd <- myPredList_perSpc[[as.character(myBBCH)]][[mySpc]]
    
    myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.obs <- myDat$transition_dates
    
    ## CDD model
    if ("CDD.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        data = myDat,
                                        cost = rmse,
                                        model = "CDD.model",
                                        method = "GenSA",
                                        lower = c(15,-3000),
                                        upper = c(30,0),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           data = myDat,
                           model = "CDD.model")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "CDD.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Tbase_CDD = optimal_pars$par[1],
                                Fcrit_CDD = optimal_pars$par[2]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["CDD.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_CDD <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "CDD",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat("\nCDD.model calibrated\n---")
      cat(paste0("\nCDD.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## DM1 model
    if ("DM1.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        data = myDat,
                                        cost = rmse,
                                        model = "DM1.model",
                                        method = "GenSA",
                                        lower = c(15,11,-2000),
                                        upper = c(30,16,0),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           data = myDat,
                           model = "DM1.model")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "DM1.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Tbase_DM1 = optimal_pars$par[1],
                                Pbase_DM1 = optimal_pars$par[2],
                                Fcrit_DM1 = optimal_pars$par[3]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["DM1.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_DM1 <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "DM1",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nDM1.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## DM2 model
    if ("DM2.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        data = myDat,
                                        cost = rmse,
                                        model = "DM2.model",
                                        method = "GenSA",
                                        lower = c(15,11,-2000),
                                        upper = c(30,16,0),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           data = myDat,
                           model = "DM2.model")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "DM2.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Tbase_DM2 = optimal_pars$par[1],
                                Pbase_DM2 = optimal_pars$par[2],
                                Fcrit_DM2 = optimal_pars$par[3]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["DM2.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_DM2 <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "DM2",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nDM2.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## TPM model
    if ("TPM.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        data = myDat,
                                        cost = rmse,
                                        model = "TPM.model",
                                        method = "GenSA",
                                        lower = c(11,0.02,100,0),
                                        upper = c(16,0.1,250,200),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           data = myDat,
                           model = "TPM.model")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "TPM.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Pbase_TPM = optimal_pars$par[1],
                                a_TPM     = optimal_pars$par[2],
                                b_TPM     = optimal_pars$par[3],
                                Fcrit_TPM = optimal_pars$par[4]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["TPM.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_TPM <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "TPM",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nTPM.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## SIAM model
    if ("SIAM.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        predictor = myPrd$DoY_out,
                                        data = myDat,
                                        cost = rmse,
                                        model = "SecondGen_PIA.models",
                                        method = "GenSA",
                                        lower = c(11,0.02,100,0,0),
                                        upper = c(16,0.1,250,300,1),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           predictor = myPrd$DoY_out,
                           data = myDat,
                           model = "SecondGen_PIA.models")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "SIAM.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Pbase_SIAM = optimal_pars$par[1],
                                a_SIAM     = optimal_pars$par[2],
                                b_SIAM     = optimal_pars$par[3],
                                c_SIAM     = optimal_pars$par[4],
                                d_SIAM     = optimal_pars$par[5]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["SIAM.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_SIAM <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "SIAM",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nSIAM.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## TDM model
    if ("TDM.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        predictor = myPrd$temp_GS,
                                        data = myDat,
                                        cost = rmse,
                                        model = "SecondGen_PIA.models",
                                        method = "GenSA",
                                        lower = c(11,0.02,100,0,0),
                                        upper = c(16,0.1,250,300,1),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           predictor = myPrd$temp_GS,
                           data = myDat,
                           model = "SecondGen_PIA.models")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "TDM.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Pbase_TDM = optimal_pars$par[1],
                                a_TDM     = optimal_pars$par[2],
                                b_TDM     = optimal_pars$par[3],
                                c_TDM     = optimal_pars$par[4],
                                d_TDM     = optimal_pars$par[5]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["TDM.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_TDM <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "TDM",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nTDM.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## TPDM model
    if ("TPDM.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        predictor = list(myPrd$temp_GS, myPrd$RD_summer),
                                        data = myDat,
                                        cost = rmse,
                                        model = "SecondGen_PIA.models",
                                        method = "GenSA",
                                        lower = c(11,0.02,100,0,0,0),
                                        upper = c(16,0.1,250,300,1,1),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           predictor = list(myPrd$temp_GS, myPrd$RD_summer),
                           data = myDat,
                           model = "SecondGen_PIA.models")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "TPDM.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Pbase_TPDM = optimal_pars$par[1],
                                a_TPDM     = optimal_pars$par[2],
                                b_TPDM     = optimal_pars$par[3],
                                c_TDPM     = optimal_pars$par[4],
                                d_TPDM     = optimal_pars$par[5],
                                e_TPDM     = optimal_pars$par[6]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["TPDM.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_TPDM <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "TPDM",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nTPDM.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## PIA_gsi model
    if ("PIAgsi.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        predictor = myPrd$cGSI,
                                        data = myDat,
                                        cost = rmse,
                                        model = "SecondGen_PIA.models",
                                        method = "GenSA",
                                        lower = c(11,0.02,100,0,0),
                                        upper = c(16,0.1,250,300,1),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           predictor = myPrd$cGSI,
                           data = myDat,
                           model = "SecondGen_PIA.models")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "PIAgsi.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Pbase_PIAgsi = optimal_pars$par[1],
                                a_PIAgsi     = optimal_pars$par[2],
                                b_PIAgsi     = optimal_pars$par[3],
                                c_PIAgsi     = optimal_pars$par[4],
                                d_PIAgsi     = optimal_pars$par[5]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["PIAgsi.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_PIAgsi <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "PIAgsi",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nPIAgsi.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## PIA- model
    if ("PIAmns.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        predictor = myPrd$cA_totw,
                                        data = myDat,
                                        cost = rmse,
                                        model = "SecondGen_PIA.models",
                                        method = "GenSA",
                                        lower = c(11,0.02,100,0,0),
                                        upper = c(16,0.1,250,300,1),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           predictor = myPrd$cA_totw,
                           data = myDat,
                           model = "SecondGen_PIA.models")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "PIAmns.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Pbase_PIAmns = optimal_pars$par[1],
                                a_PIAmns     = optimal_pars$par[2],
                                b_PIAmns     = optimal_pars$par[3],
                                c_PIAmns     = optimal_pars$par[4],
                                d_PIAmns     = optimal_pars$par[5]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["PIAmns.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_PIAmns <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "PIAmns",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nPIAmns.model calibrated. (", Sys.time(), ")\n---"))
    }
    
    ## PIA+ model
    if ("PIApls.model" %in% calibModl) {
      optimal_pars <- pr_fit_parameters(par = NULL,
                                        predictor = myPrd$cA_tot,
                                        data = myDat,
                                        cost = rmse,
                                        model = "SecondGen_PIA.models",
                                        method = "GenSA",
                                        lower = c(11,0.02,100,0,0),
                                        upper = c(16,0.1,250,300,1),
                                        control = list(max.call = 40000))
      
      myPred <- pr_predict(par = optimal_pars$par,
                           predictor = myPrd$cA_tot,
                           data = myDat,
                           model = "SecondGen_PIA.models")
      
      mySmry <- list(Spec   = mySpc,
                     #Site   = StID,
                     #GeoLoc = myDat$location[, 1],
                     Model  = "PIApls.model",
                     Method = "GenSA",
                     RMSE   = optimal_pars$value,
                     OptPar = c(Pbase_PIApls = optimal_pars$par[1],
                                a_PIApls     = optimal_pars$par[2],
                                b_PIApls     = optimal_pars$par[3],
                                c_PIApls     = optimal_pars$par[4],
                                d_PIApls     = optimal_pars$par[5]),
                     LU.Mod = myPred)
      
      mySmry_perSpc[[as.character(myBBCH)]][[mySpc]][["PIApls.model"]] <- mySmry
      
      myLcDoy_perSpc[[as.character(myBBCH)]][[mySpc]]$LC.mod_PIApls <- myPred
      
      myLcDoy_perSpcDT <- rbind(myLcDoy_perSpcDT,
                                data.table::data.table(Spec   = mySpc,
                                                       BBCH   = myBBCH,
                                                       Model  = "PIApls",
                                                       CalID  = names(myPred),
                                                       Stat   = substr(myDat$year, 1,
                                                                       unlist(gregexpr("_", myDat$year)) - 1),
                                                       Year   = as.integer(substr(myDat$year,
                                                                                  unlist(gregexpr("_",
                                                                                                  myDat$year)) + 1,
                                                                                  100)),
                                                       DOYobs = myDat$transition_dates,
                                                       DOYmod = myPred))
      
      rm(optimal_pars, myPred, mySmry)
      
      cat(paste0("\nPIApls.model calibrated. (", Sys.time(), ")\n---"))
    }
    cat(paste0("\n\ndone for species ", mySpc, "\n(", Sys.time(),")\n=-=-=\n\n"))
  }
  
  cat(paste0("\n\ndone for ", ifelse(myBBCH == 94, "leaf colouring",
                                     ifelse(myBBCH == 95, "leaf fall",
                                            paste0("BBCH ", myBBCH))), "\n(", Sys.time(),")\n==--==--==\n\n"))
}

save(mySmry_perSpc, file = paste0(Output, "Summary_PerSpc",
                                  args[1], "_", args[2], "_CZ.RData"))
save(myLcDoy_perSpc, file = paste0(Output, "LcDoy_PerSpc",
                                   args[1], "_", args[2], "_CZ.RData"))
data.table::fwrite(myLcDoy_perSpcDT, file = paste0(Output, "LcDoy_PerSpc",
                                                   args[1], "_", args[2], "_CZ.csv"), sep = ";")

cat(paste0("\nSPECIES-SPECIFIC CALIBRATION DONE.\n(", Sys.time(),")\n================================="))

cat("\n=================================\nEND\n=================================")

##----------------------------------------
## References

# Dufr?ne, E. et al. Modelling carbon and water cycles in a beech forest: Part I: Model description and uncertainty analysis on modelled NEE. Ecol. Modell. 185, 407-436 (2005).
# Delpierre, N. et al. Modelling interannual and spatial variability of leaf senescence for three deciduous tree species in France. Agric. For. Meteorol. 149, 938-948 (2009).
# Keenan, T. F. & Richardson, A. D. The timing of autumn senescence is affected by the timing of spring phenology: Implications 434 for predictive models. Glob. Chang. Biol. 21, 2634-2641 (2015).
# Lang, W., Chen, X., Qian, S., Liu, G. & Piao, S. A new process-based model for predicting autumn phenology: How is leaf senescence controlled by photoperiod and temperature coupling? Agric. For. Meteorol. 268, 124-135 (2019).
# Liu, G., Chen, X., Fu, Y. & Delpierre, N. Modelling leaf coloration dates over temperate China by considering effects of leafy season climate. 460 Ecol. Modell. 394, 34-43 (2019).
# Hufkens, K., Basler, D., Milliman, T., Melaas, E. K. & Richardson, A. D. An integrated phenology modelling framework in r. Methods Ecol. Evol. 9, 1276-1285 (2018).
