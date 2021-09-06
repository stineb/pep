# combine model output from MODIS p-model runs

library(tidyverse)

# bail if not on euler
if(!grepl('eu-', Sys.info()['nodename'])){
  stop("You are not on Euler, source data unavailable - abort abort abort!")
}

# list all files
files <- list.files("~/data/pep_pmodel_output/","*.rds", full.names = TRUE)

# combine all files
df <- do.call("rbind",
        lapply(files, 
               function(file){readRDS(file)
                 })
        )

# save data
saveRDS(df, file = "data/pmodel_output/pep_pmodel_output.rds")