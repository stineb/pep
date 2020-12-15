#!/bin/bash

bsub -n 1 -W 72:00 -u bestocke -J get_watch_cru -R "span[mem=20000]" "R --vanilla --slave < ~/pep/rscript_get_watch_cru.R > ~/hpc_log/rscript_get_watch_cru.Rout"
