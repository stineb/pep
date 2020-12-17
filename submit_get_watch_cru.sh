#!/bin/bash

bsub -n 1 -W 72:00 -u bestocke -J get_watch_cru -R "rusage[mem=25000]" "Rscript --vanilla rscript_get_watch_cru.R"
