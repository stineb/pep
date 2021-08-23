#!/bin/bash

bsub -W 72:00 -u bestocke -J run_pmodel_pep_smalljob -R "rusage[mem=20000]" "R --vanilla --slave < ~/pep/rscript_run_pmodel_pep.R"
