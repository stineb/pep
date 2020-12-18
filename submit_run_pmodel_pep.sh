#!/bin/bash

bsub -n 24 -W 72:00 -u bestocke -J run_pmodel_pep -R "span[ptile=24]" "R --vanilla --slave < ~/mct/rscript_run_pmodel_pep.R"
