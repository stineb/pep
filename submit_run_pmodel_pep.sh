#!/bin/bash

# bsub -n 24 -W 72:00 -u bestocke -J run_pmodel_pep -R "span[ptile=24]" "R --vanilla --slave < ~/pep/rscript_run_pmodel_pep.R > ~/hpc_log/rscript_run_pmodel_pep.Rout"

bsub -n 24 -W 72:00 -u bestocke -J run_pmodel_pep -R "span[mem=10000]" "R --vanilla --slave < ~/pep/rscript_run_pmodel_pep.R > ~/hpc_log/rscript_run_pmodel_pep.Rout"


# njobs=10
# for ((n=1;n<=${njobs};n++)); do
#     echo "Submitting chunk number $n ..."
#     bsub -W 72:00 -u bestocke -J "run_pmodel_pep $n" -R "rusage[mem=2000]" "Rscript --vanilla rscript_run_pmodel_pep.R $n $njobs"   # not all require that much memory
# done