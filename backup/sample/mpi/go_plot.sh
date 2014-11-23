#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=large"
#PJM -L "node=16"
#PJM -L "elapse=10:00"
#PJM -j

for opn in 1 2 4 8 16
do
#    echo -n $opn
    mpirun -np $opn ./a.out
done