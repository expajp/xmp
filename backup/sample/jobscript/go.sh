#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=large"
#PJM -L "node=16"
#PJM -L "elapse=02:00"
#PJM -j

for opn in 1 2 4 8 16
do
    echo -n $opn
    echo " process(es):"
    mpirun -np $opn ./a.out
    echo""
done