#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=large"
#PJM -L "node=16"
#PJM -L "elapse=30:00"
#PJM -j

for opn in 2 4 8
do
    echo -n $opn
    echo " process(es):"
    time mpirun -np $opn ./miflow
    echo""
done