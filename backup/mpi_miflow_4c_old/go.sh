#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=small"
#PJM -L "node=8"
#PJM -L "elapse=10:00"
#PJM -j

for opn in 1 2 4 8
do
    echo -n $opn
    echo " process(es):"
    time mpirun -np $opn ./miflow
    echo""
done