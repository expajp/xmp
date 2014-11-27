#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=small"
#PJM -L "node=8"
#PJM -L "elapse=10:00"
#PJM -m e
#PJM --mail-list "s.ogawara@stu.kobe-u.ac.jp"
#PJM -j

for opn in 1 2 4 8
do
    echo -n $opn
    echo " process(es):"
    mpirun -np $opn ./miflow
    echo""
done