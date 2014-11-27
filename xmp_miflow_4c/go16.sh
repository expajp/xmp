#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=medium"
#PJM -L "node=16"
#PJM -L "elapse=10:00"
#PJM -m e
#PJM --mail-list "s.ogawara@stu.kobe-u.ac.jp"
#PJM -j

mpirun -np 16 ./miflow