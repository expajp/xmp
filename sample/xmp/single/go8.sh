#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=small"
#PJM -L "node=8"
#PJM -L "elapse=02:00"
#PJM -j

time mpirun -np 8 ./a.out