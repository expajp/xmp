#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=medium"
#PJM -L "node=16"
#PJM -L "elapse=10:00"
#PJM -j

time mpirun -np 16 ./miflow
