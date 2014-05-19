#!/bin/bash
#PJM -N "jobname"
#PJM -L "rscgrp=large"
#PJM -L "node=16"
#PJM -L "elapse=02:00"
#PJM -j

mpirun -np 16 ./a.out